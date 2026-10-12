"""Opt-in checkpoint setup after installing foundation-requirements.txt.

Run with the foundation environment's Python:
  python inst/benchmarks/setup-foundation.py --root .local --download
Without --download, only already-cached files are used. No credentials or
license acceptance are automated. The selected public model revisions are pinned.
"""
import argparse
import csv
import hashlib
import os
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(".local"))
    parser.add_argument("--download", action="store_true")
    args = parser.parse_args()
    root = args.root.resolve()
    os.environ["HF_HOME"] = str(root / "huggingface")
    from huggingface_hub import hf_hub_download

    models = [
        ("tabimpute", "Tabimpute/TabImpute",
         "96d7a0f160184cfb361a03559430baf1cf31aec5", "tabimpute_001.ckpt", False),
        ("tabicl_classifier", "jingang/TabICL",
         "4dcd344ece2c00be9e831fdd35bed57b5ad83e19", "tabicl-classifier-v2-20260212.ckpt", False),
        ("tabicl_regressor", "jingang/TabICL",
         "4dcd344ece2c00be9e831fdd35bed57b5ad83e19", "tabicl-regressor-v2-20260212.ckpt", False),
        ("tabpfn_classifier", "Prior-Labs/TabPFN-v2-clf",
         "f851f2a3c941544733b712d8c0f96dfae9b28862", "tabpfn-v2-classifier.ckpt", True),
        ("tabpfn_regressor", "Prior-Labs/TabPFN-v2-reg",
         "4972a65a1b30806315c6f92499959ffbfc69a673", "tabpfn-v2-regressor.ckpt", True),
    ]
    expected_sha256 = {
        "tabimpute": "b8bacc5f1b8274d2b5f98ef1e2e2ee6771a018dbb72849d9b3a4abaa807a9bb1",
        "tabicl_classifier": "bdc7dbd5e4ff21f8f0456fcf90c6b7cdf72dbea960f2d05b19bec19f9b3d4ed0",
        "tabicl_regressor": "0db9cb538f114e79026bf08f45f41ad8dd7ad2de2aaca9a5ca8cd3bd9748ae7a",
        "tabpfn_classifier": "f65a35685aeef42e31b796d9bfa34e68d6fc780bc98e7bff7763802964cf435f",
        "tabpfn_regressor": "2ab5a07d5c41dfe6db9aa7ae106fc6de898326c2765be66505a07e2868c10736",
    }
    records = []
    for name, repo, revision, filename, explicit_path in models:
        path = Path(hf_hub_download(
            repo_id=repo, filename=filename, revision=revision,
            local_dir=root / "models" if explicit_path else None,
            local_files_only=not args.download))
        # TabICL's offline adapter resolves its fixed filenames via the local
        # main ref. Pin that ref in this isolated cache, never the global cache.
        if repo == "jingang/TabICL":
            ref = root / "huggingface/hub/models--jingang--TabICL/refs/main"
            ref.parent.mkdir(parents=True, exist_ok=True)
            ref.write_text(revision)
        digest = hashlib.sha256()
        with path.open("rb") as stream:
            for block in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(block)
        if digest.hexdigest() != expected_sha256[name]:
            raise ValueError(f"Checkpoint checksum mismatch: {name}")
        records.append(dict(name=name, repo=repo, revision=revision,
                            filename=filename, sha256=digest.hexdigest(),
                            path=str(path)))
    with (root / "foundation-models.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=records[0].keys())
        writer.writeheader()
        writer.writerows(records)
    print("Verified five local checkpoints; manifest:", root / "foundation-models.csv")


if __name__ == "__main__":
    main()
