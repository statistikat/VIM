"""Optional exact-equivalence checks in the foundation Python environment.

Run directly with .local/foundation-env/bin/python; no weights or network needed.
"""
import torch
from tabimpute.prior.splits import create_train_test_sets
from uci_foundation_runtime import vectorized_train_test_sets, VectorizedTabImpute


def test_tensors():
    devices = ["cpu"] + (["mps"] if torch.backends.mps.is_available() else [])
    for device in devices:
        for dtype in (torch.float32, torch.bfloat16):
            for missing in ("none", "some", "all"):
                x = torch.arange(24, device=device, dtype=dtype).reshape(6, 4)
                truth = x.clone()
                if missing == "some":
                    x[1, 2] = x[4, 0] = float("nan")
                elif missing == "all":
                    x[:] = float("nan")
                for old, new in zip(create_train_test_sets(x, truth),
                                    vectorized_train_test_sets(x, truth)):
                    torch.testing.assert_close(old, new, rtol=0, atol=0, equal_nan=True)
                    assert old.device == new.device


def test_restore():
    import tabimpute.interface as interface
    original = interface.create_train_test_sets

    class Imputer:
        device = "cpu"

        def _get_imputation_chunk(self):
            return "chunk"

    original_chunk = Imputer._get_imputation_chunk

    class FailingModel:
        imputer = Imputer()

        def impute(self):
            assert interface.create_train_test_sets is vectorized_train_test_sets
            assert Imputer._get_imputation_chunk is not original_chunk
            assert self.imputer._get_imputation_chunk() == "chunk"
            raise ValueError("expected failure")

    try:
        VectorizedTabImpute(FailingModel()).impute()
    except ValueError as error:
        assert str(error) == "expected failure"
    else:
        raise AssertionError("Expected failure was swallowed")
    assert interface.create_train_test_sets is original
    assert Imputer._get_imputation_chunk is original_chunk


if __name__ == "__main__":
    test_tensors()
    test_restore()
    print("Exact tensor equivalence and restoration checks passed.")
