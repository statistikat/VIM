"""Optional foundation-model adapters. No model imports at module load."""

def tabicl_impute(x, categorical, n_estimators=4, n_iterations=2,
                  temperature=1e-8, seed=42, device='cpu', allow_download=False):
    import numpy as np
    from tabicl import TabICLUnsupervised
    x = np.array(x, dtype=np.float32, copy=True)
    x[np.isnan(x)] = np.nan
    model = TabICLUnsupervised(n_estimators=int(n_estimators),
        categorical_features=list(categorical), random_state=seed, device=device,
        estimator_params={'allow_auto_download': bool(allow_download), 'n_jobs': 1,
                          'kv_cache': False})
    model.fit(x)  # shared weights loaded once across all target columns
    return model.impute(x, temperature=float(temperature), n_iterations=int(n_iterations))


def tabpfn_impute(x, categorical, n_estimators=4, n_permutations=1,
                  temperature=1e-9, seed=42, device='cpu', allow_download=False,
                  classifier_path=None, regressor_path=None, model_version='v2'):
    import numpy as np
    import torch
    from pathlib import Path
    from tabpfn import TabPFNClassifier, TabPFNRegressor
    from tabpfn.constants import ModelVersion
    from tabpfn_extensions.unsupervised import TabPFNUnsupervisedModel
    x = np.array(x, dtype=np.float64, copy=True)
    x[np.isnan(x)] = np.nan
    categorical = list(categorical)
    if seed is not None:
        torch.manual_seed(int(seed))
    def estimator(cls, path):
        options = dict(n_estimators=int(n_estimators), device=device,
                       random_state=seed, n_preprocessing_jobs=1)
        if path is not None:
            if not Path(path).is_file():
                raise ValueError('Checkpoint path does not exist')
            return cls(model_path=path, **options)
        if not allow_download:
            raise ValueError('Offline TabPFN requires a local checkpoint for each used variable type')
        return cls.create_default_for_version(ModelVersion(model_version), **options)
    clf = estimator(TabPFNClassifier, classifier_path) if categorical else None
    reg = estimator(TabPFNRegressor, regressor_path) if len(categorical) < x.shape[1] else None
    model = TabPFNUnsupervisedModel(tabpfn_clf=clf, tabpfn_reg=reg)
    model.set_categorical_features(categorical)
    model.fit(x)
    # Override the extension's cardinality heuristic with the declared schema.
    model.set_categorical_features(categorical)
    result = model.impute(x, t=float(temperature), n_permutations=int(n_permutations))
    return result.detach().cpu().numpy() if torch.is_tensor(result) else np.asarray(result)
