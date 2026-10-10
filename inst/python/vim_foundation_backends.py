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
