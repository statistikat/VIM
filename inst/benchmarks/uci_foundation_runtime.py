"""Benchmark-only TabImpute input assembly; model and imputation math unchanged."""


def vectorized_train_test_sets(x, x_full):
    import torch

    def assemble(indices):
        if not len(indices):
            # Match upstream empty-output dtype/device exactly.
            return torch.empty(0, x.shape[0] + x.shape[1] + 2), torch.empty(0)
        rows, cols = indices[:, 0], indices[:, 1]
        features = torch.cat((indices.to(dtype=x.dtype), x[rows], x[:, cols].T), dim=1)
        return features, x_full[rows, cols]

    missing = torch.isnan(x)
    train_x, train_y = assemble(torch.nonzero(~missing))
    test_x, test_y = assemble(torch.nonzero(missing))
    return train_x, train_y, test_x, test_y


class VectorizedTabImpute:
    """Scope the equivalent tensor builder to one impute call; restore upstream."""

    def __init__(self, model):
        self.model = model

    def impute(self, *args, **kwargs):
        import tabimpute.interface as interface

        import torch

        original = interface.create_train_test_sets
        imputer_type = type(self.model.imputer)
        original_chunk = imputer_type._get_imputation_chunk

        def bounded_chunk(imputer, *chunk_args, **chunk_kwargs):
            result = original_chunk(imputer, *chunk_args, **chunk_kwargs)
            if str(imputer.device).startswith("mps"):
                torch.mps.synchronize()
                torch.mps.empty_cache()
            return result

        interface.create_train_test_sets = vectorized_train_test_sets
        imputer_type._get_imputation_chunk = bounded_chunk
        try:
            return self.model.impute(*args, **kwargs)
        finally:
            interface.create_train_test_sets = original
            imputer_type._get_imputation_chunk = original_chunk
