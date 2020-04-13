import scipy.sparse as sp
import numpy as np
from sklearn.preprocessing import normalize


class TfidfTransformer:
    def __init__(self, norm="l2", smooth_idf=True):
        self.smooth_idf = smooth_idf
        self.norm = norm

    def fit(self, X):
        if not sp.issparse(X):
            X = sp.csr_matrix(X)
        dtype = X.dtype if X.dtype in (
            np.float64, np.float32, np.float16) else np.float64
        n_samples, n_features = X.shape
        df = np.bincount(X.indices, minlength=n_features).astype(dtype)
        df += int(self.smooth_idf)
        n_samples += int(self.smooth_idf)
        # avoid zero
        idf = np.log(n_samples/df) + 1
        self._idf_diag = sp.diags(idf, offsets=0, shape=(
            n_features, n_features), format='csr', dtype=dtype)
        return self

    def transform(self, X, copy=True):
        if not sp.issparse(X):
            X = sp.csr_matrix(X, dtype=np.float64)
        n_samples, n_features = X.shape
        X = X * self._idf_diag
        if self.norm:
            X = normalize(X, norm=self.norm, copy=False)
        return X

    @property
    def idf_(self):
        return np.ravel(self._idf_diag.sum(axis=0))

    @idf_.setter
    def idf_(self, value):
        value = np.asarray(value, dtype=np.float64)
        n_features = value.shape[0]
        self._idf_diag = sp.spdiags(value, diags=0, m=n_features,
                                    n=n_features, format='csr')
