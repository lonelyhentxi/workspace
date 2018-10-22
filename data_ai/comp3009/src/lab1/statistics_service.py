from typing import Collection, Any, Dict, Tuple
from functools import reduce
from math import sqrt, inf
from shared.type_def import Numeric
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
from typing import Callable


class StatisticsService:

    def sum(self, series: Collection[Numeric]) -> float:
        return reduce(lambda a, x: a + x, series)

    def mean(self, series: Collection[Numeric]) -> float:
        return self.sum(series) / len(series)

    def max(self, series: Collection[Numeric]) -> float:
        return reduce(lambda a, x: max(a, x), series)

    def min(self, series: Collection[Numeric]) -> float:
        return reduce(lambda a, x: min(a, x), series)

    def range(self, series: Collection[Numeric]) -> float:
        range_max, range_min = reduce(lambda a, x: (max(a[0], x), min(a[1], x)), series, (0, inf))
        return range_max - range_min

    def uvar(self, series: Collection[Numeric]) -> float:
        mean = self.mean(series)
        return reduce(lambda a, x: a + pow(x - mean, 2), series, 0) / len(series) - 1

    def var(self, series: Collection[Numeric]) -> float:
        mean = self.mean(series)
        return reduce(lambda a, x: a + pow(x - mean, 2), series, 0) / len(series)

    def ustd(self, series: Collection[Numeric]) -> float:
        return sqrt(self.uvar(series))

    def std(self, series: Collection[Numeric]) -> float:
        return sqrt(self.var(series))

    def unique(self, series: Collection[Any]) -> Collection[Any]:
        return set(series)

    def aggregate(self, series: Collection[Any]) -> Dict[Any, int]:
        unique = self.unique(series)
        agg = dict.fromkeys(unique, 0)
        for i in series:
            agg[i] = agg[i] + 1
        return agg

    def mode(self, series: Collection[Any]) -> Tuple[Any, int]:
        agg = self.aggregate(series)
        if len(agg) == 0:
            raise RuntimeWarning("empty series has no mode")
        return reduce(lambda a, x: x if x[1] > a[1] else a, agg.items(), (None, 0))

    def freq(self, series: Collection[Any]) -> Collection[Tuple[Any, float]]:
        agg = self.aggregate(series)
        density_sum: float = self.sum(agg.values())
        return list(zip(agg.keys(), map(lambda x: x / density_sum, agg.values())))

    def make_factor_indexes(self, series: Collection[Any]) -> Dict[Any, int]:
        unique = self.unique(series)
        ret: Dict[Any, int] = {}
        for index, record in enumerate(unique):
            ret[record] = index
        return ret

    def corrdot_gen(self, corr_method: str) -> Callable:

        def _corrdot(*args, **kwargs):
            df: pd.DataFrame = args[0]
            corr_r = df.corr(args[1], corr_method)
            corr_text = f"{corr_r:2.2f}".replace("0.", ".")
            ax = plt.gca()
            ax.set_axis_off()
            marker_size = abs(corr_r) * 10000
            ax.scatter(.5, .5, marker_size, corr_r, alpha=0.6, cmap="coolwarm",
                       vmin=-1, vmax=1, transform=ax.transAxes)
            font_size = abs(corr_r) * 40 + 5
            ax.annotate(corr_text, [.5, .5, ], xycoords="axes fraction",
                        ha='center', va='center', fontsize=font_size)

        return _corrdot

    def draw_corr(self, df: pd.DataFrame, corr_method: str) -> sns.PairGrid:
        g = sns.PairGrid(df, aspect=1.4, diag_sharey=False)
        g.map_lower(sns.regplot, order=3, ci=False, line_kws={'color': 'black'})
        g.map_diag(sns.distplot, kde_kws={'color': 'black'})
        corrdot = self.corrdot_gen(corr_method)
        return g.map_upper(corrdot)
