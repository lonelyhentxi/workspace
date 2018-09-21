from injector import inject
from shared.path_service import PathService
from shared.log_service import LogService
import pandas
from pandas import DataFrame
import numpy
import matplotlib.pyplot
from os import path
from lab1.statistics_service import StatisticsService
from typing import Collection, Any
from shared.type_def import Numeric


class UserAccountService:
    @inject
    def __init__(self, path_service: PathService, log_service: LogService, statistics_service: StatisticsService):
        self.path_service: PathService = path_service
        self.log_render = lambda msg: '[UserAccountService]: {0}'.format(msg)
        self.logger = log_service.logger
        self.statistics_service: StatisticsService = statistics_service

    def from_csv(self, sub_path: str) -> DataFrame:
        csv_path = path.join(
            self.path_service.get_resource("lab1"),
            sub_path)
        raw_df: DataFrame = pandas.read_csv(csv_path, sep=";", encoding="utf-8")
        self.logger.info(self.log_render("read csv file from {0}".format(csv_path)))
        return raw_df

    def numeric_describe(self, series: Collection[Numeric], series_name: str):
        methods = ['mean', 'max', 'min', 'range', 'std', 'ustd', 'var', 'uvar']
        self.logger.info(self.log_render(
            'numeric series \'{}\' description'.format(series_name)
        ))
        for m_name in methods:
            self.logger.info(self.log_render(
                '{} - {}'.format(m_name, getattr(self.statistics_service, m_name)(series))
            ))

    def factor_describe(self, series: Collection[Any], series_name):
        self.logger.info(self.log_render(
            'factor series \'{}\' description'.format(series_name)
        ))
        mode = self.statistics_service.mode(series)
        self.logger.info(self.log_render(
            'mode - {} {}'.format(str(mode[0]), mode[1])
        ))
        for k, v in self.statistics_service.freq(series):
            self.logger.info(self.log_render(
                'freq - {} {}'.format(str(k), v)
            ))
