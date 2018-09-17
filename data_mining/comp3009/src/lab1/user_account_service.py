from injector import inject
from shared.path_service import PathService
from shared.log_service import LogService
import pandas
from pandas import DataFrame
import numpy
import matplotlib.pyplot
from os import path


class UserAccountService:
    @inject
    def __init__(self, path_service: PathService, log_service: LogService):
        self.path_service: PathService = path_service
        self.log_render = lambda msg: '[UserAccountService]: {0}'.format(msg)
        self.logger = log_service.logger

    def from_csv(self, sub_path: str)->DataFrame:
        csv_path = path.join(
            self.path_service.get_resource("lab1"),
            sub_path)
        raw_df: DataFrame = pandas.read_csv(csv_path, sep=";", encoding="utf-8")
        self.logger.info(self.log_render("read csv file from {0}".format(csv_path)))
        return raw_df
