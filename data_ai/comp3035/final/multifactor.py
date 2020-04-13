# only used in ricequant.com

import math
import numpy as np
import pandas as pd
import talib


def init(context):
    context.benchmark='中信证券'
    context.s1 = "中信证券"
    context.days = 250
    context.period = 3
    update_universe(context.s1)
    scheduler.run_weekly(get_historyret,weekday=1)
    scheduler.run_weekly(trading,weekday=1)


def get_historyret(context,bar_dict):
    prices = history(context.days*context.period,'1d','close')[context.s1].values
    context.max_price = np.max(prices[-250:-1])
    context.min_price = np.min(prices[-250:-1])
    context.current_price = prices[-1]
	

def trading(context, bar_dict):  
    b1 = context.max_price/bar_dict[context.s1].close - 1
    c1 = 1 - context.min_price/bar_dict[context.s1].close
    kelly = (0.5*b1-0.5*c1)/(b1*c1)
    if kelly > 1:
        kelly = 1
    if kelly < 0:
        kelly = 0
    if bar_dict[context.s1].close > context.max_price:
        kelly = 1
    if bar_dict[context.s1].close < context.min_price:
        kelly = 0
    if bar_dict[context.s1].is_trading:
        order_target_percent(context.s1,kelly*0.98)