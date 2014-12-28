# Experiments with Harmonic Rotation of a financial time series (OHLC) data

Please note this is a work in progress and needs to be verified as corrected. I do not take any responsibility for decisions you make based on values returned by these functions.

### What's working?
The included script (r_fractal.R) can currently take OHLC data and return a set of statistical measures on the ranges between fractal highs and lows.

Fractals for this purpose are defined as bars which are either the highest of some N bars before AND after, or visa vera the lowest of some N bars before AND after. For more info see: http://trading-stocks.netfirms.com/fractals.htm

##### Using the script
There are two functions that provide an entry point in to the script.

`fracStatsBy(filename, N, factors)` can be used to return range stats subsetted by month, year, hour or any combination of the three.
E.g. `result <- fracStatsBy(filename, N, c("month","hour"))` would return stats factored by hour and month, for the data in `filename`, for the highest high of N bars and lowest low of N bars.

`fracStats(filename, N)` is a simpler version that returns stats without subsetting.

Files are expeted to be in CSV format with 7 columns, ```symbol, datetime, open, high, low, close, volume```, with NO HEADER ROW.

datetime is expected to be in the format ```month/day/year hour:minute:second``` e.g. ```02/02/2013 12:12:12```

##### Examples
A valid command, using the test data for one trading day would be:
``` result <- fracStatsBy("test_data/es_one_day.csv",3, c("month"))```

In this case:

1. The file ```es_one_day.csv``` is used in the sub directory ```test_data```. 
2. Bars which are the highest high out of the previous 3 and following 3 bars are found.
3. Bars which are the lowest low of the previous 3 and following 3 bars are found.
4. The returned data is factored by month.

The first 4 columns of the returned data are:

1. Symbol
2. Mean - mean value of high-low ranges.
3. Median - median value of high-low ranges.
4. Mode - the most frequent high-low range.

The next 10 columns hold the values returned from the quantile function, splits in to 10 groups.

###### Example Output
Example output from `fracStatsBy("hourly_eurusd.csv",3,c("month"))`

           month        mean  median   mode          sd     0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100%
    1     01 0.006780737 0.00530 0.0040 0.005108420 0.0007 0.0023 0.0029 0.0037 0.0045 0.0053 0.0063 0.0077 0.0098 0.0129 0.0464
    2     02 0.006219026 0.00500 0.0022 0.004701222 0.0004 0.0019 0.0025 0.0033 0.0040 0.0050 0.0059 0.0071 0.0091 0.0122 0.0329
    3     03 0.006359435 0.00510 0.0040 0.004697771 0.0008 0.0021 0.0028 0.0035 0.0042 0.0051 0.0061 0.0074 0.0092 0.0120 0.0546
    4     04 0.005967117 0.00490 0.0022 0.004250901 0.0005 0.0019 0.0027 0.0034 0.0041 0.0049 0.0057 0.0070 0.0085 0.0116 0.0325
    5     05 0.006319171 0.00480 0.0020 0.005113476 0.0003 0.0018 0.0026 0.0032 0.0039 0.0048 0.0060 0.0074 0.0093 0.0127 0.0389
    6     06 0.006492882 0.00520 0.0041 0.004776303 0.0004 0.0022 0.0029 0.0037 0.0044 0.0052 0.0060 0.0073 0.0090 0.0131 0.0340
    7     07 0.005829068 0.00465 0.0039 0.004301772 0.0002 0.0019 0.0027 0.0033 0.0039 0.0046 0.0055 0.0067 0.0082 0.0112 0.0396
    8     08 0.005897306 0.00470 0.0023 0.004441282 0.0001 0.0019 0.0025 0.0032 0.0039 0.0047 0.0056 0.0069 0.0084 0.0111 0.0396
    9     09 0.006420487 0.00480 0.0021 0.005363405 0.0003 0.0019 0.0026 0.0033 0.0041 0.0048 0.0057 0.0072 0.0091 0.0128 0.0435
    10    10 0.006779097 0.00490 0.0034 0.005972785 0.0002 0.0020 0.0027 0.0034 0.0041 0.0049 0.0059 0.0076 0.0099 0.0136 0.0508   
    11    11 0.006688399 0.00520 0.0037 0.005464734 0.0005 0.0021 0.0028 0.0034 0.0042 0.0052 0.0060 0.0075 0.0098 0.0132 0.0501
    12    12 0.006533015 0.00510 0.0021 0.005676791 0.0008 0.0020 0.0027 0.0034 0.0042 0.0051 0.0060 0.0073 0.0090 0.0124 0.0560
