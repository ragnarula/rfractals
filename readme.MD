# Experiments with Harmonic Rotation of a financial time series (OHLC) data

Please note this is a work in progress and needs to be verified as corrected. I do not take any responsibility for decisions you make based on values returned by these functions.

### What's working?
The included script (r_fractal.R) can currently take OHLC data and return the ranges between high and low fractals.

Fractals for this purpose are defined as bars which are either the highest of some N bars before AND after, or visa vera the lowest of some N bars before AND after. For more info see: http://trading-stocks.netfirms.com/fractals.htm

### TODO
Would like to be implement functions that return ranges factored by month and hour of day.

##### Using the script
A single command ```result <- getFractalRanges(filename, N)``` will return a numerical vector of the absolute ranges between high and low fractals of the series in the given file name. 

Files are expeted to be in CSV format with 7 columns, ```symbol, datetime, open, high, low, close, volume```, with NO HEADER ROW.

datetime is expected to be in the format ```month/day/year hour:minute:second``` e.g. ```02/02/2013 12:12:12```

##### Examples
A valid command, using the test data for one trading day would be:
``` result <- getFractalRanges("test_data/es_one_day.csv",3)```

In this case:

1. The file ```es_one_day.csv``` is used in the sub directory ```test_data```. 
2. Bars which are the highest high out of the previous 3 and following 3 bars are found.
3. Bars which are the lowest low of the previous 3 and following 3 bars are found.
4. The ranges between the high and low fractals are returned as a numeric vector.

The data can then be plotted using the command: ```plot(table(result))```
