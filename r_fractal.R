#This is a script to retrieve harmonic rotation information from a financial time series (OHLC) data.
#Created by Raghav Narula 2014, please see readme.MD for more information.


#Entry point in to script, choose file and fractal interval.
#Returns a numeric vector of the ranges between highs and lows.
#Should be all positive (absolute) values.
getFractalRanges <- function(file, interval){
        print("reading")
        data <- readData(file)
        print("finding highs")
        data$hifrac <- isHighFractal(data$high,interval)
        print("finding lows")
        data$lofrac <- isLowFractal(data$low,interval)
        print("getting ranges")
        res <- fracRange(data)
        res
}

#Reads the data and parses date time column in to R datetime object.
#Creates two factor variables, day and hour for further work.
readData <- function(file){
        data <- read.csv(file, header = FALSE)
        colnames(data) <- c("symbol","datetime","open","high","low","close","volume")
        data$datetime <- strptime(data$datetime, format = "%m/%d/%Y %H:%M")
        data$hour <- as.factor(format(data$datetime, "%H"))
        data$month <- as.factor(format(data$datetime, "%m"))
        data
}

#Returns weather a bars high value is the highest of the previous N and following N bars.
isHighFractal <- function(series, constant){
        result <- logical()
        #check from N onwards to length - N (prevent overflow and underflow, make sure data required for result is available)
        for(i in (constant + 1):(length(series)-constant)){
                #assume this bar is the highest
                highest <- i-constant;
                #try and find a higher one in the current range
                for(j in ((i-constant):(i+constant))){
                        if(series[j] > series[highest]){
                                highest <- j
                        }
                }
                #set true only is the current bar is the highest in the range
                if(series[i] == series[highest]){
                      result[i] <- TRUE  
                } else {
                        result[i] <- FALSE
                }
        }
        #set first N bars to FALSE (not enough data)
        for(i in 1:constant-1){
                result[length(series) - i] <- FALSE
        }
        #set last N bars to FALSE (not enough data)
        for(i in 1:constant){
                result[i] <- FALSE
        }
        
        result
}

#same as above but returns weather the bar is the lowest of the previous N and following N bars.
isLowFractal <- function(series, constant){
        result <- logical()
        for(i in (constant + 1):(length(series)-constant)){
                lowest <- i-constant;
                for(j in ((i-constant):(i+constant))){
                        if(series[j] < series[lowest]){
                                lowest <- j
                        }
                }
                if(series[i] == series[lowest]){
                        result[i] <- TRUE  
                } else {
                        result[i] <- FALSE
                }
        }
        
        for(i in 1:constant-1){
                result[length(series) - i] <- FALSE
        }
        for(i in 1:constant){
                result[i] <- FALSE
        }
        
        result
}

#wrapper for the function below, to make compatible with 'by' function
fracRange <- function(data){
        fracRange2(data$high,data$low,data$hifrac,data$lofrac)
}
#returns a vector of the absolute difference between consecutive low and high fractals.
fracRange2 <- function(high, low, hifrac, lofrac){
        result <- numeric()
        for(i in 1:length(high)){
                if(!hifrac[i] && !lofrac[i]){
                        next
                }
                if(hifrac[i]){
#                         print(paste("high", i,high[i]))
                        hi <- i
                        lowest <- NA
                        #search for the lowest low fractal until another high fractal is found. May be in same bar.
                        for(j in hi:length(high)){
                                if(j > length(high)){
                                        break
                                }
                                if(lofrac[j]){
                                        if(is.na(lowest)){
                                                lowest <- j
                                        } else if (low[j] < low[lowest]){
                                                lowest <- j
                                        }
                                        
                                }
                                #break if new hi fractal is found.
                                if(j != hi && hifrac[j]){
                                        break
                                }
                        }
                        if(!is.na(lowest)){
                                range <- high[hi] - low[lowest]
                                #add result only if the range is greater than 0
                                #sometimes slow markets produce backwards fractals
                                if(range > 0){
                                        result <- c(result, range)
                                }
                        }
                }
                if(lofrac[i]){
#                         print(paste("low", i, low[i]))
                        lo <- i
                        highest <- NA
                        #search for the highest high fractal until another low fractal is found. May be in same bar.
                        for(j in lo:length(high)){
                                if(j > length(high)){
                                        break
                                }
                                if(hifrac[j]){
                                        if(is.na(highest)){
                                                highest <- j
                                        } else if (high[j] > high[highest]){
                                                highest <- j
                                        }
                                        
                                }
                                #break if new low fractal is found
                                if(j != lo && lofrac[j]){
                                        break
                                }
                        }
                        if(!is.na(highest)){
                                range <- high[highest] - low[lo]
                                #add result only if the range is greater than 0
                                #sometimes slow markets produce backwards fractals
                                if(range > 0){
                                        result <- c(result, range)
                                }
                        }
                }
        }
        result
}