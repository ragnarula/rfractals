#This is a script to retrieve harmonic rotation information from a financial time series (OHLC) data.
#Created by Raghav Narula 2014, please see readme.MD for more information.

#Reads a file and returns a data frame which gives the mean, median and sd of the fractal ranges
#factored by the month and hour
fracStatsBy <- function(file,interval,bounds){
        library(plyr)
        print("reading")
        data <- readData(file)
        print("finding highs")
        data <- ddply(data, c("year"), isHighFractal,3,.parallel = TRUE)
        print("finding lows")
        data <- ddply(data, c("year"), isLowFractal,3,.parallel = TRUE)
        print("calculating stats")
        result <- ddply(data,bounds,fracStats,bounds,.parallel = TRUE)
        print("converting to numeric")
        for(i in 1:length(bounds)){
                result[,!names(result) %in% bounds] <- sapply(result[,!names(result) %in% bounds],as.numeric)
        }
#         result[,c(-1,-2)] <- sapply(result[,c(-1,-2)],as.numeric)
        result
}



fracStatsByBar <- function(file,interval){
        library(plyr)
        print("reading")
        data <- readData(file)
        print("finding highs")
        data$hifrac <- isHighFractal(data$high,interval)
        print("finding lows")
        data$lofrac <- isLowFractal(data$low,interval)
        print("calculating stats")
        result <- fracStats(data)
        print("converting to numeric")
        result <- sapply(result,as.numeric)
        result
}



#Reads a file and returns a numeric vector of the ranges of the fractals in that file
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
        data$datetime <- as.POSIXct(data$datetime)
        data$hour <- as.factor(format(data$datetime, "%H"))
        data$month <- as.factor(format(data$datetime, "%m"))
        data$year <- as.factor(format(data$datetime, "%Y"))
        data
}

#Returns weather a bars high value is the highest of the previous N and following N bars.
isHighFractal <- function(data, constant){
        series <- data$high
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
                        if(highest > i){
                                i <- highest
                        } else {
                                i <- i + constant + 1
                        }
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
        
        data$hifrac <- result
        data
}

#same as above but returns weather the bar is the lowest of the previous N and following N bars.
isLowFractal <- function(data, constant){
        series <- data$low
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
                        if(lowest > i){
                                i <- lowest
                        } else {
                                i <- i + constant + 1
                        }
                }
        }
        
        for(i in 1:constant-1){
                result[length(series) - i] <- FALSE
        }
        for(i in 1:constant){
                result[i] <- FALSE
        }
        
        data$lofrac <- result
        data
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

#gets the fractal ranges for the data frame, and returns a data frame with the mean, median, and sd of the fratal ranges
fracStats <- function(data,bounds){
#         print("pre loop")
#         print(data$month[1])
#         print(data$hour[1])
        for(i in 1:length(bounds)){
#                 print("in loop")
                data[,bounds[i]] <- cut(data$datetime, breaks = bounds[i])
#                 print("in loop end")
        }   
#         print("post loop")
        ranges <- by(data, data[,bounds] , fracRange)
        ranges <- unlist(ranges, use.names = FALSE)
#         print(ranges)
        mean <- mean(ranges)
#         print(paste("mean",mean))
        median <- median(ranges)
#         print(paste("median",median))
        mode <- names(sort(-table(ranges)))[1]
#         print(paste("mode",mode))
        sd <- sd(ranges)
#         print(paste("sd",sd))
        quants <- quantile(ranges, seq(0,1,0.1), type = 1)
        res <- c(mean,median,mode,sd,quants)
        names(res) <- c("mean","median","mode","sd",names(quants))
        res
}