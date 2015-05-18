
############
# Packages #
############

library(RODBC)
library(magrittr)
library(pocr)
library(mstate)
library(ggplot2)
library(reshape2)
library(dplyr)

################################
# Function for getting ts data #
################################

ts_func <- function(dat, ..., x, freq = 12, max.rate = 1000, s.window = "per") {
    
    #if(is(dat, "grouped_df")) {
    #	dat <- ungroup(dat)
    #}
    if(any(is.infinite(dat[[x]])) == TRUE) {
        dat[[x]][is.infinite(dat[[x]])] <- 0
    } 
    if(any(dat[[x]] > max.rate)) {
        dat[[x]] <- pmin(dat[[x]], max.rate)	
    } 
    
    dat <- subset(dat, ...)
    
    if((sum(is.na(dat[[x]])) > (1/3 * length(dat[[x]]))) == FALSE) {
        dat[[x]][is.na(dat[[x]])] <- 0
    } 
    if(any(anyNA(dat))) {
        # stop("Too many missing values.")
        return(dat)
    }
    if(length(dat[[x]]) < freq * 2){
        # stop("Not enough data points to deseasonalize.")
        return(dat)
    } 
    
    filt_ts <- ts(dat[[x]], frequency = freq)
    filt_stl <- stl(filt_ts, s.window)$time.series
    filt_df <- as.data.frame(filt_stl[,c("seasonal", "trend")])
    return(cbind(dat, filt_df))
    
}

###############################################
# Function for getting upper and lower limits #
###############################################

lims <- function(dat, ..., x1, x2 = 0.997) {
    
    if(all(is.na(dat[[x1]]))) {
        return(dat)
    }
    
    dat <- subset(dat, ...)
    
    n <- length(dat[[x1]])
    mean_rate <- mean(dat[[x1]][1:n-1])
    dat$variance <- abs(dat[[x1]] - mean_rate)
    mean_variance <- mean(dat$variance)
    dat$lcl <- mean_rate - (mean_variance * qnorm(x2))
    dat$ucl <- mean_rate + (mean_variance * qnorm(x2))
    
    if(sum(dat$lcl) < 0) {
        dat$lcl <- pmax(dat$lcl, 0)
    }
    
    return(dat)
    
}

###########################################################
# Function for converting classes before loading data SQL #
###########################################################

class_convert <- function(x) {
    if (any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
        x = as.character(x)
    }
    if (is.integer(x)) {
        return(x)
    }
    if (is.factor(x)) {
        x = as.character(x)
    }
    if (is.character(x)) {
        y = suppressWarnings(as.numeric(x))
        if (all(! is.na(y))) { 
            x = y
        } else {
            return(x)
        }
    }
    if (all (abs((x - as.integer(x)) < .Machine$double.eps * 2))) {
        return(as.integer(x))
    }
    return(x)
}

###############
# Connections #
###############

con_test_annie <- odbcConnect("test_annie")
con_poc2 <- odbcConnect("POC")

################
# Dates for TS #
################

start_d <- '2009-01-01'
end_d <- '2014-08-01'
