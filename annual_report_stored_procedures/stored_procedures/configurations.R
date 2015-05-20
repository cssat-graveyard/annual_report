
############
# Packages #
############

library(RODBC)
library(Hmisc)
library(magrittr)
library(pocr)
library(mstate)
library(ggplot2)
library(xlsx)
library(stringr)
library(reshape2)
library(dplyr)

###############
# Connections #
###############

con_test_annie <- odbcConnect("test_annie")
con_poc <- odbcConnect("POC")

################
# Dates for TS #
################

start_d <- '2009-01-01'
end_d <- '2014-08-01'