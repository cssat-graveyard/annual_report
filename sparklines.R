library(ggplot2)
library(plotflow)
library(pocr)
#setwd("S:/Data Portal/annual_report")
load("graph_dat.RData")

measures <- list()

sections <- c("Safety", "Permanency", "Well-Being")

# Columns for table: 
# Section | Table | Measure | Units | start_val | end_val | change_cat | assessment

# Outline in google spreadsheet

# SAFETY ###

## General Safety 

### Referrals

gen.ref <- sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",]


pdfs <- list.files(pattern = "\\.pdf")
lapply(pdfs, embedFonts)
