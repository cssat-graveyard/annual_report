library(ggplot2)
library(plotflow)
library(pocr)
#setwd("S:/Data Portal/annual_report")
load("graph_dat.RData")

measures <- list()

sparkdata <- list(gen.rate.referrals = sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",
                                                              c("date", "referral.rate")],
                  gen.rate.screen = sp_rate_referral_scrn_in_clean[sp_rate_referral_scrn_in_clean$type=="Deseasonalized Trend",
                                                                  c("date", "referral.rate")],
                  gen.rate.place = sp_rate_placement_clean[sp_rate_placement_clean$type=="Deseasonalized Trend",
                                                           c("cohort.date", "placement.rate")]
                  
                  
                  
                )

sp_rate_placement_clean[sp_rate_placement_clean$type=="Deseasonalized Trend",] %>%
    head()




# Columns for table: 
# Section | Table | Measure | Units | start_val | end_val | change_cat | assessment

# Outline in google spreadsheet

# SAFETY ###

## General Safety 

### Referrals

gen.ref <- sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",]


pdfs <- list.files(pattern = "\\.pdf")
lapply(pdfs, embedFonts)
