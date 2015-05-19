
source("functions.R")
source("configurations.R")

###################################################
# Rate of Children in Out-of-Home Care On-the-Run #
###################################################

rate_care_day_otr <- sqlQuery(con_poc, "SELECT 
                              OTR.fiscal_yr
                              ,C.old_region_cd
                              ,cd_cnty
                              ,OTR.age_in_fy
                              ,OTR.care_days_otr
                              ,OTR.care_days
                              ,OTR.care_days_otr*1000.0/care_days otr_rate
                              FROM ca_ods.prtl.placement_care_days_otr AS OTR
                              JOIN [dbo].[ref_lookup_county] AS C
                              ON OTR.cd_cnty = C.county_cd
                              WHERE OTR.excludes_7day = 1
                              AND OTR.excludes_nondcfs = 1
                              AND OTR.excludes_trh = 1
                              AND OTR.age_in_fy >= 13
                              ORDER BY 
                              OTR.fiscal_yr
                              ,OTR.age_in_fy
                              ,old_region_cd")			  

##############################################
# aggregating the data from county to region #
##############################################

otr_agg <- rate_care_day_otr %>% group_by(fiscal_yr, old_region_cd, age_in_fy) %>%
    summarize(care_days_otr = sum(care_days_otr),
              care_days = sum(care_days),
              otr_rate = sum(care_days_otr)/sum(care_days))

as.data.frame(otr_agg)


lims(otr_agg, age_in_fy == 13 & old_region_cd == 0, x1 = 6)

dat2 <- as.data.frame(NULL)

for(h in 0:6){	
    dat1 <- as.data.frame(NULL)
    for (i in 13:17) {
        dat <- lims(otr_agg, age_in_fy == i & old_region_cd == h, x1 = 6)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind(dat2, dat1)					
}	

dat2$cd_otr_age <- ifelse(dat2$age_in_fy == 13, 4,
                          ifelse(dat2$age_in_fy == 14, 3,
                                 ifelse(dat2$age_in_fy == 15, 2,
                                        ifelse(dat2$age_in_fy == 16, 1, 0))))	
for(i in 1:ncol(dat2)){
    dat2[[i]] <- class_convert(dat2[[i]])
}	

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.placement_care_days_otr_limits")
sqlSave(con_test_annie, dat = dat2, tablename = "test_annie.placement_care_days_otr_limits", rownames = FALSE)




