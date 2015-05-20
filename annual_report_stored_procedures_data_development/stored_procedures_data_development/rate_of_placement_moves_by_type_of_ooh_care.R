
source("functions.R")
source("configurations.R")

#######################################################
# Rate of Placement Moves by Type of Out-of-Home Care #
#######################################################

rate_care_day_movement_tx <- sqlQuery(con_poc,"SELECT    
                                      MOB.fiscal_yr
                                      ,MOB.county_cd
                                      ,C.old_region_cd
                                      ,MOB.years_in_care
                                      ,MOB.kin_cnt
                                      ,MOB.foster_cnt
                                      ,MOB.group_cnt
                                      ,MOB.care_days
                                      ,MOB.kin_cnt*100000.0/IIF(MOB.care_days = 0, NULL, MOB.care_days) AS movement_rate_to_kin
                                      ,MOB.foster_cnt*100000.0/IIF(MOB.care_days = 0, NULL, MOB.care_days) AS movement_rate_to_foster
                                      ,MOB.group_cnt*100000.0/IIF(MOB.care_days = 0, NULL, MOB.care_days) AS movement_rate_to_group 
                                      FROM prtl.placement_care_days_mobility AS MOB
                                      JOIN [dbo].[ref_lookup_county] AS C
                                      ON MOB.county_cd = C.county_cd	
                                      WHERE exclude_7day = 1
                                      AND MOB.exclude_nondcfs = 1
                                      AND MOB.exclude_trh = 1
                                      AND MOB.age_yrs_removal = -99
                                      AND MOB.age_yrs_exit = -99 
                                      AND MOB.cd_race = 0 
                                      ORDER BY 
                                      MOB.fiscal_yr
                                      ,C.old_region_cd")										

care_day_movement_less_tx <- filter(rate_care_day_movement_tx, years_in_care <= 2)

care_day_movement_agg <-  care_day_movement_less_tx %>% group_by(fiscal_yr, old_region_cd, years_in_care) %>%
    summarize(kin_cnt = sum(kin_cnt),
              foster_cnt = sum(foster_cnt),
              group_cnt = sum(group_cnt),
              care_days = sum(care_days),
              movement_rate_to_kin = sum(kin_cnt)/sum(care_days) * 100000,
              movement_rate_to_foster = sum(foster_cnt)/sum(care_days) * 100000,
              movement_rate_to_group = sum(group_cnt)/sum(care_days) * 100000)

care_day_movement_more_tx <- filter(rate_care_day_movement_tx, years_in_care >= 3)

care_day_movement_agg_more <-  care_day_movement_more_tx %>% group_by(fiscal_yr, old_region_cd, years_in_care) %>%
    summarize(kin_cnt = sum(kin_cnt),
              foster_cnt = sum(foster_cnt),
              group_cnt = sum(group_cnt),
              care_days = sum(care_days),
              movement_rate_to_kin = sum(kin_cnt)/sum(care_days) * 100000,
              movement_rate_to_foster = sum(foster_cnt)/sum(care_days) * 100000,
              movement_rate_to_group = sum(group_cnt)/sum(care_days) * 100000)

care_day_movement_agg_more$years_in_care <- rep(3, length(care_day_movement_agg_more[[1]]))								

care_day_movement_agg_more <- select_(care_day_movement_agg_more, "fiscal_yr", "old_region_cd", "years_in_care", "kin_cnt", "foster_cnt", "group_cnt", "care_days", "movement_rate_to_kin", "movement_rate_to_foster", "movement_rate_to_group")

care_day_movement_agg <- rbind(care_day_movement_agg, care_day_movement_agg_more)


care_day_movement_agg <- rate_care_day_movement_tx %>% group_by(fiscal_yr, old_region_cd, years_in_care) %>% 
    summarize(kin_cnt = sum(kin_cnt),
              foster_cnt = sum(foster_cnt),
              group_cnt = sum(group_cnt),
              care_days = sum(care_days),
              movement_rate_to_kin = sum(kin_cnt)/sum(care_days) * 100000,
              movement_rate_to_foster = sum(foster_cnt)/sum(care_days) * 100000,
              movement_rate_to_group = sum(group_cnt)/sum(care_days) * 100000)

care_day_movement_melt <- melt(care_day_movement_agg, id.vars = c("fiscal_yr", "old_region_cd", "years_in_care", "kin_cnt", "foster_cnt", "group_cnt", "care_days"))

# creating code and text for different movement types

care_day_movement_melt$cd_movement <- ifelse(care_day_movement_melt$variable == 'movement_rate_to_kin', 0,
                                             ifelse(care_day_movement_melt$variable == 'movement_rate_to_foster', 1, 2))

care_day_movement_melt$tx_movement <- ifelse(care_day_movement_melt$variable == 'movement_rate_to_kin', 'movement_to_kin',
                                             ifelse(care_day_movement_melt$variable == 'movement_rate_to_foster', 'movement_to_foster', 'movement_to_group'))								

care_day_movement_sel <- care_day_movement_melt %>% dplyr::select(fiscal_yr, old_region_cd, years_in_care, kin_cnt, foster_cnt, group_cnt, care_days, cd_movement, tx_movement, value)	

names(care_day_movement_sel) <- c("fiscal_yr", "old_region_cd", "years_in_care", "kin_cnt", "foster_cnt", "group_cnt", "care_days", "cd_movement", "tx_movement", "movement_rate")									

dat2 <- as.data.frame(NULL)
for(h in 0:6){	
    dat1 <- as.data.frame(NULL)
    for (i in 0:2) {
        dat <- lims(care_day_movement_sel, old_region_cd == h & cd_movement == i, x1 = 10)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind.fill(dat2, dat1)					
}	

# loop to get lcl and ucl

dat3 <- as.data.frame(NULL) 
for(j in 0:2){
    dat2 <- as.data.frame(NULL)
    for(h in 0:6){	
        dat1 <- as.data.frame(NULL)
        for (i in 0:3) {
            dat <- lims(care_day_movement_sel, old_region_cd == h & years_in_care == i & cd_movement == j, x1 = 10)
            dat1 <- rbind.fill(dat1, dat)
        }
        dat2 <- rbind.fill(dat2, dat1)					
    }	
    dat3 <- rbind.fill(dat3, dat2)
}

for(i in 1:ncol(dat3)){
    dat3[[i]] <- class_convert(dat3[[i]])
}	

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "placement_care_days_mobility_limits_tds")
sqlSave(con_test_annie, dat = dat3, tablename = "placement_care_days_mobility_limits_tds", rownames = FALSE)

