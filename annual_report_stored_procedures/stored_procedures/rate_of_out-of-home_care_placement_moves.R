
source("functions.R")
source("configurations.R")

############################################
# Rate of Out-of-Home Care Placement Moves #
############################################

rate_care_day_movement <- sqlQuery(con_poc,"SELECT
                                                MOB.fiscal_yr
                                                ,MOB.county_cd
                                                ,C.old_region_cd
                                                ,MOB.years_in_care
                                                ,MOB.placement_moves
                                                ,MOB.care_days
                                                ,MOB.placement_moves*100000.0 / IIF(MOB.care_days = 0, NULL, MOB.care_days) AS movement_rate
                                            FROM ca_ods.prtl.placement_care_days_mobility AS MOB
                                            JOIN [dbo].[ref_lookup_county] AS C
                                                ON MOB.county_cd = C.county_cd	
                                            WHERE MOB.exclude_7day = 1
                                                AND MOB.exclude_nondcfs = 1
                                                AND MOB.exclude_trh = 1
                                                AND MOB.age_yrs_removal = -99
                                                AND MOB.age_yrs_exit = -99 
                                                AND MOB.cd_race = 0 
                                            ORDER BY
                                                MOB.fiscal_yr
                                                ,county_cd")

#############
# filtering # 											
#############

care_day_movement_less <- filter(rate_care_day_movement, years_in_care <= 2)

care_day_movement_agg <-  care_day_movement_less %>% group_by(fiscal_yr, old_region_cd, years_in_care) %>%
    summarize(placement_moves = sum(placement_moves),
              care_days = sum(care_days),
              movement_rate = sum(placement_moves) / sum(care_days) * 100000)

care_day_movement_more <- filter(rate_care_day_movement, years_in_care >= 3)

care_day_movement_agg_more <-  care_day_movement_more %>% group_by(fiscal_yr, old_region_cd) %>%
    summarize(placement_moves = sum(placement_moves),
              care_days = sum(care_days),
              movement_rate = sum(placement_moves) / sum(care_days) * 100000)

care_day_movement_agg_more$years_in_care <- rep(3, length(care_day_movement_agg_more[[1]]))								

care_day_movement_agg_more <- select_(care_day_movement_agg_more,"fiscal_yr", "old_region_cd", "years_in_care", "placement_moves", "care_days", "movement_rate")

care_day_movement_agg <- rbind(care_day_movement_agg, care_day_movement_agg_more)

#########################################
# loop with function to get lcl and ucl #
#########################################

dat2 <- as.data.frame(NULL)

for(h in 0:6){	
    d <- h
    dat1 <- as.data.frame(NULL)
    for (i in 0:3) {
        dat <- lims(care_day_movement_agg, old_region_cd == d & years_in_care == i, x1 = 6)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind(dat2, dat1)					
}	

for(i in 1:ncol(dat2)){
    dat2[[i]] <- class_convert(dat2[[i]])
}	

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.placement_care_days_mobility_limits_ds")
sqlSave(con_test_annie, dat = dat2, tablename = "test_annie.placement_care_days_mobility_limits_ds", rownames = FALSE)															