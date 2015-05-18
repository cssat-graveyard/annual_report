
source("functions.R")

######################
# Rate of Placements #
######################

placement_rate <- sqlQuery(con_test_annie, "SELECT RP.* 
    								,LC.old_region_cd
								FROM test_annie.rate_placement AS RP
                           JOIN ref_lookup_county_region AS LC
                           ON RP.county_cd = LC.county_cd;")						

####################################
# aggregating data to region level #
####################################

test <- placement_rate %>% group_by(qry_type, date_type, cohort_date, entry_point, old_region_cd) %>%
    summarize(cnt_households_w_plcm = sum(cnt_households_w_plcm),
              cnt_referrals_u18 = sum(cnt_referrals_u18),
              rate_placement = sum(cnt_households_w_plcm, na.rm = T)/sum(cnt_referrals_u18, na.rm = T) * 1000) %>%
    filter(cohort_date > start_d & cohort_date < end_d)

#######################
# loop to get ts data #				
#######################

dat2 <- as.data.frame(NULL)

for(h in 0:6){	
    d <- h
    dat1 <- as.data.frame(NULL)
    for (i in 0:6) {
        dat <- ts_func(test, old_region_cd == d & entry_point == i, x = 8)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind(dat2, dat1)					
}									

names(dat2) <- c("qry_type", "date_type", "cohort_date", "entry_point", "old_region_cd", "cnt_households_w_plcm", "cnt_referrals_u18", "rate_placement", "seasonal", "trend")

# loop to change classes to load data into SQL

for(i in 1:ncol(dat2)){
    dat2[, i] <- class_convert(dat2[,i])
}

#####################
# to drop NA fields #
#####################

dat2 <- filter(dat2, is.na(trend) == FALSE)

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.rate_placement_ts")
sqlSave(con_test_annie, dat = dat2, tablename = "test_annie.rate_placement_ts", rownames = FALSE)
