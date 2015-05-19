
source("functions.R")
source("configurations.R")

###########################################
# pRate of Placements by Prior Placements #
###########################################

placement_order <- sqlQuery(con_test_annie, "SELECT RO.*
                            ,LC.old_region_cd
                            FROM rate_placement_order_specific AS RO
                            JOIN ref_lookup_county_region AS LC
                            ON LC.county_cd = ro.county_cd;")

# aggregating data to region level

placement_order_agg <- placement_order %>% group_by(qry_type, date_type, cohort_date, nth_order, old_region_cd) %>%
    summarize(cnt_nth_order_placement_cases = sum(cnt_nth_order_placement_cases),
              cnt_prior_order_si_referrals = sum(cnt_prior_order_si_referrals),
              placement_rate = sum(cnt_nth_order_placement_cases, na.rm = T)/sum(cnt_prior_order_si_referrals, na.rm = T) * 1000) %>%
    filter(cohort_date > start_d & cohort_date < end_d)

# loop to get ts data	

dat2 <- as.data.frame(NULL)

for(h in 0:6){	
    d <- h
    dat1 <- as.data.frame(NULL)
    for (i in 1:3) {
        dat <- ts_func(placement_order_agg, old_region_cd == d & nth_order == i, x = 8)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind(dat2, dat1)					
}									

names(dat2) <- c("qry_type", "date_type", "cohort_date", "nth_order", "old_region_cd", "cnt_nth_order_placement_cases", "cnt_prior_order_si_referrals", "placement_rate", "seasonal", "trend")	

for(i in 1:ncol(dat2)){
    dat2[, i] <- class_convert(dat2[,i])
}

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.rate_placement_order_specific_ts")
sqlSave(con_test_annie, dat = dat2, tablename = "test_annie.rate_placement_order_specific_ts", rownames = FALSE)	

