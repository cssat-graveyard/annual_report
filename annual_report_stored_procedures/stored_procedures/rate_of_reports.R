
source("functions.R")
source("configurations")

###################
# Rate of Reports #
###################

referral_rate <- sqlQuery(con_test_annie, "SELECT RR.*
                          ,LC.old_region_cd
                          FROM rate_referrals AS RR
                          JOIN ref_lookup_county_region AS LC
                          ON RR.county_cd = LC.county_cd;")

# aggregating data to region level

referral_sums <- referral_rate %>% group_by(qry_type, date_type, start_date, entry_point, old_region_cd) %>%
    summarize(cnt_referrals = sum(cnt_referrals),
              tot_pop = sum(tot_pop),
              referral_rate = sum(cnt_referrals, na.rm = T)/sum(tot_pop, na.rm = T) * 1000) %>%
    filter(start_date > start_d & start_date < end_d)

# loop to get ts data

dat2 <- as.data.frame(NULL)

for(h in 0:6){	
    d <- h
    dat1 <- as.data.frame(NULL)
    for (i in 0:6) {
        dat <- ts_func(referral_sums , old_region_cd == d & entry_point == i, x = 8)
        dat1 <- rbind.fill(dat1, dat)
    }
    dat2 <- rbind(dat2, dat1)					
}	

names(dat2) <- c("qry_type", "date_type", "start_date", "entry_point", "old_region_cd", "cnt_referrals", "tot_pop", "referral_rate", "seasonal", "trend")

# loop to change classes to load data into SQL

for(i in 1:ncol(dat2)){
    dat2[, i] <- class_convert(dat2[,i])
}

# to drop NA fields

dat2 <- filter(dat2, is.na(trend) == FALSE)

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "rate_referrals_ts")
sqlSave(con_test_annie, dat = dat2, tablename = "rate_referrals_ts", rownames = FALSE)


