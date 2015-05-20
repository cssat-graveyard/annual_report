
source("functions.R")
source("configurations.R")

#############################################################
# Rate of Substantiated Cases of Maltreatment While in Care #
#############################################################

maltreatment <- sqlQuery(con_test_annie, "SELECT 
                         M.*
                         ,LC.old_region_cd
                         FROM rate_care_day_maltreatment AS M
                         JOIN ref_lookup_county_region AS LC
                         ON M.county_cd = LC.county_cd
                         ORDER BY 
                         M.county_cd
                         ,M.fiscal_yr asc;")

# from the code Joe used, not sure where this came from and we don't have region data at the moment
maltreatment <- rbind(maltreatment, c(0, 2, 2014, 0, 3099673, 196, 6.323247646, 0))

maltreatment_agg <- maltreatment %>% group_by(date_type, qry_type, old_region_cd, fiscal_yr) %>%
    summarize(cnt_incidents = sum(cnt_incidents),
              care_days = sum(care_days),
              care_day_incident_rate  = sum(cnt_incidents)/sum(care_days) * 100000)

####################
# testing function #
####################

lims(maltreatment, old_region_cd == 0, x1 = 7)

##########################
# function to get limits #
##########################

dat1 <- as.data.frame(NULL)

for (i in 0:6) {
    dat <- lims(maltreatment_agg, old_region_cd == i, x1 = 7)
    dat1 <- rbind.fill(dat1, dat)
}

############################
# preparing data for mySQL #
############################

for(i in 1:ncol(dat1)){
    dat1[, i] <- class_convert(dat1[,i])
}									

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "rate_care_day_maltreatment_limits")
sqlSave(con_test_annie, dat = dat1, tablename = "rate_care_day_maltreatment_limits", rownames = FALSE)