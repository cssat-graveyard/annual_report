
##########################################
# NEED TO USE SQL QUERY WITH TEMP TABLES #
##########################################

source("functions.R")
source("configurations.R")

###############################
# Percent of Children Adopted #
###############################

# loading state level data

sp_adt <- sqlQuery(con, "select 
                   clf.child id_prsn_child 
                   ,[term_to_perm] lolf
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 1, clf.cd_discharge_type) stat
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 'Non-Adoption', clf.alt_discharge_type) discharge_type 
                   ,cd.state_fiscal_yyyy 
                   ,sum(fl_nondcfs_custody) non_dcfs_placements
                   --,tx_jurisdiction
                   --,cd_jurisdiction
                   from ##leg_free clf
                   join ca_ods.dbo.calendar_dim cd
                   on cd.calendar_date = clf.legally_free_date
                   where state_fiscal_yyyy >= 2000
                   and clf.flag_7day = 0
                   and [term_to_perm] > 0 
                   and state_fiscal_yyyy <= 2014 
                   --and row_num = 1
                   group by 
                   clf.child 
                   ,[term_to_perm]
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 1, clf.cd_discharge_type)
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 'Non-Adoption', clf.alt_discharge_type) 
                   ,cd.state_fiscal_yyyy 
                   --,tx_jurisdiction
                   --,cd_jurisdiction
                   having sum(fl_nondcfs_custody) = 0
                   order by
                   id_prsn_child")

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = sp_adt, variance = FALSE)

adopt.1year <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

head(ci.adopt)

adopt.1year$old_region_cd <- 0

############################
# loading data for regions #
############################

location <- sqlQuery(con, "SELECT 
                     county_cd
                     ,county_desc
                     ,old_region_cd 
                     FROM [dbo].[ref_lookup_county]")

sp_adt <- sqlQuery(con, "select 
                   clf.child id_prsn_child 
                   ,[term_to_perm] lolf
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 1, clf.cd_discharge_type) stat
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 'Non-Adoption', clf.alt_discharge_type) discharge_type 
                   ,cd.state_fiscal_yyyy 
                   ,sum(fl_nondcfs_custody) non_dcfs_placements
                   ,tx_jurisdiction
                   ,cd_jurisdiction
                   from ##leg_free clf
                   join ca_ods.dbo.calendar_dim cd
                   on cd.calendar_date = clf.legally_free_date
                   where state_fiscal_yyyy >= 2000
                   and clf.flag_7day = 0
                   and [term_to_perm] > 0 
                   and state_fiscal_yyyy <= 2014 
                   and row_num = 1
                   group by 
                   clf.child 
                   ,[term_to_perm]
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 1, clf.cd_discharge_type)
                   ,iif(clf.cd_discharge_type in (1, 4, 5, 6), 'Non-Adoption', clf.alt_discharge_type) 
                   ,cd.state_fiscal_yyyy 
                   ,tx_jurisdiction
                   ,cd_jurisdiction
                   having sum(fl_nondcfs_custody) = 0
                   order by
                   id_prsn_child")

location$tx_jurisdiction <- ifelse(location$county_desc != '-', paste(location$county_desc, "County"), "-")

sp_adt %<>% left_join(location, by = "tx_jurisdiction") %>% 
    select(-cd_jurisdiction, -county_cd, -tx_jurisdiction, -county_desc)

adopt.1year.r1 <- filter(sp_adt, old_region_cd == 1) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r1, variance = FALSE)

adopt.1year.r1 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r1[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r1$old_region_cd <- 1

adopt.1year.r2 <- filter(sp_adt, old_region_cd == 2) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r2, variance = FALSE)

adopt.1year.r2 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r2[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r2$old_region_cd <- 2

adopt.1year.r3 <- filter(sp_adt, old_region_cd == 3) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r3, variance = FALSE)

adopt.1year.r3 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r3[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r3$old_region_cd <- 3

adopt.1year.r4 <- filter(sp_adt, old_region_cd == 4) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r4, variance = FALSE)

adopt.1year.r4 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r4[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r4$old_region_cd <- 4

adopt.1year.r5 <- filter(sp_adt, old_region_cd == 5) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r5, variance = FALSE)

adopt.1year.r5 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r5[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r5$old_region_cd <- 5

adopt.1year.r6 <- filter(sp_adt, old_region_cd == 6) 

ci.adopt <- Cuminc(time = "lolf", status = "stat", group = "state_fiscal_yyyy", data = adopt.1year.r6, variance = FALSE)

adopt.1year.r6 <- data.frame(year = 2000:2014, adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year.r6[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.3
}

adopt.1year.r6$old_region_cd <- 6

adt.1year <- rbind(adopt.1year,adopt.1year.r1, adopt.1year.r2, adopt.1year.r3, adopt.1year.r4, adopt.1year.r5, adopt.1year.r6)

names(adt.1year) <- c("state_fiscal_yyyy", "perc_adopt", "old_region_cd")	

for(i in 1:ncol(adt.1year)){
    adt.1year[[i]] <- class_convert(adt.1year[[i]])
}		

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.permanency_incidence_adoption")
sqlSave(con_test_annie, dat = adt.1year, tablename = "test_annie.permanency_incidence_adoption", rownames = FALSE)

