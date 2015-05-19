
##########################################
# NEED TO USE SQL QUERY WITH TEMP TABLES #
##########################################

source("functions.R")
source("configurations.R")

############################################
# Percent of Children Achieving Permanency #
############################################

long_data <- sqlQuery(con_poc, "select 
    plc.child id_prsn_child 
	,datediff(dd, plc.removal_dt, discharg_frc_18) los
                      ,case
                      when rldte.cd_discharge_type between 1 and 4
                      then 1
                      when rldte.cd_discharge_type between 5 and 6
                      then 2
                      else 0
                      end stat
                      ,case
                      when rldte.cd_discharge_type between 1 and 4
                      then 'Federal Permanency'
                      when rldte.cd_discharge_type between 5 and 6
                      then 'Other Outcome'
                      else 'Still In Care'
                      end discharge_type
                      ,cd.state_fiscal_yyyy 
                      ,sum(fl_nondcfs_custody) non_dcfs_placements
                      ,county_cd
                      from ##placements plc
                      join ref_lookup_cd_discharge_type_exits rldte
                      on rldte.cd_discharge_type = plc.cd_discharge_type
                      join ca_ods.dbo.calendar_dim cd
                      on cd.calendar_date = plc.removal_dt
                      where state_fiscal_yyyy >= 2000
                      and plc.flag_7day = 0
                      and plc.ord = 1
                      group by 
                      plc.child 
                      ,datediff(dd, plc.removal_dt, discharg_frc_18)
                      ,case
                      when rldte.cd_discharge_type between 1 and 4
                      then 1
                      when rldte.cd_discharge_type between 5 and 6
                      then 2
                      else 0
                      end 
                      ,case
                      when rldte.cd_discharge_type between 1 and 4
                      then 'Federal Permanency'
                      when rldte.cd_discharge_type between 5 and 6
                      then 'Other Outcome'
                      else 'Still In Care'
                      end 
                      ,cd.state_fiscal_yyyy 
                      ,county_cd
                      having sum(fl_nondcfs_custody) = 0
                      order by 
                      plc.child")

short_data <- sqlQuery(con_poc, "select 
                       plc.child id_prsn_child 
                       ,datediff(dd, plc.removal_dt, discharg_frc_18) los
                       ,case
                       when rldte.cd_discharge_type between 1 and 4
                       then 1
                       when rldte.cd_discharge_type between 5 and 6
                       then 2
                       else 0
                       end stat
                       ,case
                       when rldte.cd_discharge_type between 1 and 4
                       then 'Federal Permanency'
                       when rldte.cd_discharge_type between 5 and 6
                       then 'Other Outcome'
                       else 'Still In Care'
                       end discharge_type
                       ,cd.state_fiscal_yyyy 
                       ,sum(fl_nondcfs_custody) non_dcfs_placements
                       from ##placements plc
                       join ref_lookup_cd_discharge_type_exits rldte
                       on rldte.cd_discharge_type = plc.cd_discharge_type
                       join ca_ods.dbo.calendar_dim cd
                       on cd.calendar_date = plc.removal_dt
                       where state_fiscal_yyyy >= 2000
                       and plc.flag_7day = 0
                       group by 
                       plc.child 
                       ,datediff(dd, plc.removal_dt, discharg_frc_18)
                       ,case
                       when rldte.cd_discharge_type between 1 and 4
                       then 1
                       when rldte.cd_discharge_type between 5 and 6
                       then 2
                       else 0
                       end 
                       --,iif(rldte.cd_discharge_type >=3, 2, rldte.cd_discharge_type) stat
                       ,case
                       when rldte.cd_discharge_type between 1 and 4
                       then 'Federal Permanency'
                       when rldte.cd_discharge_type between 5 and 6
                       then 'Other Outcome'
                       else 'Still In Care'
                       end 
                       ,cd.state_fiscal_yyyy 
                       having sum(fl_nondcfs_custody) = 0
                       order by 
                       plc.child")	

perm_one_year <- left_join(short_data, long_data)

location <- sqlQuery(con, "SELECT 
                     county_cd
                     ,old_region_cd 
                     FROM [dbo].[ref_lookup_county]")

perm_one_year <- left_join(perm_one_year, location)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm_one_year)

perm.1year <- data.frame(year = 2000:2014
                         ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year$old_region_cd <- 0

perm.1year.r1 <- filter(perm_one_year, old_region_cd == 1) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r1)

perm.1year.r1 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r1[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r1$old_region_cd <- 1

perm.1year.r2 <- filter(perm_one_year, old_region_cd == 2) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r2)

perm.1year.r2 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r2[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r2$old_region_cd <- 2

perm.1year.r3 <- filter(perm_one_year, old_region_cd == 3) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r3)

perm.1year.r3 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r3[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r3$old_region_cd <- 3

perm.1year.r4 <- filter(perm_one_year, old_region_cd == 4) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r4)

perm.1year.r4 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r4[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r4$old_region_cd <- 4

perm.1year.r5 <- filter(perm_one_year, old_region_cd == 5) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r5)

perm.1year.r5 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r5[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r5$old_region_cd <- 5

perm.1year.r6 <- filter(perm_one_year, old_region_cd == 6) %>% select(-county_cd)

ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = perm.1year.r6)

perm.1year.r6 <- data.frame(year = 2000:2014
                            ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year.r6[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

perm.1year.r6$old_region_cd <- 6

perm.1year <- rbind(perm.1year, perm.1year.r1, perm.1year.r2, perm.1year.r3, perm.1year.r4, perm.1year.r5, perm.1year.r6)

for(i in 1:ncol(perm.1year)){
    perm.1year[[i]] <- class_convert(perm.1year[[i]]) 
}		

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.permanency_incidence_av")
sqlSave(con_test_annie, dat = perm.1year, tablename = "test_annie.mp_permanency_incidence_av", rownames = FALSE)