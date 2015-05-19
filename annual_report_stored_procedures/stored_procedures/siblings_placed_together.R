
##########################################
## NEED TO FIND TEMP TABLE CODE FOR THIS #
##########################################

source("functions.R")
source("configurations.R")

############################
# Siblings Placed Together #
############################

dat <- sqlQuery(con_poc, "select 
                        cd.state_fiscal_yyyy    
                        ,sum(cnt_id_intake_ec) sib_care_days
                        ,sum(iif(max_cnt_id_intake_pc = 1, 0, max_cnt_id_intake_pc)) sib_care_days_some_tgh
                        ,sum(iif(max_cnt_id_intake_pc = 1, 0, max_cnt_id_intake_pc))*1.0/sum(cnt_id_intake_ec) prp_some_tgh 
                      from ##tbl_sib_plc_tgh_cnt ptc
                        join ca_ods.dbo.calendar_dim cd
                            on cd.id_calendar_dim = ptc.id_calendar_dim 
                      group by 
                        cd.state_fiscal_yyyy  
                      order by 
                        cd.state_fiscal_yyyy")

for(i in 1:ncol(dat)){ 
    dat[[i]] <- class_convert(dat[[i]])
}							

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.sibling_placement")
sqlSave(con_test_annie, dat = dat, tablename = "test_annie.sibling_placement", rownames = FALSE)
