
source("functions.R")
source("configurations.R")

######################################################
# Percent of Adult-Transition Planning Meetings Held #
######################################################

dat <- sqlQuery(con_poc, "select
                cd_region
                ,count(id_prsn) kids 
                ,sum(iif(plan_cntr is null, 0, 1)) kids_with_plans
                ,sum(iif(plan_cntr is null, 0, 1))*1.0/count(id_prsn) prp_kids_with_plans
                ,cohort_fy
                from ##placement_prep17_5
                group by 
                cohort_fy
                ,cd_region
                UNION ALL
                select
                0 AS cd_region
                ,count(id_prsn) kids 
                ,sum(iif(plan_cntr is null, 0, 1)) kids_with_plans
                ,sum(iif(plan_cntr is null, 0, 1))*1.0/count(id_prsn) prp_kids_with_plans
                ,cohort_fy
                from ##placement_prep17_5
                group by 
                cohort_fy
                order by
                cohort_fy
                ,cd_region")
		
for(i in 1:ncol(dat)){ 
	dat[[i]] <- class_convert(dat[[i]])
}							
		
ggplot(dat, aes(x = cohort_fy, y = prp_kids_with_plans, fill = factor(cd_region))) +
	geom_bar(stat = 'identity', position = position_dodge()) 
	
# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.adult_readiness")
sqlSave(con_test_annie, dat = dat, tablename = "test_annie.adult_readiness", rownames = FALSE)