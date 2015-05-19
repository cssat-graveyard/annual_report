
source("functions.R")
source("configurations.R")

########################################
# Percent of Children Re-Entering Care #
########################################

sp_reent <- sqlQuery(con_poc, "SELECT
                     cal.STATE_FISCAL_YYYY
                     ,c.old_region_cd
                     ,c.old_region_desc
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366, 1, 0)) AS reent_365
                     ,COUNT(eps.federal_discharge_date) AS 'count_of_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366, 1, 0)) * 1.0 / COUNT(eps.federal_discharge_date) AS 'per_reent'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Reunification', 1, 0)) as 'reun_reent'
                     ,SUM(IIF(eps.discharge_type = 'Reunification', 1, 0)) AS 'reun_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Reunification', 1, 0)) * 1.0 /
                     SUM(IIF(eps.discharge_type = 'Reunification', 1, 0)) AS 'reun_per_reent'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Guardianship', 1, 0)) as 'guard_reent'
                     ,SUM(IIF(eps.discharge_type = 'Guardianship', 1, 0)) AS 'guard_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Guardianship', 1, 0))  * 1.0 / 
                     SUM(IIF(eps.discharge_type = 'Guardianship', 1, 0)) AS 'guard_per_exits'
                     FROM
                     (SELECT
                     cal.STATE_FISCAL_YYYY
                     ,eps.id_prsn_child
                     ,min(eps.federal_discharge_date) AS federal_discharge_date
                     FROM
                     (SELECT 
                     eps.id_prsn_child
                     ,eps.removal_dt
                     ,federal_discharge_date
                     FROM [CA_ODS].[prtl].[ooh_dcfs_eps] AS eps
                     WHERE -- id_prsn_child = 33564
                     -- DATEDIFF(YEAR, eps.birth_dt, eps.removal_dt) < 18
                     [dbo].[fnc_datediff_yrs](eps.birth_dt, eps.removal_dt) < 18
                     AND fl_exit_over_17 = 0
                     AND DATEDIFF(DAY, eps.removal_dt, federal_discharge_date) > 7
                     AND discharge_type IN ('Guardianship', 'Reunification')) AS id
                     JOIN [CA_ODS].[prtl].[ooh_dcfs_eps] as eps
                     ON id.federal_discharge_date = eps.federal_discharge_date
                     AND id.id_prsn_child = eps.id_prsn_child
                     AND id.removal_dt = eps.removal_dt
                     JOIN CA_ODS.[dbo].[CALENDAR_DIM] AS cal
                     ON eps.federal_discharge_date = cal.CALENDAR_DATE
                     GROUP BY
                     cal.STATE_FISCAL_YYYY
                     ,eps.id_prsn_child) AS id
                     JOIN [CA_ODS].[prtl].[ooh_dcfs_eps] as eps
                     ON id.federal_discharge_date = eps.federal_discharge_date
                     AND id.id_prsn_child = eps.id_prsn_child
                     JOIN CA_ODS.[dbo].[ref_lookup_county] AS c
                     ON eps.exit_county_cd = c.county_cd
                     AND old_region_cd != -99
                     JOIN CA_ODS.[dbo].[CALENDAR_DIM] AS cal
                     ON eps.federal_discharge_date = cal.CALENDAR_DATE
                     AND cal.STATE_FISCAL_YYYY > 1999
                     AND cal.STATE_FISCAL_YYYY < 2015
                     GROUP BY
                     cal.STATE_FISCAL_YYYY
                     ,c.old_region_cd
                     ,c.old_region_desc
                     UNION ALL
                     SELECT  
                     cal.STATE_FISCAL_YYYY
                     ,0 AS 'old_region_cd'
                     ,'State' AS old_region_desc
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366, 1, 0)) AS reent_365
                     ,COUNT(eps.federal_discharge_date) AS 'count_of_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366, 1, 0)) * 1.0 / COUNT(eps.federal_discharge_date) AS 'per_reent'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Reunification', 1, 0)) as 'reun_reent'
                     ,SUM(IIF(eps.discharge_type = 'Reunification', 1, 0)) AS 'reun_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Reunification', 1, 0)) * 1.0 /
                     SUM(IIF(eps.discharge_type = 'Reunification', 1, 0)) AS 'reun_per_reent'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Guardianship', 1, 0)) as 'guard_reent'
                     ,SUM(IIF(eps.discharge_type = 'Guardianship', 1, 0)) AS 'guard_exits'
                     ,SUM(IIF(eps.fl_reentry = 1 AND DATEDIFF(DAY, eps.federal_discharge_date, eps.next_reentry_date) < 366 AND eps.discharge_type = 'Guardianship', 1, 0))  * 1.0 / 
                     SUM(IIF(eps.discharge_type = 'Guardianship', 1, 0)) AS 'guard_per_exits'
                     FROM
                     (SELECT
                     cal.STATE_FISCAL_YYYY
                     ,eps.id_prsn_child
                     ,min(eps.federal_discharge_date) AS federal_discharge_date
                     FROM
                     (SELECT 
                     eps.id_prsn_child
                     ,eps.removal_dt
                     ,federal_discharge_date
                     FROM [CA_ODS].[prtl].[ooh_dcfs_eps] AS eps
                     WHERE -- id_prsn_child = 33564
                     -- DATEDIFF(YEAR, eps.birth_dt, eps.removal_dt) < 18
                     [dbo].[fnc_datediff_yrs](eps.birth_dt, eps.removal_dt) < 18
                     AND fl_exit_over_17 = 0
                     AND DATEDIFF(DAY, eps.removal_dt, federal_discharge_date) > 7
                     AND discharge_type IN ('Guardianship', 'Reunification')) AS id 
                     JOIN [CA_ODS].[prtl].[ooh_dcfs_eps] as eps
                     ON id.federal_discharge_date = eps.federal_discharge_date
                     AND id.id_prsn_child = eps.id_prsn_child
                     AND id.removal_dt = eps.removal_dt
                     JOIN CA_ODS.[dbo].[CALENDAR_DIM] AS cal
                     ON eps.federal_discharge_date = cal.CALENDAR_DATE
                     GROUP BY
                     cal.STATE_FISCAL_YYYY
                     ,eps.id_prsn_child) AS id
                     JOIN [CA_ODS].[prtl].[ooh_dcfs_eps] as eps
                     ON id.federal_discharge_date = eps.federal_discharge_date
                     AND id.id_prsn_child = eps.id_prsn_child
                     JOIN CA_ODS.[dbo].[ref_lookup_county] AS c
                     ON eps.exit_county_cd = c.county_cd
                     JOIN CA_ODS.[dbo].[CALENDAR_DIM] AS cal
                     ON eps.federal_discharge_date = cal.CALENDAR_DATE
                     AND cal.STATE_FISCAL_YYYY > 1999
                     AND cal.STATE_FISCAL_YYYY < 2015
                     GROUP BY
                     cal.STATE_FISCAL_YYYY
                     ORDER BY
                     cal.STATE_FISCAL_YYYY
                     ,c.old_region_cd")

reent <- sp_reent %>% select(STATE_FISCAL_YYYY, old_region_cd, old_region_desc, reent_365, count_of_exits, per_reent)

for(i in 1:ncol(reent)){ 
	reent[[i]] <- class_convert(reent[[i]])
}	

# loading data into mySQL
# sqlDrop(con_test_annie, sqtable = "test_annie.episode_reentries_av")
sqlSave(con_test_annie, dat = reent, tablename = "test_annie.episode_reentries_av", rownames = FALSE)
