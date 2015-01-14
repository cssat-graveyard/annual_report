library(ggplot2)
library(plotflow)
library(pocr)
library(dplyr)
library(mstate)
library(magrittr)

#setwd("S:/Data Portal/annual_report")
load("graph_dat.RData")

names(rate_care_day_movement_tx) <- make.names(names(rate_care_day_movement_tx))
rcdm = rate_care_day_movement_tx
rcdm %<>% mutate(type = factor(type, levels = c("Transition to Foster",
                                                "Transition to Kin",
                                                "Transition to Group")),
                 years_in_care = factor(paste("years", years_in_care)))

movement_tx <- rcdm %>% select(fiscal_yr, Movement.Rate) %>%
    split(f = list(rcdm$years_in_care, rcdm$type))
names(movement_tx) = make.names(names(movement_tx))

rcdm %>% group_by(type, years_in_care) %>%
    arrange(fiscal_yr) %>%
    summarize(y2000 = first(Movement.Rate),
              y2014 = last(Movement.Rate))


names(rate_care_day_otr) <- make.names(names(rate_care_day_otr))
otr = rate_care_day_otr %>% select(fiscal_yr, On.The.Run.Rate) %>%
    mutate(On.The.Run.Rate = 100 * On.The.Run.Rate) %>%
    split(f = rate_care_day_otr$age_in_fy)
names(otr) = paste("age", names(otr))


# We need to recalculate permanency and adoption with unique names
ci.perm <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = sp_perm)

perm.1year <- data.frame(year = 2000:2014
                         ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    perm.1year[i,2] <- ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.perm[which(ci.perm$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

ci.adopt <- Cuminc(time = "lolf"
             ,status = "stat"
             ,group = "state_fiscal_yyyy"
             ,data = sp_adt
             ,variance = FALSE)

adopt.1year <- data.frame(year = 2000:2014
                       ,adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    adopt.1year[i,2] <- ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci.adopt[which(ci.adopt$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.2
}

# Reentry #OLD CHANGE ME

sp_reent <- sqlQuery(con, "select * from ca_ods.base.episode_reentries")
fitCI <- survfit(Surv(lop, stat) ~ state_fiscal_yyyy,
                 data=sp_reent)
years <- 2000:2014
ci.reent <- data.frame(time=fitCI$time
                 ,surv=fitCI$surv
                 ,strata=rep(years, fitCI$strata))

reent.1year <- data.frame(year = 2000:2014
                       ,reent = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
    reent.1year[i,2] <- 1-ci.reent[which(ci.reent$strata == (i+1999)), ][which.min(abs(ci.reent[which(ci.reent$strata == (i+1999)), ]$time - 365)), ]$surv
}


## Re-done siblings:

## I ran this query once and saved the output since it depends on temp tables
# sibs <- sqlQuery(con, "select 
#                         cd.state_fiscal_yyyy    
#                         ,sum(cnt_id_intake_ec) sib_care_days
#                         ,sum(iif(max_cnt_id_intake_pc = 1, 0, max_cnt_id_intake_pc)) sib_care_days_some_tgh
#                         ,sum(iif(max_cnt_id_intake_pc = 1, 0, max_cnt_id_intake_pc))*1.0/sum(cnt_id_intake_ec) prp_some_tgh 
#                       from ##tbl_sib_plc_tgh_cnt ptc
#                         join ca_ods.dbo.calendar_dim cd
#                             on cd.id_calendar_dim = ptc.id_calendar_dim 
#                       group by 
#                         cd.state_fiscal_yyyy  
#                       order by 
#                         cd.state_fiscal_yyyy")
#save(sibs, file = "sibs-recalculated.Rdata")

## Correcting transition staffing:
# plan <- sqlQuery(con, "select
#                         count(id_prsn) kids 
#                         ,sum(iif(plan_cntr is null, 0, 1)) kids_with_plans
#                         ,sum(iif(plan_cntr is null, 0, 1))*1.0/count(id_prsn) prp_kids_with_plans
#                         ,cohort_fy
#                       from ##placement_prep17_5
#                       group by 
#                         cohort_fy
#                       order by
#                         cohort_fy")
# save(plan, file = "plan-recal.Rdata")
load("plan-recal.Rdata")
plan.transition = plan %>% select(fiscal_yr = cohort_fy, prop = prp_kids_with_plans)


sibs.together <- select(sibs, fiscal_yr = state_fiscal_yyyy, some_together = prp_some_tgh)

sparkdata <- c(list(
    # General Safety
    gen.rate.referrals = sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",
                                                c("date", "referral.rate")],
    gen.rate.screen = sp_rate_referral_scrn_in_clean[sp_rate_referral_scrn_in_clean$type=="Deseasonalized Trend",
                                                     c("date", "referral.rate")],
    gen.rate.place = sp_rate_placement_clean[sp_rate_placement_clean$type=="Deseasonalized Trend",
                                             c("cohort.date", "placement.rate")],
    # Re-Referrals
    re.ref.1 = filter(sp_rate_referrals_order_clean, order == "1st Order") %>%
        select(date, referral.rate),
    re.ref.2 = filter(sp_rate_referrals_order_clean, order == "2nd Order") %>%
        select(date, referral.rate),
    re.ref.3 = filter(sp_rate_referrals_order_clean, order == "3rd Order") %>%
        select(date, referral.rate),
    # Screenings
    re.screen.1 = filter(sp_rate_referrals_scrn_in_order_clean, order == "1st Order") %>%
        select(date, referral.rate),
    re.screen.2 = filter(sp_rate_referrals_scrn_in_order_clean, order == "2nd Order") %>%
        select(date, referral.rate),
    re.screen.3 = filter(sp_rate_referrals_scrn_in_order_clean, order == "3rd Order") %>%
        select(date, referral.rate),
    # Placement
    re.place.1 = filter(sp_rate_placement_order_clean, order == "1st Order") %>%
        select(cohort.date, placement.rate),
    re.place.2 = filter(sp_rate_placement_order_clean, order == "2nd Order") %>%
        select(cohort.date, placement.rate),
    re.place.3 = filter(sp_rate_placement_order_clean, order == "3rd Order") %>%
        select(cohort.date, placement.rate),
    # Safe Care Days
    safe.care = sp_rate_care_day_maltreatment_clean %>% 
        mutate(safe.rate = 1e5 - care.day.incident.rate) %>%
        select(fiscal.yr, safe.rate),
    # General Movement
    move.1 = filter(rate_care_day_movement_long, str_detect(years_in_care, "First"),
                    variable == "Movement Rate") %>%
        select(fiscal_yr, value),
    move.2 = filter(rate_care_day_movement_long,
                    str_detect(years_in_care, "Second"),
                    variable == "Movement Rate") %>%
        select(fiscal_yr, value),
    move.3 = filter(rate_care_day_movement_long,
                    str_detect(years_in_care, "Third"),
                    variable == "Movement Rate") %>%
        select(fiscal_yr, value)
    ),
    # Specific Movement (in separate list---see above)
    movement_tx,
    # On-The-Run, in separate list (see above)
    otr,
    list(
        # permanency within 1 year
        perm.1year = perm.1year,
        # adoption within 1 year
        adopt.1year = adopt.1year,
        reent.1year = reent.1year,
        sibs.together = sibs.together,
        plan.transition = plan.transition
    )
)

sparkplots <- lapply(sparkdata, FUN = spark)

for(i in seq_along(sparkplots)) {
    ggsave(filename = paste0("spark-", names(sparkplots)[i], ".pdf"),
           plot = sparkplots[[i]], width = 3, height = 1)
}

pdfs <- list.files(pattern = "\\.pdf")
lapply(pdfs, embedFonts)


tables <- plyr::ldply(sparkdata, .fun = sparktable)
tables$pdf <- paste0("spark-", names(sparkdata))
write.csv(tables[, -1], file = "spark table data.csv", row.names = F)



