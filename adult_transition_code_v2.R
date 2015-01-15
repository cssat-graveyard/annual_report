# graph 3

require(pocr)
con <- odbcConnect("POC")
dat <- sqlQuery(con, "select 
                sum([cnt_referrals]) all_ref
                ,ref_gt_3
                ,ref1.start_date
                ,ref_gt_3*1.0/sum([cnt_referrals]) prp_gt3
                from ca_ods.prtl.rate_referrals_order_specific ref1
                join (select 
                sum([cnt_referrals]) ref_gt_3
                ,start_date
                from ca_ods.prtl.rate_referrals_order_specific
                where county_cd = 0 
                and nth_order > 3
                and start_date >= '2009-01-01'
                group by 
                start_date) ref2
                on ref1.start_date = ref2.start_date
                where county_cd = 0 
                group by 
                ref_gt_3
                ,ref1.start_date")

load("sibs-recalculated.Rdata")

pR_I_prpgt3 <- ggplot(dat, aes(x=start_date, y = prp_gt3, colour=poc_colors[3])) + 
  geom_smooth(colour=poc_colors[3], fill=NA, size=2) + 
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Percentage of Intakes with More than 3 Prior Intakes", x = "", y = "") + 
  theme_bw()

ggsave(file="graph3_v2.pdf")

# graph 8

require(pocr)
con <- odbcConnect("POC")
rate_care_day_movement_tx <- sqlQuery(con,"select
                                      fiscal_yr
                                      ,years_in_care
                                      ,kin_cnt*100000.0/care_days movement_rate_to_kin
                                      ,foster_cnt*100000.0/care_days movement_rate_to_foster
                                      ,group_cnt*100000.0/care_days movement_rate_to_group 
                                      from ca_ods.prtl.placement_care_days_mobility
                                      where exclude_7day = 1
                                      and exclude_nondcfs = 1
                                      and exclude_trh = 1
                                      and age_yrs_removal = -99
                                      and age_yrs_exit = -99 
                                      and county_cd = 0
                                      and cd_race = 0 
                                      and years_in_care <= 2")


rate_care_day_movement_kin_0 <- subset(rate_care_day_movement_tx, years_in_care == 0)
mean_rate_0 <- mean(rate_care_day_movement_kin_0$movement_rate_to_kin[1:n-1])
rate_care_day_movement_kin_0$variance <- abs(rate_care_day_movement_kin_0$movement_rate_to_kin - mean_rate_0)
mean_variance_0 <- mean(rate_care_day_movement_kin_0$variance)
rate_care_day_movement_kin_0$lcl_kin <- mean_rate_0-(mean_variance_0*qnorm(0.997))
rate_care_day_movement_kin_0$ucl_kin <- mean_rate_0+(mean_variance_0*qnorm(0.997))

rate_care_day_movement_foster_0 <- subset(rate_care_day_movement_tx, years_in_care == 0)
mean_rate_0 <- mean(rate_care_day_movement_foster_0$movement_rate_to_foster[1:n-1])
rate_care_day_movement_foster_0$variance <- abs(rate_care_day_movement_foster_0$movement_rate_to_foster - mean_rate_0)
mean_variance_0 <- mean(rate_care_day_movement_foster_0$variance)
rate_care_day_movement_foster_0$lcl_foster <- mean_rate_0-(mean_variance_0*qnorm(0.997))
rate_care_day_movement_foster_0$ucl_foster <- mean_rate_0+(mean_variance_0*qnorm(0.997))

rate_care_day_movement_group_0 <- subset(rate_care_day_movement_tx, years_in_care == 0)
mean_rate_0 <- mean(rate_care_day_movement_group_0$movement_rate_to_group[1:n-1])
rate_care_day_movement_group_0$variance <- abs(rate_care_day_movement_group_0$movement_rate_to_group - mean_rate_0)
mean_variance_0 <- mean(rate_care_day_movement_group_0$variance)
rate_care_day_movement_group_0$lcl_group <- mean_rate_0-(mean_variance_0*qnorm(0.997))
rate_care_day_movement_group_0$ucl_group <- mean_rate_0+(mean_variance_0*qnorm(0.997))

rate_care_day_movement_kin_1 <- subset(rate_care_day_movement_tx, years_in_care == 1)
mean_rate_1 <- mean(rate_care_day_movement_kin_1$movement_rate_to_kin[1:n-1])
rate_care_day_movement_kin_1$variance <- abs(rate_care_day_movement_kin_1$movement_rate_to_kin - mean_rate_1)
mean_variance_1 <- mean(rate_care_day_movement_kin_1$variance)
rate_care_day_movement_kin_1$lcl_kin <- mean_rate_1-(mean_variance_1*qnorm(0.997))
rate_care_day_movement_kin_1$ucl_kin <- mean_rate_1+(mean_variance_1*qnorm(0.997))

rate_care_day_movement_foster_1 <- subset(rate_care_day_movement_tx, years_in_care == 1)
mean_rate_1 <- mean(rate_care_day_movement_foster_1$movement_rate_to_foster[1:n-1])
rate_care_day_movement_foster_1$variance <- abs(rate_care_day_movement_foster_1$movement_rate_to_foster - mean_rate_1)
mean_variance_1 <- mean(rate_care_day_movement_foster_1$variance)
rate_care_day_movement_foster_1$lcl_foster <- mean_rate_1-(mean_variance_1*qnorm(0.997))
rate_care_day_movement_foster_1$ucl_foster <- mean_rate_1+(mean_variance_1*qnorm(0.997))

rate_care_day_movement_group_1 <- subset(rate_care_day_movement_tx, years_in_care == 1)
mean_rate_1 <- mean(rate_care_day_movement_group_1$movement_rate_to_group[1:n-1])
rate_care_day_movement_group_1$variance <- abs(rate_care_day_movement_group_1$movement_rate_to_group - mean_rate_1)
mean_variance_1 <- mean(rate_care_day_movement_group_1$variance)
rate_care_day_movement_group_1$lcl_group <- mean_rate_1-(mean_variance_1*qnorm(0.997))
rate_care_day_movement_group_1$ucl_group <- mean_rate_1+(mean_variance_1*qnorm(0.997))

rate_care_day_movement_kin_2 <- subset(rate_care_day_movement_tx, years_in_care == 2)
mean_rate_2 <- mean(rate_care_day_movement_kin_2$movement_rate_to_kin[1:n-1])
rate_care_day_movement_kin_2$variance <- abs(rate_care_day_movement_kin_2$movement_rate_to_kin - mean_rate_2)
mean_variance_2 <- mean(rate_care_day_movement_kin_2$variance)
rate_care_day_movement_kin_2$lcl_kin <- mean_rate_2-(mean_variance_2*qnorm(0.997))
rate_care_day_movement_kin_2$ucl_kin <- mean_rate_2+(mean_variance_2*qnorm(0.997))

rate_care_day_movement_foster_2 <- subset(rate_care_day_movement_tx, years_in_care == 2)
mean_rate_2 <- mean(rate_care_day_movement_foster_2$movement_rate_to_foster[1:n-1])
rate_care_day_movement_foster_2$variance <- abs(rate_care_day_movement_foster_2$movement_rate_to_foster - mean_rate_2)
mean_variance_2 <- mean(rate_care_day_movement_foster_2$variance)
rate_care_day_movement_foster_2$lcl_foster <- mean_rate_2-(mean_variance_2*qnorm(0.997))
rate_care_day_movement_foster_2$ucl_foster <- mean_rate_2+(mean_variance_2*qnorm(0.997))

rate_care_day_movement_group_2 <- subset(rate_care_day_movement_tx, years_in_care == 2)
mean_rate_2 <- mean(rate_care_day_movement_group_2$movement_rate_to_group[1:n-1])
rate_care_day_movement_group_2$variance <- abs(rate_care_day_movement_group_2$movement_rate_to_group - mean_rate_2)
mean_variance_2 <- mean(rate_care_day_movement_group_2$variance)
rate_care_day_movement_group_2$lcl_group <- mean_rate_2-(mean_variance_2*qnorm(0.997))
rate_care_day_movement_group_2$ucl_group <- mean_rate_2+(mean_variance_2*qnorm(0.997))


rate_care_day_movement_group <- rbind(rate_care_day_movement_group_0[,-which(names(rate_care_day_movement_group_0) %in% c("movement_rate_to_kin"
                                                                                                                          ,"movement_rate_to_foster"
                                                                                                                          ,"variance"))]
                                      ,rate_care_day_movement_group_1[,-which(names(rate_care_day_movement_group_1) %in% c("movement_rate_to_kin"
                                                                                                                           ,"movement_rate_to_foster"
                                                                                                                           ,"variance"))]
                                      ,rate_care_day_movement_group_2[,-which(names(rate_care_day_movement_group_2) %in% c("movement_rate_to_kin"
                                                                                                                           ,"movement_rate_to_foster"
                                                                                                                           ,"variance"))])
names(rate_care_day_movement_group) <- c("fiscal_yr", "years_in_care", "movement_rate", "lcl", "ucl")

rate_care_day_movement_foster <- rbind(rate_care_day_movement_foster_0[,-which(names(rate_care_day_movement_foster_0) %in% c("movement_rate_to_kin"
                                                                                                                             ,"movement_rate_to_group"
                                                                                                                             ,"variance"))]
                                       ,rate_care_day_movement_foster_1[,-which(names(rate_care_day_movement_foster_1) %in% c("movement_rate_to_kin"
                                                                                                                              ,"movement_rate_to_group"
                                                                                                                              ,"variance"))]
                                       ,rate_care_day_movement_foster_2[,-which(names(rate_care_day_movement_foster_2) %in% c("movement_rate_to_kin"
                                                                                                                              ,"movement_rate_to_group"
                                                                                                                              ,"variance"))])
names(rate_care_day_movement_foster) <- c("fiscal_yr", "years_in_care", "movement_rate", "lcl", "ucl")

rate_care_day_movement_kin <- rbind(rate_care_day_movement_kin_0[,-which(names(rate_care_day_movement_kin_0) %in% c("movement_rate_to_foster"
                                                                                                                    ,"movement_rate_to_group"
                                                                                                                    ,"variance"))]
                                    ,rate_care_day_movement_kin_1[,-which(names(rate_care_day_movement_kin_1) %in% c("movement_rate_to_foster"
                                                                                                                     ,"movement_rate_to_group"
                                                                                                                     ,"variance"))]
                                    ,rate_care_day_movement_kin_2[,-which(names(rate_care_day_movement_kin_2) %in% c("movement_rate_to_foster"
                                                                                                                     ,"movement_rate_to_group"
                                                                                                                     ,"variance"))])
names(rate_care_day_movement_kin) <- c("fiscal_yr", "years_in_care", "movement_rate", "lcl", "ucl")

rate_care_day_movement_kin$type <- "Transition to Kin"
rate_care_day_movement_group$type <- "Transition to Group"
rate_care_day_movement_foster$type <- "Transition to Foster"

rate_care_day_movement_tx <- rbind(rate_care_day_movement_kin, rate_care_day_movement_group, rate_care_day_movement_foster)



names(rate_care_day_movement_tx) <- c("fiscal_yr"
                                      ,"years_in_care"
                                      ,"Movement Rate"
                                      ,"Lower Limit"
                                      ,"Upper Limit"
                                      ,"type")

#write.csv(rate_care_day_movement_tx, "rate_care_day_movement_tx.csv")



rate_care_day_movement_tx_long <- melt(rate_care_day_movement_tx, id.vars = c("fiscal_yr", "years_in_care", "type"))

rate_care_day_movement_tx_long$years_in_care <- as.factor(rate_care_day_movement_tx_long$years_in_care)

levels(rate_care_day_movement_tx_long$years_in_care) <- c("First Year Care Days", "Second Year Care Days", "Third Year Care Days")

library(stringr)
library(dplyr)
library(reshape2)

move_to_kin <- filter(rate_care_day_movement_tx_long, str_detect(type, "Kin"))
move_to_kin <- dcast(move_to_kin, formula = fiscal_yr + years_in_care + type ~ variable, value.var = "value")
names(move_to_kin) <- make.names(names(move_to_kin))

move_to_kin_graph <- ggplot(move_to_kin, aes(x = fiscal_yr, y = Movement.Rate, ymin = Lower.Limit, ymax = Upper.Limit)) +
    geom_point(size=3) + 
    geom_smooth(fill=NA, color = portal_colors[8], size = 1, method="lm") +
    geom_line(aes(y = Lower.Limit), color = poc_colors[1]) +
    geom_line(aes(y = Upper.Limit), color = poc_colors[3]) +
    labs(title = "Transition to Kin Care (per 100,000 Care Days)", x = "", y = "") + 
    facet_wrap(~ years_in_care) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA))

ggsave("movement_to_kin_v2.pdf", move_to_kin_graph, width = 12, height = 6)

# graph 12
# the data here is incorrect

library(pocr)
library(RODBC)

con <- odbcConnect("POC")
sp_sib_plcmt <- sqlQuery(con, "select * from ca_ods.prtl.sibling_care_days_placed_tghr")
sp_sib_plcmt$rate <- sp_sib_plcmt$care_days_tghr/sp_sib_plcmt$care_days
sp_sib_plcmt <- sp_sib_plcmt[order(sp_sib_plcmt$fiscal_yr),] 

n <- length(sp_sib_plcmt$care_days_tghr)
mean_rate <- mean(sp_sib_plcmt$rate[1:n-1])
sp_sib_plcmt$variance <- abs(sp_sib_plcmt$rate - mean_rate)
mean_variance <- mean(sp_sib_plcmt$variance)

sp_sib_plcmt$lcl <- mean_rate-(mean_variance*qnorm(0.997))
sp_sib_plcmt$ucl <- mean_rate+(mean_variance*qnorm(0.997))

#write.csv(sp_sib_plcmt, "rate_sib_plcmt.csv")

sp_sib_plcmt_long <- melt(sp_sib_plcmt[,c("fiscal_yr", "rate", "lcl", "ucl")], id.vars = "fiscal_yr")


levels(sp_sib_plcmt_long$variable) <- c("Sibling Placement Rate"
                                        ,"Lower Limit"
                                        ,"Upper Limit")


pR_SP <-  

R_SP_14 <- sp_sib_plcmt[sp_sib_plcmt$fiscal_yr==2014, ][[4]]
R_SP_13 <- sp_sib_plcmt[sp_sib_plcmt$fiscal_yr==2013, ][[4]]


ggsave(file="graph12_sibling_placenment_v2.pdf") 


# graph 13

library(pocr)
library(RODBC)

con <- odbcConnect("POC")
library(Hmisc)

setwd("C:/Users/oieeri26/Desktop/GitHub/annual_report")

sp_exits_planning <- sqlQuery(con, "select
                              count(id_prsn) kids 
                              ,sum(iif(plan_cntr is null, 0, 1)) kids_with_plans
                              ,sum(iif(plan_cntr is null, 0, 1))*1.0/count(id_prsn) prp_kids_with_plans
                              ,cohort_fy as fiscal_yr
                              from ##placement_prep17_5
                              group by 
                              cohort_fy
                              order by
                              cohort_fy")

sp_exits_planning_conf <- binconf(x=sp_exits_planning$kids_with_plans, n=sp_exits_planning$kids)

conf <- cbind(as.data.frame(sp_exits_planning_conf[,1]), as.data.frame(sp_exits_planning_conf[,2]), as.data.frame(sp_exits_planning_conf[,3]))

names(conf) <- c("planning_rate", "lower", "upper")

sp_exits_planning <- cbind(sp_exits_planning, conf)

write.csv(sp_exits_planning, "rate_exits_planning_v2.csv")

pR_ATP <- ggplot(sp_exits_planning, aes(y=planning_rate, x=fiscal_yr)) +
    geom_bar(position="dodge", stat="identity",  fill=poc_colors[1]) +
    geom_errorbar(aes(ymin=lower, ymax=upper), position="dodge", width=0.25, size=1) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Percent of Adult-Transition Planning", y = "", x = "") + 
    theme_bw() 

R_ATP_14 <- sp_exits_planning[sp_exits_planning$fiscal_yr==2014, ][[4]]
R_ATP_13 <- sp_exits_planning[sp_exits_planning$fiscal_yr==2013, ][[4]]

ggsave(file="graph_13_adult_transition_plot_v2.pdf")

# Graph 14

require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
# con <- odbcConnect("Test Annie")
con <- odbcConnect("test_annie")

library(Hmisc)

grade_three_lit <- sqlQuery(con, "SELECT
                            year
                            ,cd_student_type
                            ,i0
                            ,i1
                            ,passed
                            ,fl_disability
                            FROM test_annie.prtl_lit3_perc
                            where 
                            cd_student_type = 3;")

dodge <- position_dodge(width=0.9)

grade_three_lit$fl_disability <- as.factor(grade_three_lit$fl_disability)

levels(grade_three_lit$fl_disability) <- c("Foster Children (Non-Disabled)"
                                           ,"Foster Children (Disabled)")
grade_three_lit$year <- format(grade_three_lit$year, "%Y")

grade_three_lit$i0 <- grade_three_lit$i0/100
grade_three_lit$i1 <- grade_three_lit$i1/100
grade_three_lit$passed <- grade_three_lit$passed/100

pR_L_3 <- ggplot(grade_three_lit, aes(y = passed, x = year, fill = fl_disability)) +
  geom_bar(position=position_dodge(width = 0.9, height = NULL), stat = "identity") +
  geom_errorbar(aes(ymin=i0, ymax=i1), width = 0.4, position=position_dodge(width = 0.9, height = NULL), size = 1) +
  scale_fill_manual(values = poc_colors[1:2], name = "") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Percent of Third Graders Meeting Literacy Standards", x = "", y = "") + 
  theme_bw() +
  theme(legend.position="bottom")

R_L3_08 <- grade_three_lit[grade_three_lit$year==2008 & grade_three_lit$fl_disability=="Foster Children (Non-Disabled)", ][[5]]
R_L3_07 <- grade_three_lit[grade_three_lit$year==2007 & grade_three_lit$fl_disability=="Foster Children (Non-Disabled)", ][[5]]

ggsave(file="graph_14_literacy_percent_v2.pdf")


