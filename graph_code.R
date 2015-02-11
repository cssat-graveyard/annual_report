require(RODBC)
require(pocr)
require(sqldf)
con <- odbcConnect("test_annie")
sp_rate_referral <- sqlQuery(con, "call sp_rate_referral('0','0','0')")
sp_rate_referral_clean <- cr_clean(sp_rate_referral)

get.rows <- sp_rate_referral_clean$date >= as.Date("2009-07-01") & 
  sp_rate_referral_clean$date <= as.Date("2014-07-01") 
sp_rate_referral_clean <- sp_rate_referral_clean[get.rows, ] 

ts1 <- ts(sp_rate_referral_clean$referral.rate, start=c(2009,2), frequency=12)
stl1 <- stl(ts1, "per")$time.series
sp_rate_referral_clean$type <- "Observed"
stl_dat <- sp_rate_referral_clean[,c("date", "cnt.referrals", "tot.pop", "referral.rate", "type")]
stl_dat$type <- "Deseasonalized Trend"
stl_dat$referral.rate <- stl1[,2]
sp_rate_referral_clean <- rbind(sp_rate_referral_clean, stl_dat)

sp_rate_referral_clean_2014 <- subset(sp_rate_referral_clean, date > as.Date("2013-06-01") & date <= as.Date("2014-06-01"))
sp_rate_referral_clean_2013 <- subset(sp_rate_referral_clean, date > as.Date("2012-06-01") & date <= as.Date("2013-06-01"))


R_I_14 <- mean(sp_rate_referral_clean_2014$referral.rate)
R_I_13 <- mean(sp_rate_referral_clean_2013$referral.rate)

pR_I <- ggplot(sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",]
               , aes(x=date
                     ,y=referral.rate
                     #,group=type
                     #,colour=type
               )) + 
  geom_line(size=1, colour=poc_colors[2]) +
  #geom_smooth(colour=poc_colors[3], fill=NA, size=2) + 
  xlab("") +
  ylab("General Rate of Intakes") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  
  theme_bw()

ggsave(file="p20_graph1.pdf")

pR_I_counts <- ggplot(sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",]
                      , aes(x=date
                            ,y=cnt.referrals
                            #,group=type
                            #,colour=type
                      )) + 
  #geom_line(size=1, colour=poc_colors[2]) +
  geom_smooth(colour=poc_colors[3], fill=NA, size=2) + 
  xlab("") +
  ylab("Count of Intakes") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  theme_bw()

ggsave(file="p21_graph1.pdf")

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
pR_I_prpgt3 <- ggplot(dat
       , aes(x=start_date
                                   ,y=prp_gt3
                                           #,group=type
                                           ,colour=poc_colors[3]
                                   )) + 
  #geom_line(size=1, colour=poc_colors[2]) +
  geom_smooth(colour=poc_colors[3], fill=NA, size=2) + 
  xlab("") +
  ylab("Proportion of Intakes with More than 3 Prior Intakes") + 
  theme_bw()

ggsave(file="p22_graph1.pdf")


require(pocr)
con <- odbcConnect("Test Annie")
sp_rate_referral_scrn_in <- sqlQuery(con, "call sp_rate_referral_scrn_in('0','0','0')")
sp_rate_referral_scrn_in_clean <- cr_clean(sp_rate_referral_scrn_in)

get.rows <- sp_rate_referral_scrn_in_clean$date >= as.Date("2009-02-01") & 
  sp_rate_referral_scrn_in_clean$date <= as.Date("2014-07-01") 
sp_rate_referral_scrn_in_clean <- sp_rate_referral_scrn_in_clean[get.rows, ] 

ts1 <- ts(sp_rate_referral_scrn_in_clean$referral.rate, start=c(2009,2), frequency=12)
stl1 <- stl(ts1, "per")$time.series
sp_rate_referral_scrn_in_clean$type <- "Observed"
stl_dat <- sp_rate_referral_scrn_in_clean[,c("date", "cnt.referrals", "tot.pop", "referral.rate", "type")]
stl_dat$type <- "Deseasonalized Trend"
stl_dat$referral.rate <- stl1[,2]
sp_rate_referral_scrn_in_clean <- rbind(sp_rate_referral_scrn_in_clean, stl_dat)


pR_S <- ggplot(sp_rate_referral_scrn_in_clean[sp_rate_referral_scrn_in_clean$type=="Deseasonalized Trend",]
               ,aes(x=date
                    ,y=referral.rate
                    #,group=type
                    #,colour=type
               )) + 
  geom_line(size=2, colour=poc_colors[2]) +
  xlab("") +
  ylab("Screened-In Intakes Per 1,000 Intakes") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  theme_bw()

sp_rate_referral_scrn_in_clean_2014 <- subset(sp_rate_referral_scrn_in_clean, date > as.Date("2013-06-01") & date <= as.Date("2014-06-01"))
sp_rate_referral_scrn_in_clean_2013 <- subset(sp_rate_referral_scrn_in_clean, date > as.Date("2012-06-01") & date <= as.Date("2013-06-01"))

R_S_14 <- round(mean(sp_rate_referral_scrn_in_clean_2014$referral.rate), 0)
R_S_13 <- round(mean(sp_rate_referral_scrn_in_clean_2013$referral.rate), 0)

ggsave(file="p23_graph1.pdf")

require(pocr)
con <- odbcConnect("Test Annie")
pR_S_counts <- ggplot(sp_rate_referral_scrn_in_clean[sp_rate_referral_scrn_in_clean$type=="Deseasonalized Trend",]
                      , aes(x=date
                            ,y=cnt.referrals
                            #,group=type
                            #,colour=type
                      )) + 
  #geom_line(size=1, colour=poc_colors[2]) +
  geom_smooth(colour=poc_colors[3], fill=NA, size=2) + 
  xlab("") +
  ylab("Count of Screened-In Intakes") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  theme_bw()

ggsave(file="p23_graph2.pdf")

con <- odbcConnect("Test Annie")
sp_rate_placement <- sqlQuery(con, "call sp_rate_placement('0','0','0')")
sp_rate_placement_clean <- cr_clean(sp_rate_placement)

get.rows <- sp_rate_placement_clean$cohort.date >= as.Date("2009-02-01") & 
  sp_rate_placement_clean$cohort.date <= as.Date("2014-07-01") 
sp_rate_placement_clean <- sp_rate_placement_clean[get.rows, ] 


ts1 <- ts(sp_rate_placement_clean$placement.rate, start=c(2009,2), frequency=12)
stl1 <- stl(ts1, "per")$time.series
sp_rate_placement_clean$type <- "Observed"
stl_dat <- sp_rate_placement_clean[,c("cohort.date", "cnt.households.w.plcm", "cnt.referrals.u18", "placement.rate", "type")]
stl_dat$type <- "Deseasonalized Trend"
stl_dat$placement.rate <- stl1[,2]
sp_rate_placement_clean <- rbind(sp_rate_placement_clean, stl_dat)


pR_P <- ggplot(sp_rate_placement_clean[sp_rate_placement_clean$type=="Deseasonalized Trend",]
               , aes(x=cohort.date
                     ,y=placement.rate
                     #,group=type
                     #,colour=type
               )) + 
  geom_line(size=2, colour=poc_colors[2]) +
  xlab("") +
  ylab("Placements Per 1,000 Screened-In Intakes") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  theme_bw()

ggsave(file="p24_graph1.pdf")

con <- odbcConnect("Test Annie")
pR_P_counts <- ggplot(sp_rate_placement_clean[sp_rate_placement_clean$type=="Deseasonalized Trend",]
                      , aes(x=cohort.date
                            ,y=cnt.households.w.plcm
                            #,group=type
                            #,colour=type
                      )) + 
  #geom_line(size=2, colour=poc_colors[2]) +
  geom_smooth(size=2, colour=poc_colors[3], fill=NA) +
  xlab("") +
  ylab("Count of Placements") + 
  scale_color_manual(labels = c("Deseasonalized Trend"
                                ,"Observations")
                     ,values=c(poc_colors[3:4])
                     ,guide=guide_legend(title= "")
  ) +
  theme_bw()

sp_rate_placement_clean_2014 <- subset(sp_rate_placement_clean, cohort.date > as.Date("2013-06-01") & cohort.date <= as.Date("2014-06-01"))
sp_rate_placement_clean_2013 <- subset(sp_rate_placement_clean, cohort.date > as.Date("2012-06-01") & cohort.date <= as.Date("2013-06-01"))


R_P_14 <- mean(sp_rate_placement_clean_2014$placement.rate)
R_P_13 <- mean(sp_rate_placement_clean_2013$placement.rate)

require(pocr)
con <- odbcConnect("Test Annie")
sp_rate_referrals_order <- sqlQuery(con, "call sp_rate_referrals_order('0','0','1,2,3')")
sp_rate_referrals_order_clean <- cr_clean(sp_rate_referrals_order)

get.rows <- sp_rate_referrals_order_clean$date >= as.Date("2009-02-01") & 
  sp_rate_referrals_order_clean$date <= as.Date("2014-07-01") 
sp_rate_referrals_order_clean <- sp_rate_referrals_order_clean[get.rows, ] 

piR_I <- ggplot(sp_rate_referrals_order_clean, aes(x=date
                                                   ,y=referral.rate
                                                   ,group=order
                                                   ,colour=order)) + 
  #geom_point(size=2.5) +
  geom_smooth(method="lm") +
  xlab("") +
  ylab("Intakes Per 1,000 Households") + 
  scale_color_manual(values=c(poc_colors[1:3])
                     ,guide=guide_legend(title= "Order of Intake")
  ) +
  theme_bw() +
  theme(legend.position="bottom")


sp_rate_referrals_order1_clean_2014 <- subset(sp_rate_referrals_order_clean, date > as.Date("2013-06-01") & 
                                                date <= as.Date("2014-06-01") &
                                                cd.order == 1)
sp_rate_referrals_order1_clean_2013 <- subset(sp_rate_referrals_order_clean, date > as.Date("2012-06-01") & 
                                                date <= as.Date("2013-06-01") &
                                                cd.order == 1)


sp_rate_referrals_order2_clean_2014 <- subset(sp_rate_referrals_order_clean, date > as.Date("2013-06-01") & 
                                                date <= as.Date("2014-06-01") &
                                                cd.order == 2)
sp_rate_referrals_order2_clean_2013 <- subset(sp_rate_referrals_order_clean, date > as.Date("2012-06-01") & 
                                                date <= as.Date("2013-06-01") &
                                                cd.order == 2)

sp_rate_referrals_order3_clean_2014 <- subset(sp_rate_referrals_order_clean, date > as.Date("2013-06-01") & 
                                                date <= as.Date("2014-06-01") &
                                                cd.order == 3)
sp_rate_referrals_order3_clean_2013 <- subset(sp_rate_referrals_order_clean, date > as.Date("2012-06-01") & 
                                                date <= as.Date("2013-06-01") &
                                                cd.order == 3)


R_I_14.1 <- mean(sp_rate_referrals_order1_clean_2014$referral.rate)
R_I_13.1 <- mean(sp_rate_referrals_order1_clean_2013$referral.rate)

R_I_14.2 <- mean(sp_rate_referrals_order2_clean_2014$referral.rate)
R_I_13.2 <- mean(sp_rate_referrals_order2_clean_2013$referral.rate)

R_I_14.3 <- mean(sp_rate_referrals_order3_clean_2014$referral.rate)
R_I_13.3 <- mean(sp_rate_referrals_order3_clean_2013$referral.rate)

ggsave(file="p20_graph2.pdf")

require(pocr)
con <- odbcConnect("Test Annie")
sp_rate_referrals_scrn_in_order <- sqlQuery(con
                                            ,"call sp_rate_referrals_scrn_in_order('0','0','1,2,3')")
sp_rate_referrals_scrn_in_order_clean <- cr_clean(sp_rate_referrals_scrn_in_order)

get.rows <- sp_rate_referrals_scrn_in_order_clean$date >= as.Date("2009-02-01") & 
  sp_rate_referrals_scrn_in_order_clean$date <= as.Date("2014-07-01") 
sp_rate_referrals_scrn_in_order_clean <- sp_rate_referrals_scrn_in_order_clean[get.rows, ] 

piR_S <- ggplot(sp_rate_referrals_scrn_in_order_clean, aes(x=date
                                                           ,y=referral.rate
                                                           ,group=order
                                                           ,colour=order)) + 
  #geom_point(size=2.5) +
  geom_smooth(method="lm") +
  xlab("") +
  ylab("Screenings Per 1,000 Intakes") + 
  scale_color_manual(values=c(poc_colors[1:3])
                     ,guide=guide_legend(title= "Order of Screening")
  ) +
  theme_bw() +
  theme(legend.position="bottom")


sp_rate_referrals_scrn_in_order1_clean_2014 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2013-06-01") & 
                                                        date <= as.Date("2014-06-01") &
                                                        cd.order == 1)
sp_rate_referrals_scrn_in_order1_clean_2013 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2012-06-01") & 
                                                        date <= as.Date("2013-06-01") &
                                                        cd.order == 1)


sp_rate_referrals_scrn_in_order2_clean_2014 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2013-06-01") & 
                                                        date <= as.Date("2014-06-01") &
                                                        cd.order == 2)
sp_rate_referrals_scrn_in_order2_clean_2013 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2012-06-01") & 
                                                        date <= as.Date("2013-06-01") &
                                                        cd.order == 2)

sp_rate_referrals_scrn_in_order3_clean_2014 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2013-06-01") & 
                                                        date <= as.Date("2014-06-01") &
                                                        cd.order == 3)
sp_rate_referrals_scrn_in_order3_clean_2013 <- subset(sp_rate_referrals_scrn_in_order_clean, date > as.Date("2012-06-01") & 
                                                        date <= as.Date("2013-06-01") &
                                                        cd.order == 3)


R_S_14.1 <- mean(sp_rate_referrals_scrn_in_order1_clean_2014$referral.rate)
R_S_13.1 <- mean(sp_rate_referrals_scrn_in_order1_clean_2013$referral.rate)

R_S_14.2 <- mean(sp_rate_referrals_scrn_in_order2_clean_2014$referral.rate)
R_S_13.2 <- mean(sp_rate_referrals_scrn_in_order2_clean_2013$referral.rate)

R_S_14.3 <- mean(sp_rate_referrals_scrn_in_order3_clean_2014$referral.rate)
R_S_13.3 <- mean(sp_rate_referrals_scrn_in_order3_clean_2013$referral.rate)

ggsave(file="p22_graph2.pdf")

require(pocr)
con <- odbcConnect("Test Annie")
sp_rate_placement_order <- sqlQuery(con,"call sp_rate_placement_order('0','0','1,2,3')")
sp_rate_placement_order_clean <- cr_clean(sp_rate_placement_order)

get.rows <- sp_rate_placement_order_clean$cohort.date >= as.Date("2009-02-01") & 
  sp_rate_placement_order_clean$cohort.date <= as.Date("2014-07-01") 
sp_rate_placement_order_clean <- sp_rate_placement_order_clean[get.rows, ] 

piR_P <- ggplot(sp_rate_placement_order_clean, aes(x=cohort.date
                                                   ,y=placement.rate
                                                   ,group=order
                                                   ,colour=order)) + 
  #geom_point(size=2.5) +
  geom_smooth(method="lm") +
  xlab("") +
  ylab("Placements Per 1,000 Screened-In Intakes") + 
  scale_color_manual(labels = c("First Order"
                                ,"Second Order"
                                ,"Third Order")
                     ,values=c(poc_colors[1:3])
                     ,guide=guide_legend(title= "Order of Placement")
  ) +
  theme_bw() +
  theme(legend.position="bottom")


sp_rate_placement_order1_clean2014 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2013-06-01") & 
                                               cohort.date <= as.Date("2014-06-01") &
                                               cd.order == 1)
sp_rate_placement_order1_clean2013 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2012-06-01") & 
                                               cohort.date <= as.Date("2013-06-01") &
                                               cd.order == 1)


sp_rate_placement_order2_clean2014 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2013-06-01") & 
                                               cohort.date <= as.Date("2014-06-01") &
                                               cd.order == 2)
sp_rate_placement_order2_clean2013 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2012-06-01") & 
                                               cohort.date <= as.Date("2013-06-01") &
                                               cd.order == 2)

sp_rate_placement_order3_clean2014 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2013-06-01") & 
                                               cohort.date <= as.Date("2014-06-01") &
                                               cd.order == 3)
sp_rate_placement_order3_clean2013 <- subset(sp_rate_placement_order_clean, cohort.date > as.Date("2012-06-01") & 
                                               cohort.date <= as.Date("2013-06-01") &
                                               cd.order == 3)


R_P_14.1 <- mean(sp_rate_placement_order1_clean2014$placement.rate)
R_P_13.1 <- mean(sp_rate_placement_order1_clean2013$placement.rate)

R_P_14.2 <- mean(sp_rate_placement_order2_clean2014$placement.rate)
R_P_13.2 <- mean(sp_rate_placement_order2_clean2013$placement.rate)

R_P_14.3 <- mean(sp_rate_placement_order3_clean2014$placement.rate)
R_P_13.3 <- mean(sp_rate_placement_order3_clean2013$placement.rate)

ggsave(file="p25_graph1.pdf")

require(pocr)
con <- odbcConnect("Test Annie")
sp_rate_care_day_maltreatment <- sqlQuery(con,"call sp_rate_care_day_maltreatment('0')")
sp_rate_care_day_maltreatment_clean <- cr_clean(sp_rate_care_day_maltreatment)

#hard coding 2014 in
sp_rate_care_day_maltreatment_clean <- rbind(sp_rate_care_day_maltreatment_clean, c(2014, 196, 3099673, 6.323247646))

n <- length(sp_rate_care_day_maltreatment_clean$care.day.incident.rate)
mean_rate <- mean(sp_rate_care_day_maltreatment_clean$care.day.incident.rate[1:n-1])
sp_rate_care_day_maltreatment_clean$variance <- abs(sp_rate_care_day_maltreatment_clean$care.day.incident.rate - mean_rate)
mean_variance <- mean(sp_rate_care_day_maltreatment_clean$variance)

sp_rate_care_day_maltreatment_clean$lcl <- mean_rate-(mean_variance*qnorm(0.997))
sp_rate_care_day_maltreatment_clean$ucl <- mean_rate+(mean_variance*qnorm(0.997))

dat1 <- sp_rate_care_day_maltreatment_clean[,c("fiscal.yr", "care.day.incident.rate")]
names(dat1) <- c("year", "value")
dat1$var <- "Care Day Incident Rate" 
dat2 <- sp_rate_care_day_maltreatment_clean[,c("fiscal.yr", "ucl")]
names(dat2) <- c("year", "value")
dat2$var <- "Upper Limit" 
dat3 <- sp_rate_care_day_maltreatment_clean[,c("fiscal.yr", "lcl")]
names(dat3) <- c("year", "value")
dat3$var <- "Lower Limit" 

dat <- rbind(dat1, dat2, dat3)

pR_F <- ggplot(dat, aes(x=year
                        ,y=value
                        ,group=var
                        ,colour=var)) + 
  geom_line(size=1) +
  xlab("") +
  ylab("Maltreatment Substantiations Per 100,000 Care Days") + 
  scale_color_manual(labels = c("Observed Rate"
                                ,"Lower Limit"
                                ,"Upper Limit")
                     ,name = ""
                     ,values=c(poc_colors[c(1,3,2)])
  ) +
  theme_bw() +
  theme(legend.position="bottom")

R_F_14 <- dat[dat$year==2014, ]$value[1]
R_F_13 <- dat[dat$year==2013, ]$value[1]

ggsave(file="p28_graph1.pdf")

require(pocr)
con <- odbcConnect("POC")

rate_care_day_movement <- sqlQuery(con,"select
                                   fiscal_yr
                                   ,years_in_care
                                   ,placement_moves*100000.0/care_days movement_rate
                                   from ca_ods.base.placement_care_days_mobility
                                   where exclude_7day = 1
                                   and exclude_nondcfs = 1
                                   and exclude_trh = 1
                                   and age_yrs_removal = -99
                                   and age_yrs_exit = -99 
                                   and county_cd = 0
                                   and cd_race = 0 
                                   and years_in_care <= 2")


rate_care_day_movement_0 <- subset(rate_care_day_movement, years_in_care == 0)
mean_rate_0 <- mean(rate_care_day_movement_0$movement_rate[1:n-1])
rate_care_day_movement_0$variance <- abs(rate_care_day_movement_0$movement_rate - mean_rate_0)
mean_variance_0 <- mean(rate_care_day_movement_0$variance)
rate_care_day_movement_0$lcl <- mean_rate_0-(mean_variance_0*qnorm(0.997))
rate_care_day_movement_0$ucl <- mean_rate_0+(mean_variance_0*qnorm(0.997))

rate_care_day_movement_1 <- subset(rate_care_day_movement, years_in_care == 1)
mean_rate_1 <- mean(rate_care_day_movement_1$movement_rate[1:n-1])
rate_care_day_movement_1$variance <- abs(rate_care_day_movement_1$movement_rate - mean_rate_1)
mean_variance_1 <- mean(rate_care_day_movement_1$variance)
rate_care_day_movement_1$lcl <- mean_rate_1-(mean_variance_1*qnorm(0.997))
rate_care_day_movement_1$ucl <- mean_rate_1+(mean_variance_1*qnorm(0.997))

rate_care_day_movement_2 <- subset(rate_care_day_movement, years_in_care == 2)
mean_rate_2 <- mean(rate_care_day_movement_2$movement_rate[1:n-1])
rate_care_day_movement_2$variance <- abs(rate_care_day_movement_2$movement_rate - mean_rate_2)
mean_variance_2 <- mean(rate_care_day_movement_2$variance)
rate_care_day_movement_2$lcl <- mean_rate_2-(mean_variance_2*qnorm(0.997))
rate_care_day_movement_2$ucl <- mean_rate_2+(mean_variance_2*qnorm(0.997))

rate_care_day_movement <- rbind(rate_care_day_movement_2[,-which(names(rate_care_day_movement_2) %in% c("variance"))]
                                ,rate_care_day_movement_1[,-which(names(rate_care_day_movement_1) %in% c("variance"))]
                                ,rate_care_day_movement_0[,-which(names(rate_care_day_movement_0) %in% c("variance"))])

names(rate_care_day_movement) <- c("fiscal_yr"
                                   ,"years_in_care"
                                   ,"Movement Rate"
                                   ,"Lower Limit"
                                   ,"Upper Limit")

#write.csv(rate_care_day_movement, "rate_care_day_movement.csv")

rate_care_day_movement_long <- melt(rate_care_day_movement, id.vars = c("fiscal_yr", "years_in_care"))

rate_care_day_movement_long$years_in_care <- as.factor(rate_care_day_movement_long$years_in_care)

levels(rate_care_day_movement_long$years_in_care) <- c("First Year Care Days", "Second Year Care Days", "Third Year Care Days")

pdR_M <- ggplot(rate_care_day_movement_long[rate_care_day_movement_long$variable!="Movement Rate",]
                ,aes(x=fiscal_yr
                     ,y=value
                     ,group=variable
                     ,colour=variable)) + 
  geom_line(size=1) +
  geom_point(data=rate_care_day_movement_long[rate_care_day_movement_long$variable=="Movement Rate",]
             ,aes(x=fiscal_yr
                  ,y=value)
             ,size=3) + 
  geom_smooth(data=rate_care_day_movement_long[rate_care_day_movement_long$variable=="Movement Rate",]
              ,aes(x=fiscal_yr
                   ,y=value)
              ,fill=NA
              ,method="lm") +
  xlab("") +
  ylab("Movement Rate Per 100,000 Care Days (Duration-Specific)") + 
  scale_color_manual(name="", values=c(poc_colors[c(1,4,2)])) +
  facet_wrap(~years_in_care) +
  theme_bw() +
  theme(legend.position="bottom")

R_M_14.1 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2014 & rate_care_day_movement$years_in_care==0, ][[3]]
R_M_13.1 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2013 & rate_care_day_movement$years_in_care==0, ][[3]]

R_M_14.2 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2014 & rate_care_day_movement$years_in_care==1, ][[3]]
R_M_13.2 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2013 & rate_care_day_movement$years_in_care==1, ][[3]]

R_M_14.3 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2014 & rate_care_day_movement$years_in_care==2, ][[3]]
R_M_13.3 <- rate_care_day_movement[rate_care_day_movement$fiscal_yr==2013 & rate_care_day_movement$years_in_care==2, ][[3]]

ggsave(file="p29_graph1.pdf")

require(pocr)
con <- odbcConnect("POC")
rate_care_day_movement_tx <- sqlQuery(con,"select
                                      fiscal_yr
                                      ,years_in_care
                                      ,kin_cnt*100000.0/care_days movement_rate_to_kin
                                      ,foster_cnt*100000.0/care_days movement_rate_to_foster
                                      ,group_cnt*100000.0/care_days movement_rate_to_group 
                                      from ca_ods.base.placement_care_days_mobility
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

pdR_Mtx <- ggplot(rate_care_day_movement_tx_long[rate_care_day_movement_tx_long$variable!="Movement Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=variable
                       ,colour=variable)) + 
  geom_line(size=1) +
  geom_point(data=rate_care_day_movement_tx_long[rate_care_day_movement_tx_long$variable=="Movement Rate",]
             ,aes(x=fiscal_yr
                  ,y=value)
             ,size=3) + 
  geom_smooth(data=rate_care_day_movement_tx_long[rate_care_day_movement_tx_long$variable=="Movement Rate",]
              ,aes(x=fiscal_yr
                   ,y=value)
              ,fill=NA
              ,method="lm") +
  xlab("") +
  ylab("Movement Rate Per 100,000 Care Days (Transition-Duration-Specific)") + 
  scale_color_manual(name="", values=c(poc_colors[c(1,4,2)])) +
  facet_grid(type~years_in_care, scales = "free_y") +
  theme_bw() +
  theme(legend.position="bottom")

library(stringr)
library(dplyr)
library(reshape2)

move_to_kin <- filter(rate_care_day_movement_tx_long, str_detect(type, "Kin"))
move_to_kin <- dcast(move_to_kin, formula = fiscal_yr + years_in_care + type ~ variable, value.var = "value")
names(move_to_kin) <- make.names(names(move_to_kin))

move_to_kin_graph <- ggplot(move_to_kin, aes(x = fiscal_yr, y = Movement.Rate,
                        ymin = Lower.Limit, ymax = Upper.Limit)) +
    geom_point(size=3) + 
    geom_smooth(fill=NA,
                color = portal_colors[8],
                size = 1,
                ,method="lm") +
    geom_line(aes(y = Lower.Limit), color = poc_colors[1]) +
    geom_line(aes(y = Upper.Limit), color = poc_colors[3]) +
    xlab("") +
    ylab("Transition to Kin Care\n(per 100,000 Care Days)") + 
    facet_wrap(~ years_in_care) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA))

ggsave("movement_to_kin.pdf", move_to_kin_graph, width = 12, height = 6)

R_Mk_14.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==0 & 
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]
R_Mk_13.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==0 &
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]

R_Mk_14.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]
R_Mk_13.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]

R_Mk_14.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]
R_Mk_13.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Kin", ][[3]]

R_Mf_14.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==0 & 
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]
R_Mf_13.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==0 &
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]

R_Mf_14.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]
R_Mf_13.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]

R_Mf_14.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]
R_Mf_13.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Foster", ][[3]]


R_Mg_14.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==0 & 
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]
R_Mg_13.1 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==0 &
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]

R_Mg_14.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]
R_Mg_13.2 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==1 &
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]

R_Mg_14.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2014 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]
R_Mg_13.3 <- rate_care_day_movement_tx[rate_care_day_movement_tx$fiscal_yr==2013 & 
                                         rate_care_day_movement_tx$years_in_care==2 &
                                         rate_care_day_movement_tx$type == "Transition to Group", ][[3]]


ggsave(file="p29.1_graph1.pdf")

require(pocr)
con <- odbcConnect("POC")

rate_care_day_otr <- sqlQuery(con,"select
                              fiscal_yr
                              ,age_in_fy
                              ,care_days_otr*1.0/care_days otr_rate
                              from ca_ods.base.placement_care_days_otr
                              where excludes_7day = 1
                              and excludes_nondcfs = 1
                              and excludes_trh = 1
                              and cd_cnty = 0
                              and age_in_fy >= 13
                              order by fiscal_yr, age_in_fy")


rate_care_day_otr_13 <- subset(rate_care_day_otr, age_in_fy == 13)
mean_rate_13 <- mean(rate_care_day_otr_13$otr_rate[1:n-1])
rate_care_day_otr_13$variance <- abs(rate_care_day_otr_13$otr_rate - mean_rate_13)
mean_variance_13 <- mean(rate_care_day_otr_13$variance)
rate_care_day_otr_13$lcl <- mean_rate_13-(mean_variance_13*qnorm(0.997))
rate_care_day_otr_13$ucl <- mean_rate_13+(mean_variance_13*qnorm(0.997))

rate_care_day_otr_14 <- subset(rate_care_day_otr, age_in_fy == 14)
mean_rate_14 <- mean(rate_care_day_otr_14$otr_rate[1:n-1])
rate_care_day_otr_14$variance <- abs(rate_care_day_otr_14$otr_rate - mean_rate_14)
mean_variance_14 <- mean(rate_care_day_otr_14$variance)
rate_care_day_otr_14$lcl <- mean_rate_14-(mean_variance_14*qnorm(0.997))
rate_care_day_otr_14$ucl <- mean_rate_14+(mean_variance_14*qnorm(0.997))

rate_care_day_otr_15 <- subset(rate_care_day_otr, age_in_fy == 15)
mean_rate_15 <- mean(rate_care_day_otr_15$otr_rate[1:n-1])
rate_care_day_otr_15$variance <- abs(rate_care_day_otr_15$otr_rate - mean_rate_15)
mean_variance_15 <- mean(rate_care_day_otr_15$variance)
rate_care_day_otr_15$lcl <- mean_rate_15-(mean_variance_15*qnorm(0.997))
rate_care_day_otr_15$ucl <- mean_rate_15+(mean_variance_15*qnorm(0.997))

rate_care_day_otr_16 <- subset(rate_care_day_otr, age_in_fy == 16)
mean_rate_16 <- mean(rate_care_day_otr_16$otr_rate[1:n-1])
rate_care_day_otr_16$variance <- abs(rate_care_day_otr_16$otr_rate - mean_rate_16)
mean_variance_16 <- mean(rate_care_day_otr_16$variance)
rate_care_day_otr_16$lcl <- mean_rate_16-(mean_variance_16*qnorm(0.997))
rate_care_day_otr_16$ucl <- mean_rate_16+(mean_variance_16*qnorm(0.997))

rate_care_day_otr_17 <- subset(rate_care_day_otr, age_in_fy == 17)
mean_rate_17 <- mean(rate_care_day_otr_17$otr_rate[1:n-1])
rate_care_day_otr_17$variance <- abs(rate_care_day_otr_17$otr_rate - mean_rate_17)
mean_variance_17 <- mean(rate_care_day_otr_17$variance)
rate_care_day_otr_17$lcl <- mean_rate_17-(mean_variance_17*qnorm(0.997))
rate_care_day_otr_17$ucl <- mean_rate_17+(mean_variance_17*qnorm(0.997))


rate_care_day_otr <- rbind(rate_care_day_otr_13[,-which(names(rate_care_day_otr_13) %in% c("variance"))]
                           ,rate_care_day_otr_14[,-which(names(rate_care_day_otr_14) %in% c("variance"))]
                           ,rate_care_day_otr_15[,-which(names(rate_care_day_otr_15) %in% c("variance"))]
                           ,rate_care_day_otr_16[,-which(names(rate_care_day_otr_16) %in% c("variance"))]
                           ,rate_care_day_otr_17[,-which(names(rate_care_day_otr_17) %in% c("variance"))])

names(rate_care_day_otr) <- c("fiscal_yr"
                              ,"age_in_fy"
                              ,"On-The-Run Rate"
                              ,"Lower Limit"
                              ,"Upper Limit")

#write.csv(rate_care_day_otr, "rate_care_day_otr.csv")

rate_care_day_otr_long <- melt(rate_care_day_otr, id.vars = c("fiscal_yr", "age_in_fy"))

rate_care_day_otr_long$age_in_fy <- as.factor(rate_care_day_otr_long$age_in_fy)

levels(rate_care_day_otr_long$age_in_fy) <- c("13 Year Old Care Days"
                                              ,"14 Year Old Care Days"
                                              ,"15 Year Old Care Days"
                                              ,"16 Year Old Care Days"
                                              ,"17 Year Old Care Days")



paR_OTR <- ggplot(rate_care_day_otr_long[rate_care_day_otr_long$variable!="On-The-Run Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=variable
                       ,colour=variable)) + 
  geom_line(size=1) +
  geom_point(data=rate_care_day_otr_long[rate_care_day_otr_long$variable=="On-The-Run Rate",]
             ,aes(x=fiscal_yr
                  ,y=value)
             ,size=3) + 
  geom_smooth(data=rate_care_day_otr_long[rate_care_day_otr_long$variable=="On-The-Run Rate",]
              ,aes(x=fiscal_yr
                   ,y=value)
              ,fill=NA
              ,method="lm") +  
  geom_smooth(fill=NA, method="lm") +
  scale_y_continuous(labels = percent) +
  xlab("") +
  ylab("On-The-Run Rate (Age-Specific)") + 
  scale_color_manual(name="", values=c(poc_colors[c(1,4,2)])) +
  facet_grid(~age_in_fy) +
  theme_bw() +
  theme(legend.position="bottom")


R_OTR_14.13 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2014 & rate_care_day_otr$age_in_fy==13, ][[3]]
R_OTR_13.13 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2013 & rate_care_day_otr$age_in_fy==13, ][[3]]

R_OTR_14.14 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2014 & rate_care_day_otr$age_in_fy==14, ][[3]]
R_OTR_13.14 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2013 & rate_care_day_otr$age_in_fy==14, ][[3]]

R_OTR_14.15 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2014 & rate_care_day_otr$age_in_fy==15, ][[3]]
R_OTR_13.15 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2013 & rate_care_day_otr$age_in_fy==15, ][[3]]

R_OTR_14.16 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2014 & rate_care_day_otr$age_in_fy==16, ][[3]]
R_OTR_13.16 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2013 & rate_care_day_otr$age_in_fy==16, ][[3]]

R_OTR_14.17 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2014 & rate_care_day_otr$age_in_fy==17, ][[3]]
R_OTR_13.17 <- rate_care_day_otr[rate_care_day_otr$fiscal_yr==2013 & rate_care_day_otr$age_in_fy==17, ][[3]]

ggsave(file="p29.2_graph1.pdf")

require(pocr)
require(mstate)
library(RODBC)

con <- odbcConnect("POC")


sp_perm <- sqlQuery(con, "select * from ca_ods.base.episode_entries")

# tmat <- trans.comprisk(2, names = levels(sp_reun$discharge_type))
# 
# sp_reun$stat1 <- as.numeric(sp_reun$stat == 1)
# 
# sp_reun$stat2 <- as.numeric(sp_reun$stat == 2)
# 
# sp_reun_lng <- msprep(time = c(NA, "los", "los")
#                       ,status = c(NA, "stat1", "stat2")
#                       ,data = sp_reun
#                       ,keep = "state_fiscal_yyyy"
#                       ,trans = tmat)
# 
# sp_reun_lng <- expand.covs(sp_reun_lng, "state_fiscal_yyyy")

ci <- Cuminc(time = "los", status = "stat", group = "state_fiscal_yyyy", data = sp_perm)

#write.csv(ci, "ci_perm.csv")

point365 <- data.frame(year = 2000:2014
                       ,perm = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
  point365[i,2] <- ci[which(ci$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci[which(ci$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

n <- length(point365$perm)
mean_rate <- mean(point365$perm[1:n-1])
point365$variance <- abs(point365$perm - mean_rate)
mean_variance <- mean(point365$variance)

point365$lcl <- mean_rate-(mean_variance*qnorm(0.997))
point365$ucl <- mean_rate+(mean_variance*qnorm(0.997))

#write.csv(point365, "point365_perm.csv")


point365_long <- melt(point365[,c("year", "perm", "lcl", "ucl")], id.vars = "year")

levels(point365_long$variable) <- c("Permanency Incidence"
                                    ,"Lower Limit"
                                    ,"Upper Limit")

pI_pm365 <- ggplot(point365_long
                   ,aes(x=year
                        ,y=value
                        ,group=variable
                        ,colour=variable)) + 
  geom_line(size=1) +
  xlab("") +
  ylab("Cummulative Incidence of Permanency at One Year") + 
  scale_y_continuous(labels = percent) +
  scale_color_manual(name = ""
                     ,values=c(poc_colors[c(1,3,2)])) +
  theme_bw() +
  theme(legend.position="bottom")

I_pm365_14 <- point365[point365$year==2014, ][[2]]
I_pm365_13 <- point365[point365$year==2013, ][[2]]

ggsave(file="p26_graph1.pdf")





require(pocr)
require(mstate)
library(mstate)
library(pocr)

con <- odbcConnect("POC")

sp_adt <- sqlQuery(con, "select * from ca_ods.base.episode_lf_entries")
sp_adt$stat <- ifelse(sp_adt$stat == 3, 2, sp_adt$stat)

# 
# tmat <- trans.comprisk(2, names = levels(sp_adt$discharge_type))
# 
# sp_reun$stat1 <- as.numeric(sp_reun$stat == 1)
# 
# sp_reun$stat2 <- as.numeric(sp_reun$stat == 2)
# 
# sp_reun_lng <- msprep(time = c(NA, "los", "los")
#                       ,status = c(NA, "stat1", "stat2")
#                       ,data = sp_reun
#                       ,keep = "state_fiscal_yyyy"
#                       ,trans = tmat)
# 
# sp_reun_lng <- expand.covs(sp_reun_lng, "state_fiscal_yyyy")

ci <- Cuminc(time = "lolf"
             ,status = "stat"
             ,group = "state_fiscal_yyyy"
             ,data = sp_adt
             ,variance = FALSE)

#write.csv(ci, "ci_adt.csv")

point365 <- data.frame(year = 2000:2014
                       ,adt = rep(NA, length(2000:2014)))

for(i in 1:length(2000:2014)){
  point365[i,2] <- ci[which(ci$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci[which(ci$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.2
}

n <- length(point365$adt)
mean_rate <- mean(point365$adt[1:n-1])
point365$variance <- abs(point365$adt - mean_rate)
mean_variance <- mean(point365$variance)

point365$lcl <- mean_rate-(mean_variance*qnorm(0.997))
point365$ucl <- mean_rate+(mean_variance*qnorm(0.997))

#write.csv(point365, "point365_adt.csv")

point365_long <- melt(point365[,c("year", "adt", "lcl", "ucl")], id.vars = "year")

levels(point365_long$variable) <- c("Adoption Incidence"
                                    ,"Lower Limit"
                                    ,"Upper Limit")




pI_ad365 <- ggplot(point365_long
                   ,aes(x=year
                        ,y=value
                        ,group=variable
                        ,colour=variable)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = percent) +
  xlab("") +
  ylab("Cummulative Incidence of Adoption at One Year") + 
  scale_color_manual(name = ""
                     ,values=c(poc_colors[c(1,3,2)])) +
  theme_bw() +
  theme(legend.position="bottom")

I_ad365_14 <- point365[point365$year==2014, ][[2]]
I_ad365_13 <- point365[point365$year==2013, ][[2]]

ggsave(file="p26_graph2.pdf")



library(mstate)
require(pocr)
con <- odbcConnect("POC")
sp_reent <- sqlQuery(con, "select * from ca_ods.base.episode_reentries")

# 
# tmat <- trans.comprisk(2, names = levels(sp_adt$discharge_type))
# 
# sp_reun$stat1 <- as.numeric(sp_reun$stat == 1)
# 
# sp_reun$stat2 <- as.numeric(sp_reun$stat == 2)
# 
# sp_reun_lng <- msprep(time = c(NA, "los", "los")
#                       ,status = c(NA, "stat1", "stat2")
#                       ,data = sp_reun
#                       ,keep = "state_fiscal_yyyy"
#                       ,trans = tmat)
# 
# sp_reun_lng <- expand.covs(sp_reun_lng, "state_fiscal_yyyy")

ci <- Cuminc(time = "lop"
             ,status = "stat"
             ,group = "state_fiscal_yyyy"
             ,data = sp_reent)

#write.csv(ci, "ci_reent.csv")

point365 <- data.frame(year = 2000:2014
                       ,reent = rep(NA, length(2000:2014)))

#write.csv(point365, "point365_reent.csv")

for(i in 1:length(2000:2014)){
  point365[i,2] <- ci[which(ci$state_fiscal_yyyy == (i+1999)), ][which.min(abs(ci[which(ci$state_fiscal_yyyy == (i+1999)), ]$time - 365)), ]$CI.1
}

n <- length(point365$reent)
mean_rate <- mean(point365$reent[1:n-1])
point365$variance <- abs(point365$reent - mean_rate)
mean_variance <- mean(point365$variance)

point365$lcl <- mean_rate-(mean_variance*qnorm(0.997))
point365$ucl <- mean_rate+(mean_variance*qnorm(0.997))

point365_long <- melt(point365[,c("year", "reent", "lcl", "ucl")], id.vars = "year")

levels(point365_long$variable) <- c("Reentry Incidence"
                                    ,"Lower Limit"
                                    ,"Upper Limit")

pI_rt365 <- ggplot(point365_long
                   ,aes(x=year
                        ,y=value
                        ,group=variable
                        ,colour=variable)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = percent) +
  xlab("") +
  ylab("Cummulative Incidence of Reentry at One Year") + 
  scale_color_manual(name = ""
                     ,values=c(poc_colors[c(1,3,2)])) +
  theme_bw() +
  theme(legend.position="bottom")

I_rt365_14 <- point365[point365$year==2014, ][[2]]
I_rt365_13 <- point365[point365$year==2013, ][[2]]


ggsave(file="p27_graph1.pdf")


library(pocr)
library(RODBC)

con <- odbcConnect("POC")
sp_sib_plcmt <- sqlQuery(con, "select * from ca_ods.base.sibling_care_days_placed_tghr")
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


pR_SP <- ggplot(sp_sib_plcmt_long
                ,aes(x=fiscal_yr
                     ,y=value
                     ,group=variable
                     ,colour=variable)) + 
    geom_line(size=1) +
    labs(x = "", 
         y = "Rate of Sibling Placement") + 
    scale_y_continuous(labels = percent_format()) +
    scale_color_manual(name = ""
                       ,values=c(poc_colors[c(1,3,2)]), guide = FALSE) +
    theme_bw() +
    theme(legend.position="bottom")

pR_SP
# R_SP_14 <- sp_sib_plcmt[sp_sib_plcmt$fiscal_yr==2014, ][[4]]
# R_SP_13 <- sp_sib_plcmt[sp_sib_plcmt$fiscal_yr==2013, ][[4]]


ggsave(file="p28_graph2.pdf")


library(pocr)
library(RODBC)

con <- odbcConnect("POC")
con <- odbcConnect("POC")
library(Hmisc)

sp_exits_planning <- sqlQuery(con, "select * from ca_ods.base.exits_planning")

sp_exits_planning$planning_rate <- binconf(x=sp_exits_planning$exits_with_plans, n=sp_exits_planning$exits)[,1]

sp_exits_planning$lower <- binconf(x=sp_exits_planning$exits_with_plans, n=sp_exits_planning$exits)[,2]

sp_exits_planning$upper <- binconf(x=sp_exits_planning$exits_with_plans, n=sp_exits_planning$exits)[,3]

write.csv(sp_exits_planning, "rate_exits_planning.csv")

pR_ATP <- ggplot(sp_exits_planning, aes(y=planning_rate, x=fiscal_yr)) +
  geom_bar(position="dodge", stat="identity",  fill=poc_colors[1]) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position="dodge", width=0.25, size=1) +
  xlab("") +
  ylab("Rate of Adult-Transition Planning") + 
  theme_bw() 

R_ATP_14 <- sp_exits_planning[sp_exits_planning$fiscal_yr==2014, ][[4]]
R_ATP_13 <- sp_exits_planning[sp_exits_planning$fiscal_yr==2013, ][[4]]


ggsave(file="p30_graph1.pdf")


require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
con <- odbcConnect("Test Annie")
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

pR_L_3 <- ggplot(grade_three_lit, aes(y=passed
                                      ,x=year
                                      ,fill=fl_disability)) +
  geom_bar(position=position_dodge(width = 0.9
                                   ,height = NULL)
           ,stat="identity") +
  geom_errorbar(aes(ymin=i0, ymax=i1)
                ,width=0.4
                ,position=position_dodge(width = 0.9
                                         ,height = NULL)
                ,size=1) +
  xlab("") +
  scale_fill_manual(values = poc_colors[1:2], name="") +
  ylab("Third Grade Literacy Rate") + 
  theme_bw() +
  theme(legend.position="bottom")

R_L3_08 <- grade_three_lit[grade_three_lit$year==2008 & grade_three_lit$fl_disability=="Foster Children (Non-Disabled)", ][[5]]
R_L3_07 <- grade_three_lit[grade_three_lit$year==2007 & grade_three_lit$fl_disability=="Foster Children (Non-Disabled)", ][[5]]



ggsave(file="p31_graph1.pdf")




require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
con <- odbcConnect("Test Annie")
library(Hmisc)
library(dplyr)
high_school_comp <- sqlQuery(con, "call `sp_dropout_perc`('3');")

high_school_comp8 <- high_school_comp %>% filter(`Grade Level` == 9) %>%
  mutate(`Grade Level` = 8)
hsc <- rbind(high_school_comp8, high_school_comp)

pR_D <- ggplot(hsc[hsc$fl_disability==0,]
               ,aes(x=`Grade Level` + 0.5
                    ,y=`Drop Out Percent` / 100
               )) + 
  geom_step(size=1.8, direction = "vh") +
  scale_x_continuous(limits = c(8.5, 12.5)) +
  scale_y_continuous(limits = c(0, .40), labels = percent_format()) +
  xlab("Grade Level") +
  ylab("Percent Exiting High School Before Graduation") +
  theme_bw() 

R_D_07 <- hsc[hsc[,1]==12 & hsc[,2]==2007 & hsc$fl_disability==0, ][[8]]


ggsave(file="p31_graph2.pdf")
