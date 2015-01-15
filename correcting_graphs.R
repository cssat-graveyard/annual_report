# updating graphs  
require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
library(dplyr)

## constants:
bound.line.size = 0.5
data.point.size = 1.8
data.line.size = 1
base.size = 10


## count of intakes g1 ####

pR_I_counts <- ggplot(sp_rate_referral_clean[sp_rate_referral_clean$type=="Deseasonalized Trend",]
                      , aes(x=date
                            ,y=cnt.referrals
                            #,group=type
                            #,colour=type
                      )) + 
    #geom_line(size=1, colour=poc_colors[2]) +
    geom_smooth(colour=poc_colors[3], fill=NA, size= 1, method = "loess") + 
    xlab("") +
    ylab("Count of Intakes (monthly)\n") + 
    scale_y_continuous(labels = comma_format()) +
    scale_color_manual(labels = c("Deseasonalized Trend"
                                  ,"Observations")
                       ,values=c(poc_colors[3:4])
                       ,guide=guide_legend(title= "")
    ) +
    theme_classic(8) +
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "gray60", size = 0.35),
          axis.ticks = element_line(color = "gray60", size = 0.35))

ggsave(file="graph-1.pdf", plot = pR_I_counts, width = 3, height = 2.5)

## order specific intakes g2 ####

# waiting on Joe...

# graphs 3, 4, and 5 were cut ####

## safe care-days g6 ####
safe.care = sp_rate_care_day_maltreatment_clean %>% 
    mutate(safe.rate = 1e5 - care.day.incident.rate,
           ucl.old = ucl,
           ucl = 1e5 - lcl,
           lcl = 1e5 - ucl.old) %>%
    select(fiscal.yr, safe.rate, ucl, lcl)

safe.care.plot = ggplot(safe.care, aes(x = fiscal.yr, y = safe.rate)) +
    geom_line(aes(y = ucl), color = poc_colors[2]) +
    geom_line(aes(y = lcl), color = poc_colors[1]) +
    geom_line(color = poc_colors[4]) +
    geom_point(size = 2, color = poc_colors[4]) +
    scale_y_continuous(limits = c(99980, 1e5), expand = c(0, 0), labels = comma_format()) +
    scale_x_continuous(breaks = seq(2000, 2014, by = 4)) +
    labs(x = "", y = "Note: y-axis does not start at 0",
         title = "Safe Care-Days per 100,000 Care-Days") +
    theme_bw() +
    theme(legend.position="bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)))

#ggsave("safe-care.pdf", plot = safe.care.plot, width = 5, height = 3.5)
ggsave("graph-6.pdf", plot = safe.care.plot, width = 5, height = 3.5)

## overall transitions g7 ####

cdm = rate_care_day_movement_long
levels(cdm$years_in_care) = str_replace(levels(cdm$years_in_care), "Care Days", "Care-Days")
pdR_M <- ggplot(filter(cdm, variable != "Movement Rate")
                ,aes(x=fiscal_yr
                     ,y=value
                     ,group=variable
                     ,colour=variable)) + 
    geom_line(size= 0.5) +
    geom_point(data= filter(cdm, variable == "Movement Rate")
               ,aes(x=fiscal_yr
                    ,y=value)
               ,size=1.8) + 
    geom_smooth(data=filter(cdm, variable == "Movement Rate")
                ,aes(x=fiscal_yr
                     ,y=value)
                ,fill=NA
                ,method="lm") +
    labs(x = "",
         y = "Movement Rate Per 100,000 Care-Days") + 
    scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
    scale_color_manual(name="", values=c(poc_colors[c(1,4,2)]), guide = F) +
    facet_wrap(~years_in_care) +
    theme_bw(base.size) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)))
ggsave("graph-7.pdf", plot = pdR_M, width = 7, height = 3.5)

## moves to kin g8 ####

move_to_kin <- filter(rate_care_day_movement_tx_long, str_detect(type, "Kin"))
move_to_kin <- dcast(move_to_kin, formula = fiscal_yr + years_in_care + type ~ variable, value.var = "value")
names(move_to_kin) <- make.names(names(move_to_kin))

move_to_kin_graph <- ggplot(move_to_kin, aes(x = fiscal_yr, y = Movement.Rate,
                                             ymin = Lower.Limit, ymax = Upper.Limit)) +
    geom_smooth(fill=NA,
                color = portal_colors[8],
                size = 1,
                ,method="lm") +
    geom_point(size = data.point.size) + 
    geom_line(aes(y = Lower.Limit), color = poc_colors[1], size = bound.line.size) +
    geom_line(aes(y = Upper.Limit), color = poc_colors[3], size = bound.line.size) +
    xlab("") +
    scale_x_continuous(breaks = c(2000, 2004, 2008, 2012)) +
    ylab("Transition to Kin Care\n(per 100,000 Care Days)") + 
    facet_wrap(~ years_in_care) +
    theme_bw(base_size = base.size) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)))

ggsave("graph-8.pdf", plot = move_to_kin_graph, width = 7, height = 3.5)

## OTR g9 ####

#load("graph_dat.RData")
otr <- rate_care_day_otr_long
otr$age <- as.character(otr$age_in_fy)
otr$age = str_replace(otr$age, pattern = " Care Days", "s")
otr$age = str_replace(otr$age, pattern = "Year Old", "Year-Old")

paR_OTR <- ggplot(otr[otr$variable!="On-The-Run Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=variable
                       ,colour=variable)) + 
    geom_line(size= bound.line.size) +
    geom_point(data=otr[otr$variable=="On-The-Run Rate",]
               ,aes(x=fiscal_yr
                    ,y=value)
               ,size= data.point.size) + 
    geom_smooth(data=otr[otr$variable=="On-The-Run Rate",]
                ,aes(x=fiscal_yr
                     ,y=value)
                ,fill=NA
                ,method="lm") +  
    geom_smooth(fill=NA, method="lm") +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = 'Percent of Care-Days\n"On-The-Run"'#, title = 
             ) +
    scale_color_manual(name="", values=c(poc_colors[c(1,4,2)]), guide = FALSE) +
    facet_grid(~age) +
    theme_bw(base_size = base.size) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)))

#ggsave("otr.pdf", plot = paR_OTR, width = 15, height = 5)
ggsave("graph-9.pdf", plot = paR_OTR, width = 7, height = 2.5)

## permanency g10 ####
#TODO

## adoption g11 ####
#TODO

## siblings g12
#TODO

load("sibs-recalculated.Rdata")

sibs %>% select(fiscal_yr = state_fiscal_yyyy, pct = prp_some_tgh) %>%
    mutate()
    

# sp_sib_plcmt$lcl <- mean_rate-(mean_variance*qnorm(0.997))
# sp_sib_plcmt$ucl <- mean_rate+(mean_variance*qnorm(0.997))
# 
# #write.csv(sp_sib_plcmt, "rate_sib_plcmt.csv")
# 
# sp_sib_plcmt_long <- melt(sp_sib_plcmt[,c("fiscal_yr", "rate", "lcl", "ucl")], id.vars = "fiscal_yr")
# 
# 
# levels(sp_sib_plcmt_long$variable) <- c("Sibling Placement Rate"
#                                         ,"Lower Limit"
#                                         ,"Upper Limit")
# 
# ggplot(sp_sib_plcmt_long, aes(x = fiscal_yr, y ))
# 
# 
# ggsave(file="graph12_sibling_placenment_v2.pdf") 


## staffing g13 ####
#TODO

## literacy g14 ####
#TODO

## dropout g15 ####

con <- odbcConnect("test_annie")
#library(Hmisc)

high_school_comp <- sqlQuery(con, "call `sp_dropout_perc`('3');")

# Add in "8th grade" to complete geom step
high_school_comp8 <- high_school_comp %>% 
    filter(`Grade Level` == 9) %>%
    mutate(`Grade Level` = 8)
hsc <- rbind(high_school_comp8, high_school_comp)

names(hsc)[names(hsc) == "Cohort Period"] <- "cohort"
hsc$cohort.year <- factor(hsc$cohort, levels = 2006:2007,
                          labels = c("Entered 9th Grade in 2006",
                                     "Entered 9th Grade in 2007"))

pR_D <- ggplot(hsc[hsc$fl_disability==0,]
               ,aes(x=`Grade Level` + 0.5
                    ,y=`Drop Out Percent` / 100
               )) + 
    geom_step(size=data.line.size, direction = "vh", color = poc_colors[1]) +
    scale_x_continuous(limits = c(8.5, 12.5)) +
    scale_y_continuous(limits = c(0, .50), labels = percent_format(),
                       expand = c(0, 0)) +
    labs(x = "Grade Level",
         y = "Cumulative Drop-Out") +
    facet_wrap(~ cohort.year) +
    theme_bw(base_size = base.size) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)),
          axis.ticks = element_line(color = "gray50"))

ggsave(file="graph-15.pdf", plot = pR_D, width = 4, height = 2.2)

## postsec enrollment g16 ####

# TODO

## postsec completion g17 ####

# TODO