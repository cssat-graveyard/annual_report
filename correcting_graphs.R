# updating graphs  
require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
library(dplyr)

## dropout ####

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
    geom_step(size=1.8, direction = "vh") +
    scale_x_continuous(limits = c(8.5, 12.5)) +
    scale_y_continuous(limits = c(0, .42), labels = percent_format(),
                       expand = c(0, 0)) +
    labs(x = "Grade Level",
         y = "",
         title = "Cumulative Percentage Non-Disabled Foster Youth\nExiting High School Before Graduation") +
    facet_wrap(~ cohort.year) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank())
pR_D

ggsave(file="droupout.pdf", plot = pR_D, width = 8, height = 4.5)


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
    scale_color_manual(name="", values=c(poc_colors[c(1,4,2)]), guide = F) +
    facet_wrap(~years_in_care) +
    theme_bw(10) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8)))
ggsave("graph-7.pdf", plot = pdR_M, width = 6, height = 3.5)

## OTR g9 ####

load("graph_dat.RData")
otr <- rate_care_day_otr_long
otr$age <- as.character(otr$age_in_fy)
otr$age = str_replace(otr$age, pattern = " Care Days", "s")
otr$age = str_replace(otr$age, pattern = "Year Old", "Year-Old")

paR_OTR <- ggplot(otr[otr$variable!="On-The-Run Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=variable
                       ,colour=variable)) + 
    geom_line(size=1) +
    geom_point(data=otr[otr$variable=="On-The-Run Rate",]
               ,aes(x=fiscal_yr
                    ,y=value)
               ,size=3) + 
    geom_smooth(data=otr[otr$variable=="On-The-Run Rate",]
                ,aes(x=fiscal_yr
                     ,y=value)
                ,fill=NA
                ,method="lm") +  
    geom_smooth(fill=NA, method="lm") +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = "",
         title = 'Percent of Care-Days "On-The-Run"') +
    scale_color_manual(name="", values=c(poc_colors[c(1,4,2)])) +
    facet_grid(~age) +
    theme_bw() +
    theme(legend.position="bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_blank())

#ggsave("otr.pdf", plot = paR_OTR, width = 15, height = 5)
ggsave("graph-9.pdf", plot = paR_OTR, width = 15, height = 5)

