# updating graphs  

## dropout

require(RODBC)
require(ggplot2)
require(reshape2)
require(pocr)
con <- odbcConnect("test_annie")
#library(Hmisc)
library(dplyr)
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
