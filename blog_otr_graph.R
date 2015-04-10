library(ggplot2)
library(pocr)
library(extrafont)
load("graph_dat.RData")
otr <- rate_care_day_otr_long
otr$age <- as.character(otr$age_in_fy)
otr$age = str_replace(otr$age, pattern = " Care Days", "s")
otr$age = str_replace(otr$age, pattern = "Year Old", "Year-Old")

otr1 = ggplot(otr[otr$variable == "On-The-Run Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=variable
                       ,colour=variable)) + 
        geom_smooth(fill=NA
                ,method="lm") +  
        scale_y_continuous(labels = percent) +
    labs(x = "State Fiscal Year", y = 'Percent of Care-Days "On-The-Run"\n'#, title = 
             ) +
    scale_color_manual(name="", values=c(poc_colors[c(1,4,2)]), guide = FALSE) +
    scale_x_continuous(breaks = c(2000, 2007, 2014)) +
    facet_wrap(~age, ncol = 2) +
    theme_bw(base_family = "PT Sans") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "gray30", color = "gray30"),
          strip.text = element_text(color = "white"),
          axis.title = element_text(size = rel(0.8)))

ggsave("blog_otr_graph.png", plot = otr1, height = 6, width = 5, dpi = 180)


otr2 = ggplot(otr[otr$variable == "On-The-Run Rate",]
                  ,aes(x=fiscal_yr
                       ,y=value
                       ,group=age
                       ,colour=age)) + 
        geom_smooth(fill=NA
                ,method="lm", size = 1.1) +  
        scale_y_continuous(labels = percent) +
    labs(x = "State Fiscal Year", y = 'Percent of Care-Days\n"On-The-Run"',
         color = "Age"
             ) +
    scale_color_manual(values = portal_colors, guide = F) +
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2014), limits = c(2000, 2015.5)) +
    geom_text(data = filter(otr, variable == "On-The-Run Rate", fiscal_yr == 2014) %>%
                  mutate(fiscal_yr = 2015.25, value = sqrt(0.95 * value)^2 ),
              aes(label = age), size = rel(3), family = "PT Sans") +
    theme_bw(base_family = "PT Sans") +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          strip.background = element_blank(),
          axis.title = element_text(size = rel(0.8))) +
    coord_cartesian(ylim = c(0, 0.08))
#otr2
ggsave("blog_otr_graph2.png", plot = otr2, height = 4.5, width = 7.5, dpi = 180)
