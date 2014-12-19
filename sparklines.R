#setwd("S:/Data Portal/annual_report")
load("graph_dat.RData")

measures <- list()

sections <- c("Safety", "Permanency", "Well-Being")

# Columns for table: 
# Section | Table | Measure | Units | start_val | end_val | change_cat | assessment

# Outline in google spreadsheet