# packages

library(RODBC)
library(ggplot2)
library(pocr)
library(dplyr)
library(reshape2)
library(tidyr)

# setting working directory

setwd("C:/Users/oieeri26/Desktop/GitHub/annual_report")

# getting data

con <- odbcConnect("test_annie")

# 12th grade cohort

data_12th_grade <- sqlQuery(con, "SELECT * 
									FROM test_annie.prtl_12th_grade_college_enrollment AS C
										JOIN ref_lookup_student_type AS ST
											ON C.cd_student_type = ST.cd_student_type
										JOIN test_annie.ref_lookup_disability AS D
											ON D.fl_disability = C.fl_disability;")

data_12th_grade <- data_12th_grade %>% select(year, enrollment_percent, tx_student_type, tx_disibility) %>%
	filter(tx_disibility == "Non-Disabled")
	
cohort_12th_grade <- ggplot(data_12th_grade, aes(x = year, y = enrollment_percent/100, fill = tx_student_type)) +
	geom_bar(stat = "identity", position = position_dodge()) + 
	# geom_text(aes(label = percent(enrollment_percent/100)), stat = "identity", color = "white") +
    scale_y_continuous(labels = percent_format()) + #,expand = c(0, 0), limits = c(0, max(data_12th_grade$enrollment_percent/100) * 1.15)) +
	labs(y = "Enrolled in Post-Secondary Education", x = "") +
	# facet_wrap(~ tx_disibility) +
	scale_fill_manual(values = c("#D9BB32", "#6DB33F", "#6E9CAE")) +
	guides(fill = guide_legend(title = "")) +
	theme_bw() + 
	theme(panel.grid.minor = element_blank(),
		  panel.grid.major = element_blank(),
		  axis.title.x = element_text(vjust = -.5, size = 16),
		  axis.title.y = element_text(vjust = .85, size = 16),
		  legend.position="bottom")
		  
cohort_12th_grade
ggsave("cohort_12th_grade.pdf", cohort_12th_grade, width = 10)
	
# 9th grade cohort
											
data_9th_grade <- sqlQuery(con, "SELECT * 
									FROM test_annie.prtl_9th_grade_college_enrollment AS C
										JOIN ref_lookup_student_type AS ST
											ON C.cd_student_type = ST.cd_student_type
										JOIN test_annie.ref_lookup_disability AS D
											ON D.fl_disability = C.fl_disability;")

data_9th_grade <- data_9th_grade %>% select(year, enrollment_percent, tx_student_type, tx_disibility) %>%
	filter(tx_disibility == "Non-Disabled")

cohort_9th_grade <- ggplot(data_9th_grade, aes(x = tx_student_type, y = enrollment_percent/100, fill = tx_student_type)) +
	geom_bar(stat = "identity", position = position_dodge()) + 
    scale_y_continuous(labels = percent_format()) + 
	labs(y = "Enrolled in Post-Secondary Education", x = "") +
	scale_fill_manual(values = c("#D9BB32", "#6DB33F", "#6E9CAE"), guide = FALSE) +
	# guides(fill = guide_legend(title = "")) +
	theme_bw() + 
	theme(panel.grid.minor = element_blank(),
		  panel.grid.major = element_blank(),
		  axis.title.x = element_text(vjust = -.5, size = 16),
		  axis.title.y = element_text(vjust = .85, size = 16),
		  legend.position="bottom")											

cohort_9th_grade
ggsave("cohort_9th_grade.pdf", cohort_9th_grade)

		  
# college completion data											
											
data_completion <- sqlQuery(con, "SELECT * 
									FROM test_annie.prtl_post_secondary_completion AS C
										JOIN ref_lookup_student_type AS ST
											ON C.cd_student_type = ST.cd_student_type;")
																				
post_secondary_completion <- ggplot(data_completion, aes(x = tx_student_type, y = percent_completed_degree/100, fill = tx_student_type)) +
	geom_bar(stat = "identity") + 
	# geom_text(aes(label = percent(percent_completed_degree/100)), stat = "identity", color = "white", vjust = 1.5)+
	scale_y_continuous(labels = percent_format()) + #,expand = c(0, 0), limits = c(0, max(data_completion$percent_completed_degree/100) * 1.01))+
	labs(x = "", y = "Completed Post-Secondary Degree") +
	scale_fill_manual(values = c("#D9BB32", "#6DB33F", "#6E9CAE"), guide = FALSE) +
	theme_bw() +
	theme(panel.grid.minor = element_blank(),
		panel.grid.major = element_blank(),
		axis.title.x = element_text(vjust = -.5, size = 16),
		axis.title.y = element_text(vjust = .85, size = 16),
		legend.position="bottom")	

post_secondary_completion
ggsave("post_secondary_completion.pdf", post_secondary_completion)
