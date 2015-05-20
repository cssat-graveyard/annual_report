
source("configurations.R")

wb <- loadWorkbook("College.xlsx")
sheets <- getSheets(wb)


start_col <- 1
end_col <- 6
start_row <- 1

sheet <- sheets[["9th Grade Cohort"]]

# getting each xlsx sheet

grade_9 <- readColumns(sheet = sheets[["9th_Grade_Cohort"]], startColumn = start_col, endColumn = end_col, startRow = start_row, 
                       endRow = 7, header = T, stringsAsFactors = FALSE)

grade_12 <- readColumns(sheet = sheets[["12th_Grade_Cohort"]], startColumn = start_col, endColumn = end_col, startRow = start_row, 
                        endRow = 19, header = T, stringsAsFactors = FALSE)		

college_comp <- readColumns(sheet = sheets[["College_Completetion_Rate"]], startColumn = start_col, endColumn = end_col, startRow = start_row, 
                            endRow = 10, header = T, stringsAsFactors = FALSE)

###########################
# processing grade 9 data #
###########################

grade_9 <- melt(grade_9)

grade_9$fl_disability <- ifelse(str_detect(grade_9$Type, "Non"), 0, 1)
grade_9$cd_student_type <- ifelse(str_detect(grade_9$variable, "Foster.Care"), 3, ifelse(str_detect(grade_9$variable, "FRL.Non.Foster"), 2, 1))
grade_9$Year <- rep(2006, length(grade_9$Year))

grade_9 <- dcast(grade_9, Year + cd_student_type + fl_disability ~ Variable, value.var = "value")
names(grade_9) <- c("year", "cd_student_type", "fl_disability", "9th_grade_count", "college_enrollment_count", "enrollment_percent")

for(i in 1:5){
    grade_9[,i] <- as.integer(grade_9[,i])
}

grade_9 <- select(grade_9, year, cd_student_type, fl_disability, enrollment_percent, `9th_grade_count`, college_enrollment_count)
grade_9$enrollment_percent <- round(grade_9$enrollment_percent, 4) * 100

############################
# processing grade 12 data #
############################

grade_12 <- melt(grade_12)

grade_12$fl_disability <- ifelse(str_detect(grade_12$Type, "Non"), 0, 1)
grade_12$cd_student_type <- ifelse(str_detect(grade_12$variable, "Foster.Care"), 3, ifelse(str_detect(grade_12$variable, "FRL.Non.Foster"), 2, 1))
grade_12$Year <- ifelse(grade_12$Year == "2006-07", 2006, ifelse(grade_12$Year == "2007-08", 2007, 2008)) 

grade_12 <- dcast(grade_12, Year + cd_student_type + fl_disability ~ Variable, value.var = "value")
names(grade_12) <- c("year", "cd_student_type", "fl_disability", "12th_grade_count", "college_enrollment_count", "enrollment_percent")

for(i in 1:5){
    grade_12[,i] <- as.integer(grade_12[,i])
}

grade_12 <- select(grade_12, year, cd_student_type, fl_disability, enrollment_percent, `12th_grade_count`, college_enrollment_count)
grade_12$enrollment_percent <- round(grade_12$enrollment_percent, 4) * 100

######################################
# processing college completion data #
######################################

college_comp <- filter(college_comp, Type == "All")
college_comp <- melt(college_comp)


college_comp$cd_student_type <- ifelse(str_detect(college_comp$variable, "Foster.Care"), 3, 
                                       ifelse(str_detect(college_comp$variable, "FRL.Non.Foster"), 2, 1))
college_comp$Year <- 2007

college_comp <- dcast(college_comp, Year + cd_student_type ~ Variable, value.var = "value")
names(college_comp) <- c("year", "cd_student_type", "12th_grade_count", "post-secondary_completion", "percent_completed_degree")
college_comp <- select(college_comp, year, cd_student_type, percent_completed_degree, `12th_grade_count`, `post-secondary_completion`)
college_comp$percent_completed_degree <- round(college_comp$percent_completed_degree, 4) * 100

for(i in c(1:2, 4:5)){
    college_comp[,i] <- as.integer(college_comp[,i])
}


con <- odbcConnect("POC")

# sqlDrop(con_poc, sqtable = "prtl_9th_grade_college_enrollment")
# sqlDrop(con_poc, sqtable = "prtl_12th_grade_college_enrollment")
# sqlDrop(con_poc, sqtable = "prtl_post_secondary_completion")

sqlSave(con_poc, dat = grade_9, tablename = "dbo.prtl_9th_grade_college_enrollment", rownames = FALSE)
sqlSave(con_poc, dat = grade_12, tablename = "dbo.prtl_12th_grade_college_enrollment", rownames = FALSE)
sqlSave(con_poc, dat = college_comp, tablename = "dbo.prtl_post_secondary_completion", rownames = FALSE)

con <- odbcConnect("test_annie")

# sqlDrop(con_test_annie, sqtable = "prtl_9th_grade_college_enrollment")
# sqlDrop(con_test_annie, sqtable = "prtl_12th_grade_college_enrollment")
# sqlDrop(con_test_annie, sqtable = "prtl_post_secondary_completion") 

sqlSave(con_test_annie, dat = grade_9, tablename = "prtl_9th_grade_college_enrollment", rownames = FALSE)
sqlSave(con_test_annie, dat = grade_12, tablename = "prtl_12th_grade_college_enrollment", rownames = FALSE)
sqlSave(con_test_annie, dat = college_comp, tablename = "prtl_post_secondary_completion", rownames = FALSE)



