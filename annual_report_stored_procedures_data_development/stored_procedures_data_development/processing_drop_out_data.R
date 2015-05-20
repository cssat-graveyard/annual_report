source("configurations.R")

wb <- loadWorkbook("dropoutV2.xlsx")
sheets <- getSheets(wb)

################################################
# data that doesn't change from sheet to sheet #
################################################

start_col <- 1
end_col <- 9
start_row <- 1
end_row <- 25

##################
# Getting Sheets #
##################

drop_out_06 <- readColumns(sheet = sheets[["d2006-2007"]], startColumn = start_col, endColumn = end_col, startRow = start_row, 
                           endRow = end_row, header = T, stringsAsFactors = FALSE)

drop_out_07 <- readColumns(sheet = sheets[["d2007-2008"]], startColumn = start_col, endColumn = end_col, startRow = start_row, 
                           endRow = end_row, header = T, stringsAsFactors = FALSE)		

############################
# processing data for 2006 #	
############################

drop_out_06$Year <- 2006

drop_out_06$fl_disability <- ifelse(drop_out_06$Type2 == "Non-Disability", 0, 1)
drop_out_06$drop_out_percent <- round((1 - drop_out_06$X..Retained) * 100,2)
drop_out_06$cd_student_type <- ifelse(drop_out_06$Type1 == "Non-Foster Non-FRL", 1, ifelse(drop_out_06$Type1 == "Non-Foster FRL", 2, 3))

drop_out_06 <- select(drop_out_06, Grade, Year, cd_student_type, fl_disability, drop_out_percent, Beg..Tot., Dropout)

names(drop_out_06) <- c("tx_grade", "cohort_year", "cd_student_type", "fl_disability", "drop_out_percent", "total_students", "drop_out_count")

############################
# processing data for 2007 #
############################

drop_out_07$Year <- 2007

drop_out_07$fl_disability <- ifelse(drop_out_07$Type2 == "Non-Disability", 0, 1)
drop_out_07$drop_out_percent <- round((1 - drop_out_07$X..Retained) * 100,2)
drop_out_07$cd_student_type <- ifelse(drop_out_07$Type1 == "Non-Foster Non-FRL", 1, ifelse(drop_out_07$Type1 == "Non-Foster FRL", 2, 3))

drop_out_07 <- select(drop_out_07, Grade, Year, cd_student_type, fl_disability, drop_out_percent, Beg..Tot., Dropout)

names(drop_out_07) <- c("tx_grade", "cohort_year", "cd_student_type", "fl_disability", "drop_out_percent", "total_students", "drop_out_count")

drop_out <- rbind(drop_out_06, drop_out_07)

for (i in c(2:4, 6:7)){
    drop_out[,i] <- as.integer(drop_out[,i])
}

#sqlDrop(con_poc, sqtable = "dbo.prtl_dropout")
sqlSave(con_poc, dat = drop_out, tablename = "dbo.prtl_dropout", rownames = FALSE)

#sqlDrop(con_test_annie, sqtable = "prtl_dropout")
sqlSave(con_test_annie, dat = drop_out, tablename = "prtl_dropout", rownames = FALSE)




