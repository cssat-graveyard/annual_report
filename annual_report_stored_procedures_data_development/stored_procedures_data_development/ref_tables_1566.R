
source("functions.R")
source("configurations.R")

#######################################
# Creating disability reference table #
#######################################

fl_disability <- c(0, 1)
tx_disibility <- c("Non-Disabled", "Disabled")
ref_lookup_disability <- as.data.frame(cbind(fl_disability, tx_disibility))

#########################################
# Creating student typw reference table #
#########################################

cd_student_type <- c(0, 1, 2, 3)
tx_student_type <- c("All", "Not in Out-of-Home Care", "Not in Care: Receiving Free or Reduced-Price Lunch", "In Out-of-Home Care")
ref_lookup_student_type <- as.data.frame(cbind(cd_student_type, tx_student_type))

######################################
#### LOADING OR DROPPING THE DATA ####
######################################

# sqlDrop(con_test_annie, sqtable = "ref_lookup_student_type")
# sqlDrop(con_test_annie, sqtable = "ref_lookup_disability")

sqlSave(con_test_annie, dat = ref_lookup_student_type, tablename = "ref_lookup_student_type", rownames = FALSE)
sqlSave(con_test_annie, dat = ref_lookup_disability, tablename = "ref_lookup_disability", rownames = FALSE)