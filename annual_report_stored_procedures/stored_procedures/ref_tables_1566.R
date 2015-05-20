
source("functions.R")
source("configurations.R")

#######################################
# Creating disability reference table #
#######################################
#######################################
#### WARNING THIS DATA WAS CHANGED ####
#######################################

cd_disability <- c(0, 1, 2)
tx_disibility <- c("All", "Non-Disabled", "Disabled")
ref_lookup_disability <- as.data.frame(cbind(tx_disibility, cd_disability))

#########################################
# Creating student typw reference table #
#########################################
#########################################
##### WARNING THIS DATA WAS CHANGED #####
#########################################

cd_student_type <- c(0, 1, 2, 3)
tx_student_type <- c("All", "Non_Foster_Care", "FRL_Non_Foster_Care", "Foster_Care")
ref_lookup_student_type <- as.data.frame(cbind(tx_student_type, cd_student_type))

######################################
#### LOADING OR DROPPING THE DATA ####
######################################

# sqlDrop(con_test_annie, sqtable = "ref_lookup_student_type")
# sqlDrop(con_test_annie, sqtable = "ref_lookup_disability")

sqlSave(con_test_annie, dat = ref_lookup_student_type, tablename = "dbo.ref_lookup_student_type", rownames = FALSE)
sqlSave(con_test_annie, dat = ref_lookup_disability, tablename = "dbo.ref_lookup_disability", rownames = FALSE)