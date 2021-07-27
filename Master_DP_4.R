#-------------------------------------------------------------------------------
# Revision: 4
# Date:7/19/2021
# Authors: Daniel 
# Description: Included all code to remove unnecessary columns. This also includes
# grouping categorical variables, removing over 50% null columns,
# and imputing NA values.
#-------------------------------------------------------------------------------

# Automatically install all missing libraries to your server instance
# When adding code to this script, please add any libraries not already
# on this list
require("dplyr")
require("tidyverse")
require("purrr")
require("haven")
require("lubridate")

# Load all libraries
# When adding code to this script, please add any libraries not already
# on this list
library(dplyr)
library(tidyverse)
library(purrr)
library(haven)
library(lubridate)

# Set Working Directory to summer_data
setwd("D:/Summer_Data")

### Run this Once
extra_info_m = read_sas("extra_info_m.sas7bdat")
loan_info = read_sas("loan_info.sas7bdat")
extra_info_m <- extra_info_m %>%
  mutate(id=gsub("^.*?=","",url))
loan <- merge(loan_info, extra_info_m, by = "id")

# Load training and test data
train_master = read.csv("Train_Data.csv")  
test_master = read.csv("Test_Data.csv")


# manual function for mode
Mode <- function(x) {
  ux <-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

# A function to process data sets
process_data = function(df){
  
  ### imputing null values
  # revol_util : use mean
  df[is.na(df[,"revol_util"]), "revol_util"] <- mean(loan[,"revol_util"], na.rm = TRUE)
  
  # collections_12_mths_ex_med : 145 NA values, 99% are 0, change NA values to 0
  df[is.na(df[,"collections_12_mths_ex_med"]), "collections_12_mths_ex_med"] <- 0
  
  # last_credit_pull_d : use mode

  df[is.na(df[,"last_credit_pull_d"]), "last_credit_pull_d"] <- Mode(loan[,"last_credit_pull_d"])
  
  # last_pymnt_d: use mode
  df[is.na(df[,"last_pymnt_d"]), "last_pymnt_d"] <- Mode(loan[,"last_pymnt_d"])

  # tot_coll_amt : 700946 values are 0, impute NA to 0 (70276 NA values)
  df[is.na(df[,"tot_coll_amt"]), "tot_coll_amt"] <- 0
  
  # tot_cur_bal : use mean (70276 NA values)
  df[is.na(df[,"tot_cur_bal"]), "tot_cur_bal"] <- mean(loan[,"tot_cur_bal"], na.rm = TRUE)
  
  # total_rev_hi_lim : use mean (70276 NA values)
  df[is.na(df[,"total_rev_hi_lim"]), "total_rev_hi_lim"] <- mean(loan[,"total_rev_hi_lim"], na.rm = TRUE)
  
  
  #The date columns are:
  dates <- c("issue_d", "earliest_cr_line", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d")
  
  #changing sas dates to R dates
  #adding day, month, and year columns
  for (i in dates) {
    df[[paste(i, "_sas", sep="")]] = df[[i]]
    df[[i]] = format(as.Date(df[[i]], origin="1960-01-01"),"%Y-%m-%d")
    df[[paste(i, "_year", sep="")]] = as.factor(year(df[[i]]))
    df[[paste(i, "_month", sep="")]] = as.factor(month(df[[i]]))
    #df[[paste(i, "_day", sep="")]] = as.factor(day(df[[i]]))
  }
  
  # Changing all character variables to factors
  for(i in 1:ncol(df)) {    
    if (class((df[ , i])) == "character"){
      df[ , i] = as.factor(df[ , i])
    }
  }
  
  # Changing id's to factor
  df$id = as.factor(df$id)
  df$member_id = as.factor(df$member_id)
  
  ### Grouping
  
  # group pub_rec to 0 and 1s - Changed all value greater than 0 to 1
  df$pub_rec <- sapply(df$pub_rec, function(x) replace(x,x>0,1))
  df$pub_rec <- as.factor(df$pub_rec)
  
  # group acc_now_delinq to 0 and 1s - Changed all value greater than 0 to 1
  df$acc_now_delinq <- sapply(df$acc_now_delinq, function(x) replace(x,x>0,1))
  df$acc_now_delinq <- as.factor(df$acc_now_delinq)
  
  # group total_acc - By 10s up to 50 and then 50 and up
  # 0~10 -> 1
  # 11~20 -> 2
  # 21~30 -> 3
  # 31~40 -> 4
  # 41~50 -> 5
  # 50< -> 6
  df <- df %>%
    mutate(total_acc = case_when(
      total_acc >=0 & total_acc <10 ~ 1,
      total_acc >=10 & total_acc <20 ~2,
      total_acc >=20 & total_acc <30 ~3,
      total_acc >=30 & total_acc <40 ~4,
      total_acc >=40 & total_acc <50 ~5,
      total_acc >=50 ~ 6
    ))
  df$total_acc <- as.factor(df$total_acc)
  
  # group open_acc - By 5s up to 25 and rest up
  # 0~5 -> 1
  # 6~10 -> 2
  # 11~15 -> 3
  # 16~20 -> 4
  # 21~25 -> 5
  # 26< -> 6
  df <- df %>%
    mutate(open_acc = case_when(
      open_acc >=0 & open_acc <5 ~ 1,
      open_acc >=5 & open_acc <10 ~2,
      open_acc >=10 & open_acc <15 ~3,
      open_acc >=15 & open_acc <20 ~4,
      open_acc >=20 & open_acc <25 ~5,
      open_acc >=25 ~ 6
    ))
  df$open_acc <- as.factor(df$open_acc)
  
  # group delinq_2yrs - 0 , 1, 2 -> greater than 1
  df <- df %>%
    mutate(delinq_2yrs = case_when(
      delinq_2yrs ==0 ~0,
      delinq_2yrs ==1 ~1,
      delinq_2yrs > 1 ~ 2
    ))
  df$delinq_2yrs <- as.factor(df$delinq_2yrs)
  
  # group inq_last_6mths - 0 1, 2, 3-> greater than 2
  df <- df %>%
    mutate(inq_last_6mths = case_when(
      inq_last_6mths ==0 ~0,
      inq_last_6mths ==1 ~1,
      inq_last_6mths ==2 ~2,
      inq_last_6mths >2 ~3
    ))
  df$inq_last_6mths <- as.factor(df$inq_last_6mths)
  
  
  
  
  # remove columns with greater than 50% null value
  df <- df[, which(colMeans(!is.na(df))>0.5,)]
  
  # remove rows where loan status mentions "Does not meet the credit policy"
  df <- df[(!df$loan_status %in% c("Does not meet the credit policy. Status:Fully Paid", 
                                               "Does not meet the credit policy. Status:Charged Off")),]
  ### remove unnecessary columns
  # id : do not need
  # member_id : do not need
  # url : do not need
  # policy_code : all 1s
  # pymnt_plan : binary variable with only 10 "y"
  # desc : do not need
  # title : will need clean up in order to use
  # verification_status_joint : too many blanks
  # funded_amnt_inv,out_prncp_inv,funded_amnt,total_pymnt_inv,total_rec_prncp : removed due to collinearity
  
  df <- select(df, -c(id, member_id,url,policy_code, pymnt_plan, desc, title, 
                      verification_status_joint,funded_amnt_inv,out_prncp_inv,
                      funded_amnt,total_pymnt_inv,total_rec_prncp))
  
  ### remove columns but need further discussion
  # emp_title : Mallory has completed analysis, around 400,000 data points
  # SAS dates : Since we already have the original date, and Year and Month separated, SAS is no longer needed.
  # next_pymnt_d (3) : not needed for now/also high Null values
  # issue_d, earliest_cr_line, last_pymnt_d, next_pymnt_d, last_credit_pull_d : removed since separated by month and year
  # application_type : only 511 in joint
  
  df <- select(df, -c(emp_title,issue_d_sas, earliest_cr_line_sas, 
                      last_pymnt_d_sas, next_pymnt_d_sas, last_credit_pull_d_sas,
                      next_pymnt_d, next_pymnt_d_year,next_pymnt_d_month,
                      issue_d, earliest_cr_line, last_pymnt_d, next_pymnt_d, last_credit_pull_d, application_type))
  
  ### change the response (loan_status) to 0 and 1
  oldvals <- c("Fully Paid","Current","Charged Off","Issued","In Grace Period","Late (31-120 days)","Default","Late (16-30 days)")
  newvals <- factor(c("No","No","Yes","No","No","No","Yes","No"))
  newvals2<- c(0,0,1,0,0,0,1,0)
  df$default <- as.factor(newvals2[match(df$loan_status,oldvals)])
  
  
  return(df)
}

#Process the train and test data
train_master_processed = process_data(train_master)
test_master_processed = process_data(test_master)


