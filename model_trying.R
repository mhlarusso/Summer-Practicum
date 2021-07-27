

#full
logit <- glm(default ~ collections_12_mths_ex_med + delinq_2yrs + dti +
               earliest_cr_line_month + earliest_cr_line_year + inq_last_6mths +
               open_acc + pub_rec + revol_bal + revol_util + total_acc + verification_status +
               addr_state + annual_inc + emp_length + home_ownership + acc_now_delinq + 
               tot_cur_bal + total_rev_hi_lim + loan_amnt + purpose, 
             data=train2, family=binomial(link="logit"))
summary(logit)

#only good vars
logit <- glm(default ~ loan_amnt + annual_inc + verification_status + 
               revol_util + inq_last_6mths + dti + purpose,
             data=train_master_processed, family=binomial(link="logit"))
summary(logit)

concordance(logit)

#testing
logit <- glm(default ~ loan_amnt + tot_cur_bal + annual_inc + verification_status + 
               revol_util + inq_last_6mths + dti + purpose,
             data=test2, family=binomial(link="logit"))
summary(logit)

Concordance(test2$default, predict(logit, type = "response"))


#trying on good training dataset

#remove outliers
train_master_subset = train_master_subset %>% filter(annual_inc != max(annual_inc))
train_master_subset = train_master_subset %>% filter(annual_inc != max(annual_inc))
train_master_subset = train_master_subset %>% filter(annual_inc != max(annual_inc))

logit <- glm(default ~ collections_12_mths_ex_med + delinq_2yrs + dti +
               earliest_cr_line_month + earliest_cr_line_year + inq_last_6mths +
               open_acc + pub_rec + revol_bal + revol_util + total_acc + verification_status +
               addr_state + annual_inc + emp_length + home_ownership + acc_now_delinq + 
               tot_cur_bal + total_rev_hi_lim + loan_amnt + purpose, data=train_master_subset,
             family=binomial(link="logit"))

summary(logit)

#filtering out significant variables
logit_best <- glm(default ~ delinq_2yrs + dti + inq_last_6mths + open_acc + 
               pub_rec + revol_bal + revol_util + total_acc + verification_status +
               addr_state + annual_inc + acc_now_delinq + tot_cur_bal + loan_amnt + 
               total_rev_hi_lim + purpose, data=train_master_subset,
             family=binomial(link="logit"))

summary(logit_best)

#install.packages("survival")
library(survival)
concordance(logit_best)

#testing
#subset outliers
logit_best_test <- glm(default ~ delinq_2yrs + dti + inq_last_6mths + open_acc + 
                    pub_rec + revol_bal + revol_util + total_acc + verification_status +
                    addr_state + annual_inc + acc_now_delinq + tot_cur_bal + loan_amnt + 
                    total_rev_hi_lim + purpose, data=test_master_subset,
                  family=binomial(link="logit"))


logit_final = glm(default~dti+tot_cur_bal+loan_amnt+annual_inc+revol_util + inq_last_6mths + purpose, data=train_master_subset, 
    family=binomial(link="logit"))

concordance(logit_final)

summary(logit_final)

100*(exp(coef(logit_final))-1)


#graphing coefficients
purpose <- c("Credit Card", "Debt Consolidation", "Educational", "Home Improvement",
             "House", "Major Purchase", "Medical", "Moving", "Other", "Renewable Energy",
             "Small Business", "Vacation", "Wedding")
coefficients <- c(-9.716339, 10.28988, 67.82247,25.28532, 21.93917, 16.17263, 73.03888,
                   62.48229, 61.85973, 30.45561, 153.5016, 52.09538, 6.822)

purp_coef <- data.frame(cbind(purpose,coefficients))
purp_coef = purp_coef[order(coefficients),]
purp_coef$coefficients <- as.numeric(purp_coef$coefficients)


purp_coef_plot <- ggplot(data=purp_coef, aes(fct_rev(fct_reorder(purpose,
                                                                 coefficients,.desc=TRUE)),
                                             coefficients)) +
  geom_bar(stat='identity',fill='steelblue') + coord_flip()

purp_coef_plot


#inq_last_6_months plot
pulls <- c(1,2,3)  
coefficients <- c(25.90977,47.36647,73.63612)
inq_coef <- data.frame(cbind(pulls,coefficients))

inq_coef_plot <- ggplot(data=inq_coef, aes(x=pulls,y=coefficients)) +
  geom_bar(stat='identity',fill='steelblue') 

inq_coef_plot











