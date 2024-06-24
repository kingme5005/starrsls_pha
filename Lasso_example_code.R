#run a lasso model on a train_data sample and apply coefficients to a test sample
library(grf)
library(glmnet)
library(plyr)


#ID 70 percent train_data sample
setwd('/harvard/aking/DataQC/pha/data/')

train_data <- read.csv('training_sample3b.csv')
test_data1 <- read.csv('test_sample3b.csv')


skip <- c('pid_pde','year_month_key_phat0','outcome_6msa','foldid','weight1','weight2')



#subset data to select predictors
#step1 <- train_data[ ,c(7:1909)]
#step1 <- train_data[ ,c(6:23)]
step1 <- as.data.frame(train_data[,!colnames(train_data) %in% skip])

library(dplyr)

var_vals <- apply(step1, 2, function(x) length(unique(x)))

vars_with_1val <- names(var_vals[var_vals < 2])

idx_dich_vars <- which(var_vals == 2)

dich_table_from_cases <- apply(step1[train_data$outcome_6msa == 1, idx_dich_vars], 2, table) %>%
  lapply(as.numeric) %>%
  do.call('rbind',.)

rare_dich_vars <- which(dich_table_from_cases < 5, arr.ind = TRUE) %>% row.names()

step1 <- step1[,!colnames(step1) %in% c(vars_with_1val, rare_dich_vars)]


#ID 30 percent test sample
#test<-read.csv("/home/data/Harvard/king/ls2/HL/data/ak_test_allpred_n4977.csv")

#subset data to select predictors
#step2 <- test[ ,c(7:1909)]
#step2 <- test_data[ ,c(6:23)]
#step2 <- as.data.frame(test_data1[,!colnames(test_data1) %in% skip])
#step3 <- as.data.frame(test_data2[,!colnames(test_data2) %in% skip])
step2 <- as.data.frame(test_data1[,colnames(step1)])


#name a new output dataset to append new predicted probabilities
output1 <- test_data1


set.seed(268)


#changing the outcome to the factor format
Y_train <- as.factor(train_data$outcome_6msa)

#changing the train_data/test dataset to a matrix format
x_train_matrix <- as.matrix(step1,rownames=T)
x_test_matrix <- as.matrix(step2,rownames=T)


#run lasso model
set.seed(268)
out.rf<-cv.glmnet(y=Y_train, x=x_train_matrix, family = "binomial", foldid = train_data$foldid,
weight=train_data$weight2, alpha = 1)

#save coefficients
coef <- predict(out.rf,s=out.rf$lambda.min,type = "coefficients")
coef <- as.matrix(coef)


#apply coefficients and save predicted probabilities
pp_lasso1 <- predict(out.rf, newx = x_train_matrix, s=out.rf$lambda.min, type = "response")
pp_lasso2 <- predict(out.rf, newx = x_test_matrix, s=out.rf$lambda.min, type = "response")



write.csv(coef, file = "/harvard/aking/DataQC/pha/tables/sample3_lasso_coef_z.csv")

predprob1 <- data.frame(pid_pde = train_data$pid_pde, year_month_key = train_data$year_month_key_phat0, pp_lasso1)
write.csv(predprob1, file = "/harvard/aking/DataQC/pha/data/sample3_lasso_pp_training.csv", row.names = FALSE)

predprob2 <- data.frame(pid_pde = test_data1$pid_pde, year_month_key = test_data1$year_month_key_phat0, pp_lasso2)
write.csv(predprob2, file = "/harvard/aking/DataQC/pha/data/sample3_lasso_pp_test1.csv", row.names = FALSE)








