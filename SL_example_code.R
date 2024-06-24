
##############
# Set options
##############
options(scipen=999) # Turn off scientific notation


#################
# Load libraries
#################
library(SuperLearner)
library(dplyr)
library(WeightSVM)


#####################
# Load sourced files
#####################
source("/harvard/aking/sleep/code/R/SL.nnet.1.R") # Add decay
source("/harvard/aking/sleep/code/R/wrapper_SL_xgboost37.R") # Chris's expanded xgboost wrapper
source("/harvard/aking/sleep/code/R/create.Learner.grid.R") # Add tunegrid
source("/harvard/aking/sleep/code/R/SL.wsvm.R") #weights package


########################
# Set working directory
########################
setwd('/harvard/aking/DataQC/pha/data/')


##############################
# Load data and setup objects
##############################
train_data <- read.csv('training_sample3.csv')

skip <- c('pid_pde','year_month_key_phat0','outcome_6msa','foldid','weight1','weight2')


train_x <- train_data[,!colnames(train_data) %in% skip]
train_Y <- train_data$outcome_6msa
train_weight <- train_data$weight2
train_folds <- train_data$foldid
train_validrows <- lapply(1:10, function(x) which(train_folds==x))

n <- nrow(train_data) # Get number of observations


test_data <- read.csv('test_sample3.csv')

test_x <- test_data[,!colnames(test_data) %in% skip]
test_Y <- test_data$outcome_6msa
test_weight <- test_data$weight2


###################
# Create screeners
###################

# LASSO based screener
screen.lasso.sub <- function(Y,
                             X,
                             family,
                             obsWeights,
                             subs=c(1:ncol(X)),
                             alpha = 1,
                             minscreen = 5, 
                             nfolds = 5,
                             nlambda = 100,
                             dfmax = ncol(X) + 1,
                             parallel = FALSE,
                             ...)
{
  SuperLearner:::.SL.require('glmnet')
  if(!is.matrix(X)) {
    X <- model.matrix(~ -1 + ., X)
  }
  X_lasso <- X
  # X_lasso[,-subs] <- 0  ## Set the variables not in the subset to 0 ##
  
  fitCV <- glmnet::cv.glmnet(x = X_lasso,
                             y = Y,
                             family = family$family,
                             weights = obsWeights,
                             nfolds = nfolds,
                             lambda = NULL,
                             type.measure = 'deviance', 
                             alpha = alpha,
                             nlambda = nlambda,
                             dfmax = dfmax,
                             parallel = parallel,
                             ...)
  
  whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] != 0)
  # the [-1] removes the intercept
  if (sum(whichVariable) < minscreen) {
    warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
    sumCoef <- apply(as.matrix(fitCV$glmnet.fit$beta), 2, function(x) sum((x != 0)))
    newCut <- which.max(sumCoef >= minscreen)
    whichVariable <- (as.matrix(fitCV$glmnet.fit$beta)[, newCut] != 0)
  }
  
  return(whichVariable)
}

screen.lasso.15 <- function(...) {
  screen.lasso.sub (dfmax = 10,  ...)
}

screen.lasso.30 <- function(...) {
  screen.lasso.sub (dfmax = 30,  ...)
}

screen.lasso.45 <- function(...) {
  screen.lasso.sub (dfmax = 45,  ...)
}


# Ranger-based screener
screen.ranger.sub <- function (Y, X, family,subs = c(1:ncol(X)), nVar = ncol(X), 
                               ntree = 1000, maxdepth = 8, mtry = 10,
                               splitrule = NULL,
                               obsWeights,  ...)
{
  X_ranger <- X
  # X_ranger[,-subs] <- 0   ## Set the variables not in subset to 0 ##
  
  SuperLearner:::.SL.require('ranger')
  if (family$family == "gaussian") {
    rank.rf.fit <- ranger::ranger(Y ~ ., data = X_ranger,  num.tree = ntree, 
                                  max.depth = maxdepth, splitrule = splitrule,
                                  mtry = ifelse(mtry<ncol(X),mtry,),
                                  importance="impurity_corrected",
                                  case.weights = obsWeights )
  }
  if (family$family == "binomial") {
    rank.rf.fit <- ranger::ranger(as.factor(Y) ~ ., data=X_ranger,  num.tree = ntree, 
                                  max.depth = maxdepth, splitrule = splitrule,
                                  mtry = ifelse(mtry<ncol(X),mtry,),
                                  importance="impurity_corrected",
                                  case.weights = obsWeights)
  }
  whichVariable <- as.vector(rank(-rank.rf.fit$variable.importance[1:ncol(X_ranger)]) <= nVar)
  
  return(whichVariable)
}

screen.ranger.15 <- function(...) {
  screen.ranger.sub (nVar = 15,  ...)
}

screen.ranger.30 <- function(...) {
  screen.ranger.sub (nVar = 30,  ...)
}

screen.ranger.45 <- function(...) {
  screen.ranger.sub (nVar = 45,  ...)
}


#################################
# Create hyperparameter settings
#################################

## xgboost
n_tunes <- 6

set.seed(1234)
tune.rand.xgb <- data.frame(
  ntrees = rep(1000,n_tunes),
  max_depth = as.integer(ceiling(8*runif(n_tunes))),
  shrinkage = round(runif(n_tunes),3),   # Round to 3 decimals to shorten the names of algorithms
  gamma = round(runif(n_tunes, min = 0, max = 10),3),
  minobspernode = pmax(1,as.integer(n*runif(n_tunes, min = 0.05, max = 0.2))),
  early_stopping_rounds = rep(50,n_tunes),
  colsample_bytree = round(runif(n_tunes),3),
  colsample_bynode = round(runif(n_tunes),3)
)
tune.rand.xgb


## Ranger
## Make separate rangers for 10, 20 and 39 screened predictors
## Make min.node.size >= 2

n_tunes <- 2

set.seed(12345)

nvar <- 15

tune.rand.ranger15 <- data.frame(
  num.trees = rep(1000,n_tunes),
  max.depth = as.integer(ceiling(8*runif(n_tunes))),
  min.node.size = pmax(2, as.integer(n*runif(n_tunes, min = 0.05, max = 0.2))),
  mtry = pmax(2, as.integer(exp(log(nvar)*runif(n_tunes))))  ## min = 2
)
tune.rand.ranger15

nvar <- 30

tune.rand.ranger30 <- data.frame(
  num.trees = rep(1000,n_tunes),
  max.depth = as.integer(ceiling(8*runif(n_tunes))),
  min.node.size = pmax(2, as.integer(n*runif(n_tunes, min = 0.05, max = 0.2))),
  mtry = pmax(2, as.integer(exp(log(nvar)*runif(n_tunes))))  ## min = 2
)
tune.rand.ranger30

nvar <- 45

tune.rand.ranger45 <- data.frame(
  num.trees = rep(1000,n_tunes),
  max.depth = as.integer(ceiling(8*runif(n_tunes))),
  min.node.size = pmax(2, as.integer(n*runif(n_tunes, min = 0.05, max = 0.2))),
  mtry = pmax(2, as.integer(exp(log(nvar)*runif(n_tunes))))  ## min = 2
)
tune.rand.ranger45


## Weighted SVM
## Radial kernel specified later in grid.wsvm

n_tunes <- 3
set.seed(123456)

tune.rand.wsvm <- data.frame(
  cost = as.integer(runif(n_tunes,min=1,max=16))^as.integer(runif(n_tunes,min=1,max=7)),
  gamma = 1/(as.integer(runif(n_tunes,min=1,max=16))^as.integer(runif(n_tunes,min=1,max=7)))
)
tune.rand.wsvm

## Tune nnet
## #weights (limited to 1000) = size * (nvars + 1) + (size + 1)
## lasso screeners aren't exact, so add 5 to nvars to be safe

n_tunes <- 2

set.seed(1234567)

nvar <- 15
max_size <- 1
while(max_size * (nvar + 1 + 5) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.rand.nnet15 <- data.frame(
  size = as.integer(runif(n_tunes, min = 1, max = max_size)),
  decay = round(exp(log(500)*runif(n_tunes))-1,3) #0-500
)
tune.rand.nnet15

nvar <- 30
max_size <- 1
while(max_size * (nvar + 1 + 5) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.rand.nnet30 <- data.frame(
  size = as.integer(runif(n_tunes, min = 1, max = max_size)),
  decay = round(exp(log(500)*runif(n_tunes))-1,3) #0-500
)
tune.rand.nnet30

nvar <- 45
max_size <- 1
while(max_size * (nvar + 1 + 5) + (max_size + 1) < 1000)
{
  max_size <- max_size + 1
}

tune.rand.nnet45 <- data.frame(
  size = as.integer(runif(n_tunes, min = 1, max = max_size)),
  decay = round(exp(log(500)*runif(n_tunes))-1,3) #0-500
)
tune.rand.nnet45


##################
# Create learners
##################

grid.xgb = create.Learner.grid("SL.xgboost_cv", detailed_names = TRUE, 
                               tunegrid = tune.rand.xgb, name_prefix = 'SL.xgboost')

grid.ranger15 = create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = tune.rand.ranger15)
grid.ranger30 = create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = tune.rand.ranger30)
grid.ranger45 = create.Learner.grid("SL.ranger", detailed_names = TRUE, 
                                    tunegrid = tune.rand.ranger45)

grid.wsvm = create.Learner.grid("SL.wsvm", detailed_names = TRUE, 
                                params = list(kernel = "radial"),
                                tunegrid = tune.rand.wsvm)

grid.nnet15 = create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = tune.rand.nnet15, name_prefix = 'SL.nnet')
grid.nnet30 = create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = tune.rand.nnet30, name_prefix = 'SL.nnet')
grid.nnet45 = create.Learner.grid("SL.nnet.1", detailed_names = TRUE, 
                                  tunegrid = tune.rand.nnet45, name_prefix = 'SL.nnet')


grid.xgb.screen <- lapply(grid.xgb$names,c,"screen.ranger.15","screen.ranger.30","screen.ranger.45")
grid.xgb.screen

grid.ranger.screen15 <- lapply(grid.ranger15$names,c,"screen.ranger.15")
grid.ranger.screen15
grid.ranger.screen30 <- lapply(grid.ranger30$names,c,"screen.ranger.30")
grid.ranger.screen30
grid.ranger.screen45 <- lapply(grid.ranger45$names,c,"screen.ranger.45")
grid.ranger.screen45

grid.wsvm.screen <- lapply(grid.wsvm$names,c,"screen.lasso.15","screen.lasso.30","screen.lasso.45")
grid.wsvm.screen

grid.nnet.screen15 <- lapply(grid.nnet15$names,c,"screen.lasso.15")
grid.nnet.screen15
grid.nnet.screen30 <- lapply(grid.nnet30$names,c,"screen.lasso.30")
grid.nnet.screen30
grid.nnet.screen45 <- lapply(grid.nnet45$names,c,"screen.lasso.45")
grid.nnet.screen45

set.seed(123)
learners.glmnet <- create.Learner("SL.glmnet",
                                  tune = list(alpha = c(round(0.333*runif(1),3),
                                                        round(0.333 + 0.333*runif(1),3),
                                                        round(0.666 + 0.333*runif(1),3))), 
                                  detailed_names = T, name_prefix = "SL.glmnet")

list.glmnet.screen <- lapply(learners.glmnet$names, c, "screen.lasso.15","screen.lasso.30","screen.lasso.45")

SL.library <- c(grid.xgb.screen,
                grid.ranger.screen15,grid.ranger.screen30,grid.ranger.screen45,
                grid.nnet.screen15,grid.nnet.screen30,grid.nnet.screen45,
                grid.wsvm.screen,
                list.glmnet.screen)

SL.library

###################
# Run SuperLearner
###################
cvControl <- SuperLearner.CV.control(V = 10L, validRows=train_validrows)

set.seed(123)
sl <- SuperLearner(Y = train_Y, X = train_x, obsWeights = train_weight, family = binomial(),
                   SL.library = SL.library,
                   verbose = TRUE, cvControl = cvControl)

sl 

saveRDS(sl,'sl_sample3.rds')

names(sl$coef[which(sl$coef>0)])

####################################
# Get predicted probability in test
####################################
pred_train <- as.numeric(predict(sl, train_x, onlySL = TRUE)$pred)
pred_test <- as.numeric(predict(sl, test_x, onlySL = TRUE)$pred)


# Save predicted probs as csv
# This can be imported into SAS for sens, spec, and PPV
out_pred_train <- data.frame(train_data[,colnames(train_data) %in% skip],
                            pred_prob = pred_train)
out_pred_test <- data.frame(test_data[,colnames(test_data) %in% skip],
                            pred_prob = pred_test)

write.csv(out_pred_test, 'predprob_sample3_test.csv', row.names = F)
write.csv(out_pred_train, 'predprob_sample3_train.csv', row.names = F)


