library(xgboost)

education <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
age <- c(21,22,23,24,25,25,26,27,28,29,30,31,21,22,23,24,25,25,26,27,28,29,30,31)
limit_bal <- c(1000,2000,3000,1000,2000,3000,1000,2000,3000,1000,2000,3000,1000,2000,3000,1000,2000,3000,1000,2000,3000,1000,2000,3000)
default <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)

trainset <- data.frame(education, age, limit_bal, default)
trainset$education <- as.factor(trainset$education)
trainset$default <- as.factor(trainset$default)

str(trainset)

default_ind <- which(names(trainset) == "default")

# For classification problem
# Create sparse matrix
matrix <- Matrix(as.matrix(trainset[-default_ind]),sparse=TRUE)

data <- xgb.DMatrix(matrix,
                    label = as.integer(as.character(trainset$default)))

## Search to find best parameters
grid_search_params <- expand.grid(subsample = c(0.1, 0.3, 0.5, 0.7, 0.9, 1),
                                  colsample_bytree = c(0.2, 0.4, 0.6, 0.8, 1),
                                  max_depth = c(4,6,8),
                                  eta = c(0.01, 0.03, 0.05),
                                  alpha = c(0,1))

grid_search_results <- apply(grid_search_params, 1, function(parameterList){
  # Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentMaxDepth <- parameterList[["max_depth"]]
  currentEta <- parameterList[["eta"]]
  currentAlpha <- parameterList[["alpha"]]
  #print(currentColsampleRate)
  #print(currentSubsampleRate)
  #print(currentMaxDepth)
  #print(currentEta)
  #print(currentAlpha)
  param <- list("objective" = "binary:logistic",
                "eval_metric" = "auc",
                colsample = currentColsampleRate, # Column Sampling
                subsample = currentSubsampleRate, # Row Sampling
                max.depth = currentMaxDepth, # Max Tree Depth
                eta = currentEta, # Learning Rate/Shrinkage 
                alpha = currentAlpha # L1 regularization term on weights
                )
  cv_fit <- xgb.cv(data = data,
                  nround = 1000,
                  nthread = 2,
                  nfold = 3, # number of folds min = 3
                  params = param,
                  prediction = TRUE,
                  verbose = FALSE)

  auc_max <- max(cv_fit$evaluation_log$test_auc_mean)
  print(auc_max)
  return(c(currentSubsampleRate, currentColsampleRate, currentMaxDepth, currentEta, currentAlpha, auc_max))
})

grid_search_results