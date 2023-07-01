# Code from: Chinn, M. D., Meunier, B., Stumpner, S. (2023). 
#            "Nowcasting World Trade with Machine Learning: a Three-Step Approach", 
#            NBER Working Paper, No 31419, National Bureau of Economic Research
#
# Contact: baptistemeunier@hotmail.fr
# Version: 30/06/2023
#
#
# DESCRIPTION:
# The code is an user-friendly version of the code used in Chinn et al. (2023), allowing to forecast using the (simplified) three-step approach 
# This is an auxiliary function of MAIN.R, which tunes the hyper-parameters of machine learning algorithms
# The general idea is: 
#     - Divide the sample between a train and a test samples
#     - Use the last n observations for the test sample (so validation is done on most recent observations)
#     - Perform a grid search across hyper-parameters, meaning
#           o A range of values are given for each hyper-parameter (for example, the number of trees in the random forest can be 300, 500, or 700)
#           o The function tests all possible combinations of hyper-parameters
#           o Each combination is a possible model, that is fitted on the train sample and make prediction for the test sample
#           o The accuracy (RMSE) is calculated for each possible model)
#     - Select the combination of hyper-parameters (among the possible models) that minimizes the RMSE on test sample 


# =======================================================================================================================================
# Random Forest

tune_RF <- function(x,y,n){
  
  # Inputs:
  # x = regressors - should be a matrix
  # y = target variable - should be a vector
  # n = number of periods on which the hyper-parameter should be optimized
  # Output:
  # optim_param = vector of best-performing hyper-parameters in the following order:
  #           1 = mtry (number of variables selected - default value = nb(variables)/3)
  #           2 = nodesize (minimum size of terminal node - default value = 5)
  # NB: ntrees (number of trees - default value = 500) is not optimized here for 
  # (1) The sake of time
  # (2) It is generally enough to set a high value as accuracy plateaus after a number of ntrees (empirically 300 is enough in our case)
  
  print("Tuning random forest")
  
  # Start and end values
  n_tot <- nrow(x)
  n_test <- n_tot - n + 1
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("mtry","nodesize","RMSE")
  count_mod <- 1 
    
  m_min <- floor(ncol(x)/3)
  m_max <- ncol(x)
  m_step <- floor((m_max-m_min)/4)
  
  for (m_try in seq(m_min,m_max,by=m_step)){
    for (nd_size in seq(3,12,by=3)){
      
      # Define train and test samples
      x_tr <- x[1:n_test-1,]
      y_tr <- y[1:n_test-1]
      x_te <- x[n_test:n_tot,]
      y_te <- y[n_test:n_tot]
      
      # Fit model on train
      eq_rf <- randomForest(y = y_tr,
                            x = x_tr,
                            na.action = "na.omit",
                            do.trace = FALSE,
                            ntree = 300,
                            nodesize = nd_size,
                            mtry = m_try,
                            corr.bias = TRUE)
      
      # Predict on test
      fit <- predict(eq_rf, 
                     newdata = x_te,
                     na.action = "na.omit")
      
      # Write summary and advance counter
      summary[count_mod,1] <- m_try
      summary[count_mod,2] <- nd_size
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
      
    }
  }

  # Select best hyperparameter and return it
  summary %<>%
    arrange(RMSE)
  optim_param <- summary[1,1:2]
  
  return(optim_param) 
  
}


# =======================================================================================================================================
# XG Boost - tree

tune_XGBT <- function(x,y,n){
  
  # Inputs:
  # x = regressors - should be a matrix
  # y = target variable - should be a vector
  # n = number of periods on which the hyperparameter should be optimized
  # Output:
  # optim_param = vector of best-performing hyperparameters in the following order:
  #           1 = nrounds (number of iterations)
  #           2 = eta (learning rate - default value = 3)
  #           3 = max_depth (tree depth - default value = 6)
  #           4 = min_child (minimum child weight - default value = 1)
  #           5 = gamma (minimum loss reduction required to make a further partition on a leaf node of the tree - default value = 0)
  # NB: Column (row) sampling, i.e. the subsample ratio of columns (rows) kept when growing each tree are not optimized here
  # Parameters colsample_bytree (subsample of columns) are therefore left at their default value of 1. This is :
  # (1) For the sake of time
  # (2) These parameters are generally used to gain time on very large datasets - which is not our case
  
  print("Tuning gradient boosting (trees)")
  
  # Start and end values
  n_tot <- nrow(x)
  n_test <- n_tot - n + 1
  
  #
  # Step 1: optimization of nrounds and eta
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("nrounds","eta","RMSE")
  count_mod <- 1 
  
  for (nrounds in seq(from = 10, to = 100, by = 30)){
    for (eta in c(0.05,0.1,0.2,0.3)){
      
      # Define train and test samples
      x_tr <- x[1:n_test-1,]
      y_tr <- y[1:n_test-1]
      x_te <- x[n_test:n_tot,]
      y_te <- y[n_test:n_tot]
      
      temp_tr <- xgb.DMatrix(label = y_tr,
                             data = x_tr)
      
      eq_boost <- xgboost(data = temp_tr,
                          nrounds = nrounds,
                          eta = eta,
                          verbose = 0)
      
      # Predict on test
      fit <- predict(eq_boost,
                     newdata = x_te,
                     na.action = "na.omit")
      
      # Write summary and advance counter
      summary[count_mod,1] <- nrounds
      summary[count_mod,2] <- eta
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
    }
  }
  
  # Select best hyperparameters
  summary %<>%
    arrange(RMSE)
  optim_nrounds <- summary[1,1]
  optim_eta <- summary[1,2]
  
  #
  # Step 2: optimization of max_depth and min_child_weight
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("max_depth","min_child","RMSE")
  count_mod <- 1 
  
  for (max_depth in c(3,5,6,7,9)){
    for (min_child_weight in 1:4){
      
      # Define train and test samples
      x_tr <- x[1:n_test-1,]
      y_tr <- y[1:n_test-1]
      x_te <- x[n_test:n_tot,]
      y_te <- y[n_test:n_tot]
      
      temp_tr <- xgb.DMatrix(label = y_tr,
                             data = x_tr)
      
      eq_boost <- xgboost(data = temp_tr,
                          nrounds = optim_nrounds,
                          eta = optim_eta,
                          max_depth = max_depth,
                          min_child_weight = min_child_weight,
                          verbose=0)
      
      # Predict on test
      fit <- predict(eq_boost,
                     newdata = x_te,
                     na.action = "na.omit")
      
      # Write summary and advance counter
      summary[count_mod,1] <- max_depth
      summary[count_mod,2] <- min_child_weight
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
    }
  }
  
  # Select best hyperparameters
  summary %<>%
    arrange(RMSE)
  optim_depth <- summary[1,1]
  optim_child <- summary[1,2]
  
  #
  # Step 3: optimization of gamma
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 2))
  colnames(summary) <- c("gamma","RMSE")
  count_mod <- 1 
  
  for (gamma in c(0,0.1,0.3,0.5)){
    
    # Define train and test samples
    x_tr <- x[1:n_test-1,]
    y_tr <- y[1:n_test-1]
    x_te <- x[n_test:n_tot,]
    y_te <- y[n_test:n_tot]
    
    temp_tr <- xgb.DMatrix(label = y_tr,
                           data = x_tr)
    
    eq_boost <- xgboost(data = temp_tr,
                        nrounds = optim_nrounds,
                        eta = optim_eta,
                        max_depth = optim_depth,
                        min_child_weight = optim_child,
                        gamma = gamma,
                        verbose=0)
    
    # Predict on test
    fit <- predict(eq_boost,
                   newdata = x_te,
                   na.action = "na.omit")
    
    # Write summary and advance counter
    summary[count_mod,1] <- gamma
    summary[count_mod,2] <- sqrt(sum((fit - y_te)^2)/length(y_te))
    count_mod <-  count_mod + 1
  }
  
  # Select best hyperparameter
  summary %<>%
    arrange(RMSE)
  optim_gamma <- summary[1,1]
  
  #
  # Step 4: return list of optimal parameters
  #
  
  optim_param <- c(optim_nrounds,
                   optim_eta,
                   optim_depth,
                   optim_child,
                   optim_gamma)
  
  names(optim_param) <- c("nrounds",
                          "eta",
                          "max_depth",
                          "min_child_weight",
                          "gamma")
  
  return(optim_param) 
  
}


# =======================================================================================================================================
# Macroeconomic Random Forest

# Full version
tune_MRF <- function(data.in,n){
  
  # Inputs:
  # data.in = dataset with target in first column and variables in other columns - should be a matrix
  # n = number of periods on which the hyper-parameter should be optimized
  # Output:
  # optim_param = vector of best-performing hyper-parameters in the following order:
  #           1 = n_trees (number of trees - default value = 50)
  #           2 = n_var (number of variables in the linear part)
  #           3 = lambda (ridge shrinkage parameter in the linear part - default value = 0.01)
  #           4 = re_meth (resampling method - default value = 2, goes from 0 to 4)
  #           5 = bl_size (block size for resampling - default value = 12)
  # NB: other variables might be optimized but are not looked at here for the sake of time
  
  print("Tuning macroeconomic random forest")
  
  # Start and end values
  n_tot <- nrow(data.in)
  n_test <- n_tot - n + 1
  
  # Define train and test samples
  data.tr <- data.in[-n_tot,]
  y_te <- data.tr[(n_test-1):(n_tot-1),1]
  data.tr[(n_test-1):(n_tot-1),1] <- NA

  #
  # Step 1: n_trees and n_var
  #

  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("n_trees","n_var","RMSE")
  count_mod <- 1 
  
  m_min <- min(5,ncol(x))
  m_max <- max(ncol(x),9)
    
  for (n_trees in c(25,50)){
    for (n_var in seq(m_min,m_max,by=2)){
      
      # Set seed for reproducibility
      # NB: has to be the same as in Main.R
      set.seed(22122) 
      
      # Fit model on train
      eq_mrf <- MRF(data.tr,
                    B = n_trees,
                    x.pos = c(2:n_var),
                    oos.pos = c((nrow(data.tr)-11):nrow(data.tr)),
                    cheap.look.at.GTVPs = FALSE,
                    printb = FALSE)
      
      # Predict on test
      fit <- eq_mrf$pred
      
      # Write summary and advance counter
      summary[count_mod,1] <- n_trees
      summary[count_mod,2] <- n_var
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
      
    }
  }
  
  # Select best hyper-parameter and return it
  summary %<>%
    arrange(RMSE)
  optim_trees <- summary[1,1]
  optim_var <- summary[1,2]

  
  #
  # Step 2: L2 regularization parameter
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 2))
  colnames(summary) <- c("lambda","RMSE")
  count_mod <- 1 
  
  for (lambda in c(0.001,0.01,0.1)){

    # Set seed for reproducibility
    # NB: has to be the same as in Main.R
    set.seed(22122) 
    
    # Fit model on train
    eq_mrf <- MRF(data.tr,
                  B = optim_trees,
                  x.pos = c(2:optim_var),
                  oos.pos = c((nrow(data.tr)-11):nrow(data.tr)),
                  cheap.look.at.GTVPs = FALSE,
                  printb = FALSE,
                  ridge.lambda = lambda)
    
    # Predict on test
    fit <- eq_mrf$pred
    
    # Write summary and advance counter
    summary[count_mod,1] <- lambda
    summary[count_mod,2] <- sqrt(sum((fit - y_te)^2)/length(y_te))
    count_mod <-  count_mod + 1
    
  }

  # Select best hyper-parameter and return it
  summary %<>%
    arrange(RMSE)
  optim_lambda <- summary[1,1]


  #
  # Step 3: re-sampling
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("re_meth","bl_size","RMSE")
  count_mod <- 1 
  
  for (re_meth in c(2,4)){
    for (bl_size in c(6,12)){
      
      # Set seed for reproducibility
      # NB: has to be the same as in Main.R
      set.seed(22122) 
      
      # Fit model on train
      eq_mrf <- MRF(data.tr,
                    B = optim_trees,
                    x.pos = c(2:optim_var),
                    oos.pos = c((nrow(data.tr)-11):nrow(data.tr)),
                    cheap.look.at.GTVPs = FALSE,
                    printb = FALSE,
                    ridge.lambda = optim_lambda,
                    resampling.opt = re_meth,
                    block.size = bl_size)
      
      # Predict on test
      fit <- eq_mrf$pred
      
      # Write summary and advance counter
      summary[count_mod,1] <- re_meth
      summary[count_mod,2] <- bl_size
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
      
    }
  }
  
  # Select best hyper-parameter and return it
  summary %<>%
    arrange(RMSE)
  optim_meth <- summary[1,1]
  optim_size <- summary[1,2]
  
    
  #
  # Step 4: return list of optimal parameters
  #
  
  optim_param <- c(optim_trees,
                   optim_var,
                   optim_lambda,
                   optim_meth,
                   optim_size)
  
  names(optim_param) <- c("ntrees",
                          "nvar",
                          "ridge",
                          "resampling_method",
                          "block_size")
  
  return(optim_param) 
  
}

# Faster version
tune_MRF_fast <- function(data.in,n){
  
  # Inputs:
  # data.in = dataset with target in first column and variables in other columns - should be a matrix
  # n = number of periods on which the hyper-parameter should be optimized
  # Output:
  # optim_param = vector of best-performing hyper-parameters in the following order:
  #           1 = n_trees (number of trees - default value = 50)
  # NB: other variables can be optimized in non-fast version (and even more in principle)
  
  print("Tuning macroeconomic random forest (fast version)")
  
  # Start and end values
  n_tot <- nrow(data.in)
  n_test <- n_tot - n + 1
  
  # Define train and test samples
  data.tr <- data.in[-n_tot,]
  y_te <- data.tr[(n_test-1):(n_tot-1),1]
  data.tr[(n_test-1):(n_tot-1),1] <- NA
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 2))
  colnames(summary) <- c("n_var","RMSE")
  count_mod <- 1 
  
  m_min <- min(5,ncol(x))
  m_max <- max(ncol(x),9)
  
  for (n_var in seq(m_min,m_max,by=2)){
    
    # Set seed for reproducibility
    # NB: has to be the same as in Main.R
    set.seed(22122) 
    
    # Fit model on train
    eq_mrf <- MRF(data.tr,
                  x.pos = c(2:n_var),
                  oos.pos = c((nrow(data.tr)-11):nrow(data.tr)),
                  cheap.look.at.GTVPs = FALSE,
                  printb = FALSE)
    
    # Predict on test
    fit <- eq_mrf$pred
    
    # Write summary and advance counter
    summary[count_mod,1] <- n_var
    summary[count_mod,2] <- sqrt(sum((fit - y_te)^2)/length(y_te))
    count_mod <-  count_mod + 1
    
  }

  # Select best hyper-parameter and return it
  summary %<>%
    arrange(RMSE)
  optim_param <- summary[1,1]

  names(optim_param) <- c("nvar")
  return(optim_param) 
  
}


# =======================================================================================================================================
# XG Boost - linear

tune_XGBL <- function(x,y,n){

  # Inputs:
  # x = regressors - should be a matrix
  # y = target variable - should be a vector
  # n = number of periods on which the hyperparameter should be optimized
  # Output:
  # optim_param = vector of best-performing hyperparameters in the following order:
  #           1 = nrounds (number of iterations)
  #           2 = eta (learning rate)
  #           3 = alpha (L1 regularization term - default value = 0)

  print("Tuning linear gradient boosting")
  
  # Start and end values
  n_tot <- nrow(x)
  n_test <- n_tot - n + 1
  
  # Define train and test samples
  x_tr <- x[1:n_test-1,]
  y_tr <- y[1:n_test-1]
  x_te <- x[n_test:n_tot,]
  y_te <- y[n_test:n_tot]
  
  temp_tr <- xgb.DMatrix(label = y_tr,
                         data = x_tr)
  
  if(n==1){
    data_te <- t(as.matrix(x_te))
  }else{
    data_te <- as.matrix(x_te)
  }
  
  temp_te <- xgb.DMatrix(label = as.matrix(y_te), 
                         data = data_te)
  
  #
  # Step 1: optimization of nrounds / eta
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 3))
  colnames(summary) <- c("nrounds","eta","RMSE")
  count_mod <- 1 
  
  for (nrounds in seq(from = 10, to = 130, by = 30)){
    for (eta in c(0.05,0.1,0.2,0.3)){

      eq_boost <- xgb.train(data = temp_tr,
                            nrounds = nrounds,
                            verbose = 0,
                            eta = eta,
                            booster = 'gblinear')
      # Predict on test
      fit <- predict(eq_boost,
                     newdata = x_te,
                     na.action = "na.omit")
      
      # Write summary and advance counter
      summary[count_mod,1] <- nrounds
      summary[count_mod,2] <- eta
      summary[count_mod,3] <- sqrt(sum((fit - y_te)^2)/length(y_te))
      count_mod <-  count_mod + 1
    }
  }

  # Select best hyperparameters
  summary %<>%
    arrange(RMSE)
  optim_nrounds <- summary[1,1]
  optim_eta <- summary[1,2]
  
  #
  # Step 2: optimization of alpha
  #
  
  # Summary dataframe
  summary <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = 2))
  colnames(summary) <- c("alpha","RMSE")
  count_mod <- 1
  
  for (alpha in c(0.001,0.01,0.1,0.2,0.3)){
    
    eq_boost <- xgb.train(data = temp_tr,
                          nrounds = optim_nrounds,
                          eta = optim_eta,
                          verbose = 0,
                          alpha = alpha,
                          booster = 'gblinear')
    # Predict on test
    fit <- predict(eq_boost,
                   newdata = x_te,
                   na.action = "na.omit")
    
    # Write summary and advance counter
    summary[count_mod,1] <- alpha
    summary[count_mod,2] <- sqrt(sum((fit - y_te)^2)/length(y_te))
    count_mod <-  count_mod + 1
  }
    
    # Select best hyperparameters
    summary %<>%
      arrange(RMSE)
    optim_alpha <- summary[1,1]

  
  #
  # Step 4: return list of optimal parameters
  #
  
  optim_param <- c(optim_nrounds,
                   optim_eta,
                   optim_alpha)
  
  names(optim_param) <- c("nrounds",
                          "eta",
                          "alpha")
  
  return(optim_param) 

}
