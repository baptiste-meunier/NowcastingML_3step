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
# This is an auxiliary function of MAIN.R, which performs regression
# From a given in-sample and out-of-sample datasets, provides predictions for different models
#


run_regressions <- function(smpl_in,smpl_out,list_methods,n_sel,sc_ml,fast_MRF){
  
  # Inputs:
  # smpl_in = in-sample data [data.frame]
  # y = out-of-sample data [vector]
  #     NB: in the current version of the code, it should contain target, L1st_target, and L2nd_target
  # list_methods = list of regressions techniques to be tested [list of scalars]
  #                NB: the AR benchmark is always performed - regardless of selection
  #            1 = OLS
  #            2 = Markov-switching regression [requires 1]
  #            3 = Quantile regression
  #            4 = Random forest
  #            5 = XG Boost tree
  #            6 = Macroeconomic Random Forest
  #            7 = XG Boost linear
  # n_per = number of periods on which the optimization of the hyper-parameters is performed [scalar]
  # sc_ml = switch on whether data should be scaled for ML methods (0 = no, 1 = yes)
  # fast_MRF = switch on whether to perform a faster tuning of the MRF (0 = no, 1 = yes)
  
  #
  # Output: results = dataframe with the true value and the predictions of each model (plus the AR)
  #         NB: results is of dimension length(list_methods)+2 (the true value, and the AR)
  
  # Prepare results dataset
  results <- data.frame(matrix(NA,
                               nrow = 1,
                               ncol = length(list_methods)+2))
  results[1,1] <- smpl_out[1,2]
  names_col <- c("true_value","ar")
  
  # Number of factors 
  n_fct <- length(smpl_in) - 4
  
  # Train and test samples for ML methods
  y_train <- as.matrix(select(smpl_in,target))[,1]
  y_train_sc <- scale(y_train)[,1]
  x_train <- as.matrix(select(smpl_in,
                              -date,
                              -target,
                              -L1st_target,
                              -L2nd_target))
  x_train_sc <- scale(x_train)
  x_test <- as.matrix(select(smpl_out,
                             -date,
                             -target,
                             -L1st_target,
                             -L2nd_target))
  x_all <- rbind(x_train,x_test)
  x_all_sc <- x_all %>%
    scale()
  x_test_sc <- t(as.matrix(x_all_sc[nrow(x_all_sc),]))

  # Change if scaling
  if(sc_ml==1){
    y_train_ml <- y_train_sc
    x_train_ml <- x_train_sc
    x_test_ml <- x_test_sc
    x_all_ml <- x_all_sc
  }else{
    y_train_ml <- y_train
    x_train_ml <- x_train
    x_test_ml <- x_test
    x_all_ml <- x_all
  }
  
  # AR model
  eq_ar <- lm(target ~ .,
              data = select(smpl_in,
                            target,
                            L1st_target),
              na.action = "na.omit")
  
  results[1,2] <- predict(eq_ar,
                          newdata = select(smpl_out,
                                           L1st_target),
                          na.action = "na.omit")
  
  # Initiating the counter for columns
  count_col <- 3
  
  # OLS
  if(1 %in% list_methods){
    print("Forecasting with OLS")
    eq_lm_allf <- lm(target ~ .,
                     data = select(smpl_in,
                                   target,
                                   5:ncol(smpl_in)),
                     na.action = "na.omit")
    
    results[1,count_col] <- predict(eq_lm_allf,
                                    newdata = select(smpl_out,
                                                     5:ncol(smpl_in)),
                                    na.action = "na.omit")
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_ols") 

  }
  
  # Markov-switching
  if(2 %in% list_methods){
    print("Forecasting with Markov-switching")
    out <- tryCatch({
      eq_ms_allf <- msmFit(eq_lm_allf,
                           k = 2,
                           sw = rep(TRUE,n_fct+2))
      
      temp1 <- as.matrix(eq_ms_allf@Coef[1,])
      temp2 <- as.matrix(eq_ms_allf@Coef[2,])
      temp_var <- as.matrix(select(smpl_out,
                                   5:ncol(smpl_in)))
      temp_var <- cbind(1,temp_var)
      pred_1 <- sum(temp1*temp_var)
      pred_2 <- sum(temp2*temp_var)
      preds <- c(pred_1,pred_2)
      proba <- eq_ms_allf@Fit@smoProb[nrow(eq_ms_allf@Fit@smoProb),]
      proba_trans <- eq_ms_allf@transMat
      if(proba[1]>proba[2]){
        temp_prob <- proba_trans[,1]
        pred <- sum(temp_prob*preds)
      }else{
        temp_prob <- proba_trans[,2]
        pred <- sum(temp_prob*preds)
      }
    },
    error=function(e) {
      return(NA)
    })
    
    if(is.na(out)){
      pred <- predict(eq_lm_allf,
                      newdata = select(smpl_out,
                                       5:ncol(smpl_in)),
                      na.action = "na.omit")
    }
    
    results[1,count_col] <- pred
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_ms") 
  }
  
  # Quantile regression
  if(3 %in% list_methods){
    print("Forecasting with quantile regression")
    eq_qr_allf <- rq(target ~ .,
                     data = select(smpl_in,
                                   target,
                                   5:ncol(smpl_in)),
                     na.action = "na.omit")
    
    results[1,count_col] <- predict(eq_qr_allf,
                                    newdata = select(smpl_out,
                                                     5:ncol(smpl_in)),
                                    na.action = "na.omit")
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_qr") 
  }
  
  # Random forest
  if(4 %in% list_methods){
    
    
    # Calling a tuning function from DoTuning.R
    # The tuning is done on last n_per periods
    param <- tune_RF(x_train_ml,
                     y_train_ml,
                     n_per)

    print("Forecasting with random forest")
    eq_rf <- randomForest(y = y_train_ml,
                          x = x_train_ml,
                          na.action = "na.omit",
                          do.trace = FALSE,
                          ntree = 300,
                          mtry = param[1,1],
                          nodesize = param[1,2],
                          corr.bias = TRUE)

    results[1,count_col] <- predict(eq_rf,
                                    newdata = x_test_ml,
                                    na.action = "na.omit")

    # If need for scaling (when run on "_sc")
    if(sc_ml==1){results[1,count_col] <- results[1,count_col]*sd(y_train) + mean(y_train)}
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_rf") 

  }
  
  # XGBoost tree
  if(5 %in% list_methods){
    
    # Calling a tuning function from DoTuning.R
    # The tuning is done on the last n_per periods
    param <- tune_XGBT(x_train_ml,
                       y_train_ml,
                       n_per)
    
    temp_train <- xgb.DMatrix(label = y_train_ml,
                              data = x_train_ml)
    
    print("Forecasting with gradient boosting (trees)")
    eq_boost <- xgboost(data = temp_train,
                        nrounds = param[1],
                        eta = param[2],
                        max_depth = param[3],
                        min_child_weight = param[4],
                        gamma = param[5],
                        verbose=0)
    
    results[1,count_col] <- predict(eq_boost,
                                    newdata = x_test_ml,
                                    na.action = "na.omit")
    
    # If need for scaling (when run on "_sc")
    if(sc_ml==1){results[1,count_col] <- results[1,count_col]*sd(y_train) + mean(y_train)}
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_xgbt") 
  }
  
  # Macroeconomic Random forest
  if(6 %in% list_methods){
    
    # Prepare data
    data.in <- rbind(cbind(y_train_ml,x_train_ml),cbind(NA,x_test_ml))
    
    # Set seed for reproducibility
    # NB: has to be the same as in DoTuning.R
    set.seed(22122)
    
    if(fast_MRF==0){
    
      # Calling a tuning function from DoTuning.R
      # The tuning is done on the last n_per periods
      param <- tune_MRF(data.in,
                        n_per)
      
      print("Forecasting with macroeconomic random forest")
      eq_mrf <- MRF(data.in,
                    B = param[1],
                    x.pos = c(2:param[2]),
                    oos.pos = nrow(data.in),
                    cheap.look.at.GTVPs = FALSE,
                    ridge.lambda = param[3],
                    resampling.opt = param[4],
                    block.size = param[5],
                    printb = FALSE)
      
    }else if(fast_MRF==1){
      
      # Calling a tuning function from DoTuning.R
      # The tuning is done on the last n_per periods
      param <- tune_MRF_fast(data.in,
                             n_per)
      
      print("Forecasting with macroeconomic random forest")
      eq_mrf <- MRF(data.in,
                    x.pos = c(2:param),
                    oos.pos = nrow(data.in),
                    cheap.look.at.GTVPs = FALSE,
                    printb = FALSE)
      
    }
    
    results[1,count_col] <- eq_mrf$pred
    if(sc_ml==1){results[1,count_col] <- results[1,count_col]*sd(y_train) + mean(y_train)}
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_mrf") 
    
  }
  
  # XGBoost linear
  if(7 %in% list_methods){
    
    # Calling a tuning function from DoTuning.R
    # The tuning is done on the last n_per periods
    param <- tune_XGBL(x_train_ml,
                       y_train_ml,
                       n_per)
    
    print("Forecasting with gradient linear boosting")
    temp_train <- xgb.DMatrix(label = y_train_ml,
                              data = x_train_ml)
    
    eq_boost <- xgboost(data = temp_train,
                        nrounds = as.numeric(param[1]),
                        eta = as.numeric(param[2]),
                        alpha = as.numeric(param[3]),
                        booster = 'gblinear',
                        verbose = 0)

    results[1,count_col] <- predict(eq_boost,
                                    newdata = x_test_ml,
                                    na.action = "na.omit")
    
    # If need for scaling (when run on "_sc")
    if(sc_ml==1){results[1,count_col] <- results[1,count_col]*sd(y_train) + mean(y_train)}
    
    count_col <- count_col + 1
    names_col <- c(names_col,"pred_xgbl") 
  }
  

  colnames(results) <- names_col
  return(results)
    
}