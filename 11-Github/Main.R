# Code from: Chinn, M. D., Meunier, B., Stumpner, S. (2023). 
#            "Nowcasting World Trade with Machine Learning: a Three-Step Approach", 
#            NBER Working Paper, No 31419, National Bureau of Economic Research
#
# Contact: baptistemeunier@hotmail.fr
# Version: 30/06/2023
#
#
# DESCRIPTION:
# The code is an user-friendly version of the code used in Chinn et al. (2023), allowing to forecast using the (simplified) three-step approach. 
# The user needs to provide a dataset containing the target variable and a set of potential regressors. An example dataset with randomly-generated series is provided in the folder "1-Inputs".
# The user can produce forecasts and evaluate models then by:
#     - selecting one (or several) method(s) for pre-selection 
#     - selecting one (or several) method(s) for regression
#     - setting periods for out-of-sample predictions
#     - setting one (or several) horizon(s) for forecast
# This is a simplified version of the code used in Chinn et al. (2023). Main differences relate to:
#     - Factors are extracted with PCA (other methods are NOT available)
#     - The number of factors is selected with the Kaiser (1961) criterion instead of Bai and Ng (2002)
#
#
# INPUTS:
#     - 'name_input' = name of the dataset with the target variable and regressors
#       It should be located in the folder "1-Inputs".
#       An example dataset is provided in "1-Inputs" to give an idea of the required data structure. 
#       The dataset must include: 
#           o A variable named 'target' (= world CPB trade in Chinn et al., 2023)
#             NB: Please do NOT include lags of 'target' (two lags are created automatically)
#           o A variable named 'date' which must:
#                 + be at monthly frequency (other frequencies can be supported with a slight adaptation of the code)
#                 + be encoded in the format YYYY-MM-15 (NB: please put 15 as day of the month)
#           o Variables already transformed (no transformation is performed in the code)
#       Different sorts of input variables are supported including:
#           o Data with leading and trailing missing values: see for example, first 25 variables (columns C to AA) in the example dataset
#           o Data with random missing observations: see for example, last 20 variables (columns CE to CX) in the example dataset
#           o Non-stationary data: see for example, 3 variables named 'nonstat' (columns CY to DA) in the example dataset (respectively a RW, a RW with drift, and a RW with linear trend)
#             NB: non-stationary variables are automatically discarded during cleaning (see 'DoClean' function where an ADF test is performed)
#     - 'min_start_date' = Minimum starting date. Variables starting after this date will be discarded
#     - 'start_date_oos' / 'end_date_oos' = Start and end dates for out-of-sample forecasts
#     - 'list_h' = List of forecast horizons to be used
#     - 'list_methods' = List of pre-selection techniques to be used. See details below
#     - 'list_n' = List of the number of variables kept after pre-selection
#     - 'list_reg' = List of regressions techniques to be used. See details below
#
# OPTIONAL INPUTS:
#     - 'do_factors' = Switch on whether to perform factor extraction or not. Default = YES (1)
#     - 'fast_bma' = Switch on whether to perform a fast BMA if this method is selected for pre-selection. Default = YES (1)
#     - 'n_per' = Number of months on which the validation of hyper-parameters is performed. Default = 12.
#     - 'sc_ml' = Switch on whether to scale variables before performing machine learning. Default = YES (1)
#     - 'fast_MRF' = Switch on whether to perform a fast hyper-tuning of macroeconomic random forest if this method is selected for regression. Default = YES (1)
#
# OUTPUTS: 
# Outputs are created in the folder '2-Output' with sub-folders by forecast horizons (e.g. folder 'h0' for a nowcast)
# Three types of Excel outputs are provided for each run. The type of output is indicated by the name:
#     - 'pred' = out-of-sample predictions at each point in time. Columns are:
#           o 'date' = date of the actual data (NOT the date of the forecast)
#           o 'true_value' = actual data
#           o one column for each regression technique
#       NB: One different 'pred' Excel is created for each combination of pre-selection technique / number of variables kept after pre-selection
#           Hence the number of 'pred' Excel  created at each run is 'list_methods' times 'list_n'
#     - 'rmse' = out-of-sample RMSE, separated between crisis (2008-2009 and 2020-2021) and non-crisis (other years)
#       NB: One different 'rmse' Excel is created for each combination of pre-selection technique / number of variables kept after pre-selection
#           Hence the number of 'rmse' Excel created at each run is 'list_methods' times 'list_n'
#     - 'summaryALL' = summary of out-of-sample RMSE. The structure is: 
#           o Second row corresponds to the pre-selection technique used (from 0 to 4, see below)
#           o Third row corresponds to the number of variables kept after pre-selection
#           o Each row (>3) correspond to a technique. Values indicate the out-of-sample RMSE.
#       NB: Only one 'summaryALL' Excel is created at each run.
# Beyond the type of the output, the name of the Excel indicates:
#     - '_sel_xxx_' = xxx relates to the pre-selection technique(s) used (from 0 to 4, see below) 
#     - '_n_yyy_' = yyy relates to the number of variables kept after pre-selection 
#     - '_reg_zzz_' = zzz relates to the regression technique(s) used (from 1 to 6, see below) 
#     - '_h_aaa_' = aaa relates to the horizon of forecast
#     - last items in the name of the Excel refer to start and end dates of the out-of-sample forecasts
#
# 
# Where appropriate, please cite:
#     - Macroeconomic random forest as: Goulet-Coulombe, P. (2020). "The Macroeconomy as a Random Forest", arXiv pre-print
#     - Linear gradient boosting as: Chen, T., and Guestrin, C. (2016). "XGBoost: A Scalable Tree Boosting System", in Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, pp. 785-794
#     - Least Angle Regression (LARS) as: Efron, B., Hastie, T., Johnstone, I., and Tibshirani, R. (2004). "Least angle regression", Annals of Statistics, 32(2), pp. 407-499
#     - Sure Independence Screening (SIS) as: Fan, J., and Lv, J. (2008). "Sure independence screening for ultrahigh dimensional feature space", Journal of the Royal Statistical Society Series B, 70(5), pp. 849-911
#     - T-stat-based pre-selection as: Bair, E., Hastie, T., Paul, D., and Tibshirani, R. (2006). "Prediction by supervised principal components", Journal of the American Statistical Association, 101(473), pp. 119-137
#     - Iterated Bayesian Model Averaging as: Yeung, K., Bumgarner, R., and Raftery, A. (2005). "Bayesian Model Averaging: Development of an improved multi-class, gene selection and classification tool for microarray data", Bioinformatics, 21(10), pp. 2394-2402
#
#

rm(list = ls())


# ----------------------------------------------------------------------------
# STEP 0 - SET USER PARAMETERS FOR THE HORSERACE
# ----------------------------------------------------------------------------

# User parameters - part 1
# General settings
name_input <- "data_example.xlsx"       # Name of the input dataset
                                        # NB1: it should be located in folder "1-Inputs"
                                        # NB2: it should be located in folder "1-Inputs"
min_start_date <- "2001-01-15"          # Minimum start date for variables (otherwise discarded)
                                        # NB1: should be in "YYYY-MM-15" format 
                                        # NB2: pay attention to put "15" for the day
start_date_oos <- "2012-01-15"          # Start date for OOS predictions
end_date_oos <- "2022-04-15"            # End date for OOS predictions
                                        # User to check that LHS data is available for this date
                                        # These dates relate to the date of actual data, NOT of the forecast (e.g. a one-month-ahead forecast made in Dec. 2021 will account for Jan. 2022 - i.e. the forecasted date)


# User parameters - part 2
# Combinations for three-step approach
list_h <- c(-2,-1)                      # List of horizons for back-, now- or fore-cast takes place
                                        # Negative for a back-cast, 0 for a now-cast and positive for a fore-cast
                                        # Selecting only one value is possible
list_methods <- c(1,2)                  # List of pre-selection methods
                                        # 0 = No pre-selection
                                        # 1 = LARS (Efron et al., 2004)
                                        # 2 = Correlation-based (SIS: Fan and Lv, 2008)
                                        # 3 = t-stat based (Bair et al., 2006)
                                        # 4 = Iterated Bayesian Model Averaging (BMA: Yeung et al., 2005)
                                        # Selecting only one value is possible
list_n <- c(40,60)                      # List of number of variables kept after pre-selection
list_reg <- c(1,7)                      # List of regressions techniques
                                        # The AR benchmark is always performed - regardless of selection
                                        # 1 = OLS
                                        # 2 = Markov-switching regression [requires 1]
                                        # 3 = Quantile regression
                                        # 4 = Random forest
                                        # 5 = XG Boost tree
                                        # 6 = Macroeconomic Random Forest
                                        # 7 = XG Boost linear

# User parameters - 3
# Optional inputs (calibration)
do_factors <- 1                         # Switch on whether to do factors or not
                                        # 0 = no factors
                                        # 1 = factors
fast_bma <- 1                           # 1 = fast version - i.e. runs with less iterations
                                        # 0 = full number of iterations
n_per <- 12                             # Number of periods (last available ones) on which the optimization of the hyper-parameters for ML is performed
sc_ml <- 1                              # Switch on whether the data should be scaled for ML methods (0 = no, 1 = yes)
fast_MRF <- 1                           # 1 = fast tuning only on number of variables in linear part
                                        # 0 = full tuning on 5 hyper-parameters (see code)


# ----------------------------------------------------------------------------
# STEP 1 - LOAD UTILITIES
# ----------------------------------------------------------------------------

# Load packages
library(data.table)
library(purrr)
library(tidyverse)
library(readr)
library(lars)
library(readxl)
library(writexl)
library(haven)
library(magrittr)
library(haven)
library(lubridate)
library(FactoMineR)
library(factoextra)
library(randomForest)
library(glmnet)
library(xgboost)
library(keras)
library(tensorflow)
library(quantreg)
library(MSwM)
library(BMA)
library(zoo)
library(tseries)
library(doBy)
library(MacroRF)

# Load functions
source("3-Functions/DoClean.R")         # Program cleaning the dataset (interpolation of missing data, elimination of variables starting after min_start_date)
source("3-Functions/DoAlign.R")         # Program doing the re-alignment - see section 2.1 of Chinn et al. (2023)
source("3-Functions/DoPreSelection.R")  # Program doing the pre-selection - see section 1.3 of Chinn et al. (2023)
source("3-Functions/DoRegression.R")    # Program doing the regressions - see section 1.5 of Chinn et al. (2023)
source("3-Functions/DoTuning.R")        # Program doing the tuning of hyper-parameters for machine learning methods


# ----------------------------------------------------------------------------
# STEP 2 - READ (AND CLEAN) THE USER-PROVIDED DATASET
# ----------------------------------------------------------------------------

# Read data
data_init <- read_excel(paste0("1-Inputs/",name_input)) %>%
  mutate(date=ymd(date))

# Clean data-set
data_rmv <- doClean(data_init,min_start_date)


# ----------------------------------------------------------------------------
# STEP 3 - PERFORM THE HORSERACE (LOOP OVER USER PARAMETERS)
# ----------------------------------------------------------------------------

# Loop over user-defined horizons
for (hh in 1:length(list_h)){
  
  # Get horizon to test
  horizon = as.numeric(list_h[hh])
  
  # Get re-aligned dataset
  data_real <- doAlign(data_rmv,horizon)
  
  # Initializing the output
  summary_ps_meth <- data.frame(NA)

  # Loop over user-defined methods
  for (mm in 1:length(list_methods)){
    
    # Initializing the output of the results comparison exercise
    select_method = as.numeric(list_methods[mm])

    # Loop over user-defined number of variables
    for (nn in 1:length(list_n)){
      
      # Get values of the test
      n_var = as.numeric(list_n[nn])
      
      # Print
      print('======================================================================================================')
      print(paste0("HORIZON = ",horizon))
      print(paste0("METHOD = ",select_method))
      print(paste0("NUMBER OF VARIABLES = ",n_var))
      print('======================================================================================================')
      
      # Determine starting and ending dates for out-of-sample exercise
      n_start <- which(grepl(start_date_oos, as.character(data_real$date)))
      n_end <- which(grepl(end_date_oos, as.character(data_real$date)))
      
      # Prepare results dataset
      results <- data.frame(matrix(NA,
                                   nrow = n_end - n_start +1,
                                   ncol = length(list_reg)+3))

      # Loop over dates for out-of-sample predictions
      for (ii in n_start:n_end){

        # Data and dates
        data_all <- head(data_real,ii)
        date_ii <- data_all$date[ii]
        year <- substr(date_ii,1,4)
        month <- substr(date_ii,6,7)
      
        # Print
        print(paste0("Doing out-of-sample predictions for ",year," at month ",month))
        
        
        # --------------------------------------------------------------------
        # STEP A: Pre-selection
        # --------------------------------------------------------------------
        
        # Run pre-selection function
        var_sel <- pre_select(data_real,ii,horizon,select_method,n_var)
        
        # Check that the number of variables is correct
        if(length(var_sel)!=n_var && select_method != 2){
          stop("Pre-selection step did not work, please check")  
        }
        
        # Get LHS and RHS datasets with only the variables from the pre-selection
        rhs_sel <- data_all %>%
          select(date,all_of(var_sel))
        
        lhs_sel <- data_all %>%
          select(date,
                 target,
                 L1st_target,
                 L2nd_target)
        
        # Cleaning
        rm(var_sel)
        
        # --------------------------------------------------------------------
        # STEP B: Factor extraction (PCA) on pre-selected variables
        # --------------------------------------------------------------------
        
        # Clean NAs in the RHS data
        x_pca <- rhs_sel %>%
          drop_na()
        
        # Clean the NAs also in LHS 
        start_date <- as.Date(x_pca$date[1])
        end_date <- as.Date(x_pca$date[nrow(x_pca)])
        
        lhs_sel %<>%
          filter(date>=start_date) %>%
          filter(date<=end_date)
        
        if(do_factors==1){
        
          # Run PCA
          x_pca %<>% select(-date)
          
          res_pca <- PCA(X = x_pca,
                        scale = TRUE,
                        graph = FALSE,
                        ncp = length(x_pca))
          
          rhs_fct <- res_pca$ind$coord %>%
            as.data.table()
          
          # Keep only the factors with eigen values > 1 = rule of the thumb of Kaiser (1961)
          # If eigen value > 1 then explain more variance than single variable
          n_fct <- dim(rhs_fct)[2]
          rhs_fct_sel <- rhs_fct %>%
            select(1:all_of(n_fct))
          
          # Create final dataset for regression
          don_cb <- cbind(lhs_sel, rhs_fct_sel)
          
          # Cleaning
          rm(eig_val)
          rm(res_pca)
          rm(rhs_fct)
          rm(rhs_fct_sel)
          rm(n_fct)
          
        }else{
          
          # Create alternative dataset without factors and CSV output
          don_cb <- cbind(lhs_sel,select(x_pca,-date))
          
        }
        
        # Cleaning
        rm(rhs_sel)
        rm(end_date)
        rm(start_date)
        rm(lhs_sel)
        rm(x_pca)

        
        # --------------------------------------------------------------------
        # STEP C: Regression on factors
        # --------------------------------------------------------------------
        
        # Get current date
        don_reg <- don_cb %>%
          drop_na()
        n_date_ii <- which(grepl(date_ii, as.character(don_reg$date)))
        
        # Define datasets in- and out-of-sample
        smpl_in <- head(don_reg,n_date_ii-horizon-3)
        smpl_out <- dplyr::slice(don_reg,n_date_ii)
        results[ii-n_start+1,1] <- date_ii
        
        # Run regressions
        temp_res <- run_regressions(smpl_in,
                                    smpl_out,
                                    list_reg,
                                    n_sel,
                                    sc_ml,
                                    fast_MRF)
        
        # Write results  
        results[ii-n_start+1,2:length(results)] <- temp_res
        colnames(results) <- c('date',colnames(temp_res))
      
      }
      
      # Create mean of predictions (excluding AR)
      results %<>%
        mutate(pred_mean = rowMeans(select(results,starts_with("pred_"))),
               date = as_date(as.Date(as.POSIXct(date*24*60*60, origin="1970-01-01"))),
               year = year(date))
      
      # Create directory
      dir.create(file.path("./2-Output"), showWarnings = FALSE)
      dir.create(file.path(paste0("./2-Output/h",horizon)), showWarnings = FALSE)
      
      # Write results (predictions)
      write.csv(select(results,-year), file = paste0("./2-Output/",
                                              paste0("h",horizon,"/"),
                                              "pred_sel_",
                                              select_method,
                                              "_n_",
                                              n_var,
                                              "_reg_",
                                              paste(list_reg, collapse='_'),
                                              "_h_",
                                              horizon,
                                              "_",
                                              start_date_oos,
                                              "_",
                                              end_date_oos,
                                              ".csv"),
                row.names=FALSE)
      
      # Summarizing and writing aggregate results
      err <- function(X,Y){sqrt(sum((X-Y)^2)/length(Y))}
      
      total <- results %>%
        select(-date,-year) %>%
        apply(2,err,Y=results[,2])
      
      crisis <- results %>%
        filter((year %in% c(2008,2009,2020,2021))) %>%
        select(-date,-year)
      
      crisis <- apply(crisis,2,err,Y=crisis[,1])
      
      normal <- results %>%
        filter(!(year %in% c(2008,2009,2020,2021))) %>%
        select(-date,-year)
      
      normal <- apply(normal,2,err,Y=normal[,1])
      
      summary_all <- cbind(cbind(total,crisis),normal) %>%
        as.data.frame()
      summary_all <- summary_all[!(row.names(summary_all) %in% c('true_value')),]
      
      # Writing results
      # Results are written for RMSE on crisis sample (2008-2009 and 2020-2021) and non-crisis sample
      write.csv(summary_all, file = paste0("./2-Output/",
                                           paste0("h",horizon,"/"),
                                           "rmse_sel_",
                                           select_method,
                                           "_n_",
                                           n_var,
                                           "_reg_",
                                           paste(list_reg, collapse='_'),
                                           "_h_",
                                           horizon,
                                           "_",
                                           start_date_oos,
                                           "_",
                                           end_date_oos,
                                           ".csv"))
      
      # Write results to the summary
      tot_meth <- length(list_methods)
      num_col <- 1+(mm-1)*tot_meth+nn
      summary_ps_meth[1,num_col] <- select_method
      summary_ps_meth[2,num_col] <- n_var
      summary_ps_meth[3:(nrow(summary_all)+2),num_col] <- summary_all$total
      summary_ps_meth[3:(nrow(summary_all)+2),1] <- rownames(summary_all)
      
    } # End of loop on number of variables
  } # End of loop on pre-selection method
  
  # Write results
  # This is a summary of all the tested methods
  summary_ps_meth[1,1] <- "pre-selection"
  summary_ps_meth[2,1] <- "nb variables"
  write.csv(summary_ps_meth, file = paste0("./2-Output/",
                                           paste0("h",horizon,"/"),
                                           "summaryALL_sel_",
                                           paste(list_methods,collapse='_'),
                                           "_n_",
                                           paste(list_n,collapse='_'),
                                           "_reg_",
                                           paste(list_reg, collapse='_'),
                                           "_h_",
                                           horizon,
                                           "_",
                                           start_date_oos,
                                           "_",
                                           end_date_oos,
                                           ".csv"),
            row.names=FALSE)
  
} # End of loop on horizon
