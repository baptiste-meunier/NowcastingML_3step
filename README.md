# Description
The code is a user-friendly version of the code used in Chinn et al. (2023), allowing to forecast using the three-step approach. Please note this is a <b> simplified </b> version. 

For more details over the three-step approach and the possible methods at each step, please refer to Chinn, M. D., Meunier, B., Stumpner, S. (2023). "Nowcasting World Trade with Machine Learning: a Three-Step Approach", NBER Working Paper, No 31419, National Bureau of Economic Research

# How to run the code?
The code works by running <i> Main.R </i>. The user only needs to provide an input dataset and to select the settings (methods, periods, horizons) of the three-step approach.

The input dataset contains the target variable and a set of potential regressors. It should be located in the folder ‘1-Inputs’. An example input dataset with randomly generated series is provided to give a broad idea of the required data structure. The input dataset should: 
- include a variable named 'target' (= world CPB trade in Chinn et al., 2023). NB: Please do NOT include lags of 'target' (two lags are created automatically in the code).
- include a variable named 'date' which must:
  - be at monthly frequency (other frequencies can be supported with a slight adaptation of the code)
  - be encoded in the format YYYY-MM-15 (NB: please put 15 as day of the month)
- have variables already transformed (no transformation is performed in the code)

Different sorts of input variables are supported including:
- series with leading and trailing missing values: see for example, first 25 variables (columns C to AA) in the example dataset
- series with random missing observations: see for example, last 20 variables (columns CE to CX) in the example dataset
- non-stationary series: see for example, 3 variables named <i> 'nonstat' </i> (columns CY to DA) in the example dataset (respectively a RW, a RW with drift, and a RW with linear trend)

Settings of the three-step approach should be entered directly in <i> Main.R </i> which provides a detailed description of each required user setting. In a nutshell, the user can produce forecasts and evaluate models by:
- selecting one (or several) method(s) for pre-selection 
- selecting one (or several) method(s) for regression
- selecting one (or several) horizon(s) for forecast
- setting the period for out-of-sample predictions (on which models are evaluated)
- setting the starting date for in-sample estimation

<b> NB: </b> for pre-selection, the user must also specify the number of variables that are kept (=60 in Chinn et al., 2023).

# What are the outputs of the code?
Outputs are created in the folder '2-Output' with sub-folders by forecast horizons (e.g. folder 'h0' for a nowcast).
Three types of Excel outputs are provided for each run of <i> Main.R </i>. The type of output is indicated by the name:
- 'pred' = out-of-sample predictions at each point in time. Columns are:
  - 'date' = date of the actual data (NOT the date of the forecast)
  - 'true_value' = actual data
  - one column for each regression technique
- 'rmse' = out-of-sample RMSE, separated between crisis (2008-2009 and 2020-2021) and non-crisis (other years)
- 'summaryALL' = summary of out-of-sample RMSE. The structure is: 
  - second row corresponds to the pre-selection technique used
  - Third row corresponds to the number of variables kept after pre-selection
  - each row (>3) corresponds to a regression technique. Values indicate the out-of-sample RMSE
 
 <b> NB: </b> Each run of <i> Main.R </i> creates one ‘pred’ Excel and one ‘rmse’ Excel for each pre-selection technique tested. It also creates different Excels for each number of variables kept. For example, if the user tests LARS and SIS with 50 and 60 variables kept, four different ‘pred’ and four different ‘rmse’ Excels will be created (LARS-50, LARS-60, SIS-50, SIS-60). By contrast, only a single 'summaryALL' Excel is created at each run of <i> Main.R </i>.

Besides the type of output, the name of the Excel indicates the settings of the three-step approach:
- '_sel_xxx_' = xxx relates to the pre-selection technique(s) used 
- '_n_yyy_' = yyy relates to the number of variables kept after pre-selection 
- '_reg_zzz_' = zzz relates to the regression technique(s) used
- '_h_aaa_' = aaa relates to the horizon of forecast
- last items in the name of the Excel refer to start and end dates of the out-of-sample forecasts

Examples of output Excel are provided in the folder ‘2-Output’.

# How to cite the code?
Please cite as Chinn, M. D., Meunier, B., Stumpner, S. (2023). "Nowcasting World Trade with Machine Learning: a Three-Step Approach", NBER Working Paper, No 31419, National Bureau of Economic Research 

Where appropriate, please also cite
- Macroeconomic random forest as: Goulet-Coulombe, P. (2020). “The Macroeconomy as a Random Forest”, arXiv pre-print
- Linear gradient boosting as: Chen, T., and Guestrin, C. (2016). “XGBoost: A Scalable Tree Boosting System”, in Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, pp. 785–794
- Least Angle Regression (LARS) as: Efron, B., Hastie, T., Johnstone, I., and Tibshirani, R. (2004). “Least angle regression”, Annals of Statistics, 32(2), pp. 407–499
- Sure Independence Screening (SIS) as: Fan, J., and Lv, J. (2008). “Sure independence screening for ultrahigh dimensional feature space”, Journal of the Royal Statistical Society Series B, 70(5), pp. 849–911
- T-stat-based pre-selection as: Bair, E., Hastie, T., Paul, D., and Tibshirani, R. (2006). “Prediction by supervised principal components”, Journal of the American Statistical Association, 101(473), pp. 119–137
- Iterated Bayesian Model Averaging as: Yeung, K., Bumgarner, R., and Raftery, A. (2005). “Bayesian Model Averaging: Development of an improved multi-class, gene selection and classification tool for microarray data”, Bioinformatics, 21(10), pp. 2394–2402
