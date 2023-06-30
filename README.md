# Description
The code is a user-friendly version of the code used in Chinn et al. (2023), allowing to forecast using the three-step approach. Please note this is a <b> simplified </b> version. For more details over the three-step approach and the possible methods at each step, please refer to Chinn, M. D., Meunier, B., Stumpner, S. (2023). "Nowcasting World Trade with Machine Learning: a Three-Step Approach", NBER Working Paper, No 31419, National Bureau of Economic Research

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
- each row (>3) corresponds to a regression technique. Values indicate the out-of-sample RMSE.
