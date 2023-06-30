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
# This is an auxiliary function of MAIN.R, which re-aligns the data to balance the input dataset
# The procedure used is the "vertical realignment" of Altissimo et al. (2006)
# NB1: This is the simplest solution, while also being found to have similar performances as more sophisticated approaches in Marcellino and Schumacher (2010).
# NB2: In addition to the Altissimo et al. (2006) procedure which manages missing data points at the end (by lagging)
#      The code also creates leads of data - where relevant - for "excess" data points at the end.
#      This a symmetric treatment than the vertical realignment.
#      This also creates new series (instead of replacing them) while the above procedure replaces
# Please refer to section 2.1 of the paper for further details on the procedure.
# Where appropriate, please cite the vertical re-alignment as: Altissimo, F., Cristadoro, R., Forni, M., Lippi, M., and Veronese, G. (2006). "New Eurocoin: Tracking Economic Growth in Real Time", CEPR Discussion Papers, No 5633
#

doAlign <- function(data_rmv,horizon){
  
  # Inputs:
  # data_rmv = data-set before re-alignment
  # horizon = horizon of back-, now- or forecast [scalar]
  #           NB: negative for a back-casting, 0 for now-casting and positive for fore-casting
  # Output: 
  # data_real = data re-aligned 

  
  # -----------------------------------
  # 1 - Set date of back-, now-, fore-cast
  # -----------------------------------
  
  # Determine current date (i.e. current month for the forecaster)
  temp <- data_rmv %>%
    select(date) %>%
    drop_na() %>%
    tail(1)
  month_data <- temp[[1,1]]
  
  # Determine date of back-, now-, or fore-cast given horizon
  month_of_data <- month(ymd(month_data))
  year_of_data <- year(ymd(month_data))
  if((month_of_data + horizon)<=0){
    year_cast <- year_of_data - (floor(abs(horizon)/12)+1) 
  }else if((month_of_data + horizon)>12){
    year_cast <- year_of_data + (floor(abs(horizon)/12)+1)
  }else{
    year_cast <- year_of_data
  }
  month_cast <- (month_of_data + horizon) %% 12
  month_cast <- ifelse(month_cast==0,12,month_cast)
  date_cast <- as.character(ymd(paste0(year_cast,"-",month_cast,"-15")))
  
  # Determine position index of date_cast [date of back-, now-, fore-cast]
  n_time <- which(grepl(date_cast, as.character(data_rmv$date))) 
  
  # If no data yet (forecast), create lines of NA at the end of data-set
  if(length(n_time)==0){
    first_date <- data_rmv$date[1]
    data_rmv %<>%
    mutate(date = as_date(as.Date(as.POSIXct(date, origin="1970-01-14")))) %>%
    complete(date = seq.Date(as.Date(first_date), as.Date(date_cast), by="month"))
    n_time <- which(grepl(date_cast, as.character(data_rmv$date)))
  }

  
  # -----------------------------------
  # 2 - Initialize realigned data-set
  # -----------------------------------
  
  # Initialize re-aligned data-set with LHS variable and date
  data_real <- data_rmv %>%
    select(date,
           target)
  
  # Create first and second available lags for LHS variable
  # Get indices of NA positions 
  non_NA_index <- which(!is.na(data_real$target))
  first_non_NA <- min(non_NA_index)
  last_non_NA <- max(non_NA_index)
  diff_na <- last_non_NA - n_time
  
  # Check that diff_na should be strictly negative 
  # Otherwise it means that the user is trying to forecast data already available
  # In the latter case, the code 'deletes' the observation to allow for forecast
  if(diff_na>=0){
    
    # Issue warnings
    warning(paste0("There is already an observation for the horizon ",horizon,". The observation has been deleted to allow for forecasts. But for a more realistic set-up, please change horizons in 'list_h' in MAIN.R"))
    
    # Delete observation(s)
    # In this case we consider this would be the first non-missing
    data_real$target[(n_time-diff_na):n_time] <- NA
    
    # Re-create first and second available lags for LHS variable
    # Get indices of NA positions 
    non_NA_index <- which(!is.na(data_real$target))
    first_non_NA <- min(non_NA_index)
    last_non_NA <- max(non_NA_index)
    diff_na <- last_non_NA - n_time
    
  }
  
  # Create first and second lagged values
  # with no interpolation of first values as would be the case in Altissimo et al. (2006) procedure which applies to RHS variables
  data_real %<>%
  mutate(L1st_target = lag(target,-diff_na),
         L2nd_target = lag(target,-diff_na+1))

  
  # -----------------------------------
  # 3 - Re-align data-set
  # -----------------------------------

  # Create temporary data-set with RHS variables to be re-aligned
  temp <- data_rmv %>%
    select(-date,
           -target)
    
  # Loop over RHS variables to realign
  for (jj in 1:ncol(temp)){
  
    # Select individual variable
    temp_col <- temp[jj]
    
    # Check first and last non-NA positions and difference with n_time
    non_NA_index <- which(!is.na(temp_col))
    first_non_NA <- min(non_NA_index)
    last_non_NA <- max(non_NA_index)
    diff_na <- last_non_NA -  n_time
    
    if(diff_na==0){
      
      # If last non-NA = date of cast, bind directly with realigned data-set 
      data_real <- cbind(data_real,temp_col)
      
    }else if(diff_na<0){
      
      # If last non-NA before date of cast, lag variable and ...
      temp_col <- lag(temp_col,-diff_na)
      
      # ... replace the first NA(s) thereby created by the first available value
      # NB: this follows the procedure Altissimo et al. (2006)
      count <- 1
      while(count<=-diff_na){
        temp_col[first_non_NA + count - 1,1] <- temp_col[first_non_NA - diff_na,1]
        count <- count + 1
      }
      
      # Rename the lagged variable and bind with realigned dataset
      prefix <- paste0("Lg",-diff_na)
      colnames(temp_col) <- paste(prefix, colnames(temp_col), sep = "")
      data_real <- cbind(data_real,temp_col)
      
    }else if(diff_na>0){
      
      # If last non-NA after date of cast, create additional lead variables
      # NB: this is new compared with Altissimo et al. (2006) procedure
      # This applies a sort of "symmetry" for variables with "excess" data points
      temp_col_init <- temp_col
      count <- 1
      name_var <- colnames(temp_col_init)
      while(count<=diff_na){
        colnames_prev <- colnames(temp_col)
        temp_col <- cbind(temp_col,lead(temp_col_init,count))
        new_name <- paste0("Ld",count,name_var)
        colnames(temp_col) <- c(colnames_prev,new_name)
        count <- count + 1
      }
    
      # Bind with realigned dataset 
      data_real <- cbind(data_real,temp_col)
    
    } # End of if loop 
  } # End of for loop
  
  # Check procedure was correct
  # There should be only one NA = the point that the forecaster tries to back-, now-, or fore-cast
  if(sum(is.na(dplyr::slice(data_real,n_time)))>1){
    stop("Re-alignment procedure not successful, please check")  
  }

  
  # -----------------------------------
  # 4 - Return re_aligned data-set
  # -----------------------------------
  
  return(data_real)
  
}