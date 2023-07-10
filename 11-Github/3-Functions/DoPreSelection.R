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
# This is an auxiliary function of MAIN.R, which performs pre-selection
# For a given dataset, it provides the n_var "most informative" variables as estimated by the pre-selection method select_method
# 


pre_select <- function(data_real,ii,horizon,select_method,n_var){
  
  # INPUTS:
  #   data_real = dataset with LHS and RHS variables [data.frame]
  #               NB: it can contain NAs
  #               NB2: in the current version of the code, it should contain d12trade_wd_pt, L1st_d12trade_wd_pt, and L2nd_d12trade_wd_pt
  #   ii = point (date) for trimming the dataset [scalar]
  #   horizon = horizon of back-, now- or forecast [scalar]
  #             NB: negative for a backcasting, 0 for nowcasting and positive for forecasting
  #   select_method = switch on the pre-selection method to use [scalar]
  #               0 = No pre-selection
  #               1 = LARS (Efron et al., 2004)
  #               2 = Correlation-based (SIS: Fan and Lv, 2008)
  #               3 = t-stat based (Bair et al., 2006)
  #               4 = Iterated Bayesian Model Averaging (BMA: Yeung et al., 2005)
  #   n_var = number of variables to be selected [scalar]
  #   fast_bma = switch on whether to use a faster version of the BMA [scalar]
  #          1 = yes (runs less iterations but is way faster)
  #          0 = no (runs all iterations, even those that might be un-needed at the end of the sample)
  # OUTPUT: 
  #   var_sel = list of selected variables 

  
  # Initiating data
  smpl <- head(data_real,ii-horizon-3) %>%
    select(-date,
           -L1st_target,
           -L2nd_target) %>%
    drop_na()
  
  if(select_method==2){
    order <- smpl %>%
      mutate(across(-target,~cor(.,
                                 target,
                                 use = "pairwise.complete.obs",
                                 method  = "pearson"))) %>%
      select(-target) %>%
      distinct() %>%
      t() %>%
      as.data.frame() %>%
      rename(corr=V1)
    
    out_sel <- order %>%
      mutate(corr = abs(corr)) %>%
      arrange(desc(corr)) %>%
      head(n_var) %>%
      mutate(variable=as.character(variable))
    
    var_sel <- out_sel$variable
    
  }else if(select_method==1){
    
    # Creating batches for LARS loop
    batch <- smpl
    count <- 0
    order <- data.frame(V1=c(0))
    
    # LARS loop (have to run in different batchs if number of variables is too high)
    while (count < n_var) {
      
      # Running LARS equation
      x <- as.matrix(select(batch,-target))
      y <- as.matrix(select(batch,target))
      eq <- lars(x = x,
                 y = y,
                 type="lar")
      
      # Ordering the variables
      out <- as.data.frame(coef(eq)) %>%
        summarise(across(everything(),~sum(.==0))) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column('name') %>%
        arrange(V1) %>%
        group_by(V1) %>%
        filter(n()==1) %>%
        column_to_rownames('name')
      
      order <- rbind(order,out)
      
      # Deleting variables already ordered from the sample
      var <- row.names(out)
      batch %<>%
        select(!all_of(var))
      
      # Checking if nrow(out) = 0 to avoid infinite loops
      if (nrow(out)==0){
        # Putting all remaining variables at the end
        x <- as.matrix(select(batch,-target))
        n_end <- ncol(x)
        out <- data.frame(V1=rep(1,n_end))
        rownames(out) <- colnames(x)
        order <- rbind(order,out)
        count <- count + n_end
        print("Warning: Using special procedure for LARS")
      }else{
        # Just updating the count      
        count <- count + nrow(out)
      } 
    }
    
    # Getting the list of selected variables
    out_sel <- out %>%
      head(n_var)
    var_sel <- row.names(out_sel)

  }else if(select_method==0){
    
    order <- smpl %>%
      select(-target)
    
    out_sel <- order
    
    var_sel <- colnames(out_sel)
    
  }else if(select_method==3){
    
    init <- head(data_real,ii-horizon-3) %>%
      select(-date) %>%
      drop_na()
    
    list_var <- colnames(select(smpl,-target))
    order <- data.frame(name_var=NA,
                        tstat=NA)
    
    for (v in seq_along(list_var)){
      
      data_eq <- init %>%
        select(target,
               L1st_target,
               L2nd_target,
               list_var[v])
      
      eq <- lm(target ~ .,
               data = data_eq,
               na.action = "na.omit")
      
      order[v,1] <- list_var[v]
      order[v,2] <- coef(summary(eq))[4, "t value"]
    }
    
    # Order by t-stat (higher to lower)
    order %<>%
      arrange(desc(tstat))
    
    # Getting the list of selected variables
    out_sel <- order %>%
      head(n_var)
    var_sel <- out_sel$name_var

  }else if(select_method==4){
    
    # Hyper-parameters of BMA
    # Max number of variables (maxNvar) should not be > 30 or code will crash
    # NB: for fast BMA, the threshold of posterior probe and number of iterations have been determined empirically
    # This corresponds to value balancing time consumption with accuracy 
    # I.e. after 20 iterations, there are already 20 variables with posterior probability above 20
    # Continuing would just mean removing the variable with lowest probability and testing individually remaining ones (which can be inefficient)
    max_bma <- 20
    th_bma <- 20
    niter_fast_bma <- 20
    
    # Inputs to BMA
    x_all <- select(smpl,-target)
    y <- as.matrix(select(smpl,target))
    var_sel <- c()
    n_var_sel <- 0
    
    while(n_var_sel < n_var){
      
      # Get number of variables in this batch
      var_left <- n_var - n_var_sel
      max_batch <- min(max_bma,var_left)
      
      # Get variables for the batch
      x <- x_all %>%
        select(-all_of(var_sel)) %>%
        as.matrix()
      
      # If max_batch = 1, the do the selection based on sortedX (which is itself based on BMA)
      if(max_batch==1){
        bma <- iBMA.bicreg(x, 
                           y, 
                           thresProbne0 = th_bma,
                           verbose = TRUE,
                           maxNvar = max_batch+1,
                           nIter = 1)
        
        sel_fast <- bma$sortedX
        var_sel <- c(var_sel,colnames(sel_fast)[1])
        
      }else{
        if(fast_bma==1){
          
          # In fast BMA, the number of iterations (nIter) is set at a low value
          # For large datasets, this means some variables might NOT be considered by BMA but:
          # (1) variables are sorted prior to the BMA so after some iterations, variables are likely uninformative
          # (2) empirically this is confirmed: no variables enter the model after some iterations
          # (3) a large nIter results in a highly time-consuming procedure (for little to no gains given 1 and 2)
          bma <- iBMA.bicreg(x, 
                             y, 
                             thresProbne0 = th_bma,
                             verbose = TRUE,
                             maxNvar = max_batch,
                             nIter = niter_fast_bma)
          
          # Select intermediary selection of variables
          sel_fast <- bma$sortedX %>%
            as.data.frame() %>%
            select(all_of(bma$currentSet))
          var_sel <- c(var_sel,colnames(sel_fast))
          
          
        }else if(fast_bma==0){
          
          # Run BMA
          bma <- iBMA.bicreg(x, 
                             y, 
                             thresProbne0 = th_bma,
                             verbose = TRUE,
                             maxNvar = max_batch,
                             nIter = ncol(x))
          
          # Add results to var_sel
          var_sel <- c(var_sel,bma$bma$namesx)
          
        }
      }
      
      # Count number of var selected
      n_var_sel <- length(var_sel)
      
    }
    
    # Outputs
    order <- NA
    out_sel <- NA
    
  }
  
  return(var_sel)
}
