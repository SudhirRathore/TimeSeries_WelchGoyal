title: "CaseStudy1"
author: "Sudhir"
date: "2023-11-17"
output: html_document

  #LibraryImport
list.files(path = "../input")
library(readr)
library(timeSeries)
library(tseries)
library(hrbrthemes)
library(tidyr)
library(leaps)
#load packages
pacman::p_load(dplyr,
               TSstudio,
               here,
               purrr,
               readxl,
               magrittr,
               ggplot2,
               gridExtra,
               forecast,
               rlang,
               quantmod,
               tsibble,
               lubridate,
               zoo,
               stats,#for acf/pcf
               gridExtra,
               lmSubsets,
               xtable) 






#DataPreprocessing
DataMonthly <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet = "Monthly")
DataQuarterly <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet ="Quarterly" )
DataAnnual <- read_xlsx(path = "/kaggle/input/casestudydataset/PredictorData2022.xlsx", sheet = "Annual")

new_colnames_dict <- c(
  "Time" = "yyyymm",
  "Time" = "yyyyq",
  "Time" = "yyyy",
  "SP500_Index" = "Index",
  "Dividends_12MonthSum" = "D12",
  "Earnings_12MonthSum" = "E12",
  "BookToMarket_Ratio" = "b/m",
  "TreasuryBill_Rate" = "tbl",
  "AAA_CorporateBond_Yield" = "AAA",
  "BAA_CorporateBond_Yield" = "BAA",
  "LongTermGovBond_Yield" = "lty",
  "NetEquityExpansion" = "ntis",
  "RiskFree_Rate" = "Rfree",
  "Inflation_CPI" = "infl",
  "LongTermRateOfReturn" = "ltr",
  "CorporateBond_Return" = "corpr",
  "StockVariance" = "svar",
  "CrossSectionalPremium" = "csp",
  "ConsumptionWealthIncomeRatio" = "cay", #check these variables
  "InvestmentCapitalRatio" = "ik",
  "Dividend3YearPriceRatio" = "D3",
  "Earnings3YearPriceRatio" = "E3",
  "ConsumptionWealthIncomeRatioMonthly" = "caym",
  "PercentageEquityIssuing" = "eqis"
)

filtered_dict_monthly <- new_colnames_dict[new_colnames_dict %in% colnames(DataMonthly)]
DataMonthly <- rename(DataMonthly, !!!filtered_dict_monthly)

filtered_dict_quarterly <- new_colnames_dict[new_colnames_dict %in% colnames(DataQuarterly)]
DataQuarterly <- rename(DataQuarterly, !!!filtered_dict_quarterly)

filtered_dict_annual <- new_colnames_dict[new_colnames_dict %in% colnames(DataAnnual)]
DataAnnual <- rename(DataAnnual, !!!filtered_dict_annual)

for(i in names(DataMonthly)) {
  if(class(DataMonthly[[i]]) == "character") {
    DataMonthly[[i]] <- as.numeric(DataMonthly[[i]])
  }
}

for(i in names(DataQuarterly)) {
  if(class(DataQuarterly[[i]]) == "character") {
    DataQuarterly[[i]] <- as.numeric(DataQuarterly[[i]])
  }
}



for(i in names(DataAnnual)) {
  if(class(DataAnnual[[i]]) == "character") {
    DataAnnual[[i]] <- as.numeric(DataAnnual[[i]])
  }
}


DataMonthly$Time <- zoo::as.yearmon(as.character(DataMonthly$Time), format="%Y%m")
DataQuarterly$Time <- zoo::as.yearqtr(as.character(DataQuarterly$Time), format="%Y%q")
DataAnnual$Time <- zoo::as.yearmon(paste0(as.character(DataAnnual$Time), "-01"), format="%Y-%m")

DataMonthly %<>% select(-CRSP_SPvw, -CRSP_SPvwx)
DataQuarterly %<>% select(-CRSP_SPvw, -CRSP_SPvwx)
DataAnnual %<>% select(-CRSP_SPvw, -CRSP_SPvwx)


#Calculate Returns
calculate_returns <- function(input){
  input %<>% mutate(returns = as.vector(quantmod::Delt(SP500_Index))) 
  input %<>% mutate(ExcessReturn = returns - RiskFree_Rate)
  input %<>% select(-returns, -RiskFree_Rate, -SP500_Index)
  return(input)
}

# Lag Predictors
lag_predictors <- function(input){
  output <- input %>%
    mutate(across(-c(Time, ExcessReturn), lag, .names = "lag_{.col}")) %>% 
    select(Time, ExcessReturn, starts_with("lag_")) %>% 
    slice(-1) 
  return(output)
}

# Function for fitting Linear Model with all predictors

# Data Preprocessing


DataMonthly %<>% calculate_returns()
DataQuarterly %<>% calculate_returns()
DataAnnual %<>% calculate_returns()


DataMonthly %<>% lag_predictors()
DataQuarterly %<>% lag_predictors()
DataAnnual %<>% lag_predictors()




#Excess Return Plot
excess_return_plot <- function(data) {
  options(repr.plot.width = 14, repr.plot.height = 6)
  
  ggplot(data, aes(y = ExcessReturn, x = Time)) +
    geom_line(color = "blue", linewidth = 0.5, linetype = 1) +
    theme_ipsum() +
    ggtitle("Excess Return Time Series")
}
excess_return_plot(DataMonthly)
excess_return_plot(DataQuarterly)
excess_return_plot(DataAnnual)






#ACF and PACF Plot
forecast::Acf(DataMonthly$ExcessReturn %>% na.omit(), main="ACF for Monthly Data", lag.max = NULL)
forecast::Pacf(DataMonthly$ExcessReturn %>% na.omit(), main="PACF for Monthly Data", lag.max = NULL)


forecast::Acf(DataQuarterly$ExcessReturn %>% na.omit(), main="ACF for Quarterly Data")
forecast::Pacf(DataQuarterly$ExcessReturn %>% na.omit(), main="PACF for Quarterly Data")


forecast::Acf(DataAnnual$ExcessReturn %>% na.omit(), main="ACF for Annual Data")
forecast::Pacf(DataAnnual$ExcessReturn %>% na.omit(), main="PACF for Annual Data")








#Information Criteria Plot Function and monthly plot
# Function to fit ARIMA models and calculate information criteria

fit_arima_and_info_criteria <- function(x, p) {
  arima_model <- arima(x, order = c(p, 0, 0))
  
  loglik <- sum(log(dnorm(x - arima_model$resid)))
  n <- length(x)
  
  aic <- AIC(arima_model)
  bic <- BIC(arima_model)
  
  info_df <- data.frame(p = p, AIC = aic, BIC = bic)
  return(info_df)
}


# Fit ARIMA models for different values of p
p_max <- 10  # Set the maximum order
info_criteria_list <- lapply(1:p_max, function(p) fit_arima_and_info_criteria(DataMonthly$ExcessReturn, p))

# Combine the results into a data frame
info_criteria_df <- do.call(rbind, info_criteria_list)

# Create the plot
plot <- ggplot(info_criteria_df, aes(x = p)) +
  geom_line(aes(y = AIC, color = "AIC"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = BIC, color = "BIC"), linetype = "solid", size = 0.5) +
  labs(title = "Information Criteria for AR(p) Models with Monthly Data",
       y = "Information Criterion",
       x = "Order of AR Model (p)") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 1:p_max) +  # To ensure integer x-axis labels
  scale_color_manual(values = c("AIC" = "red", "BIC" = "black")) +
  guides(color = guide_legend(title = "Information Criterion")) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),  # Adjust X-axis label size and weight
    axis.text.y = element_text(size = 14, face = "bold"),  # Adjust Y-axis label size and weight
    axis.title = element_text(size = 14, face = "bold"), # Adjust axis title (x and y) size and weight
    legend.text = element_text(size = 12, face = "bold"),  # Adjust legend text size and weight
    legend.title = element_text(size = 14, face = "bold") 
  )

# Display the plot
plot






#yearly data IC plot
p1_max = 10
info_criteria_list <- lapply(1:p1_max, function(p) fit_arima_and_info_criteria(DataQuarterly$ExcessReturn, p))

# Combine the results into a data frame
info_criteria_df <- do.call(rbind, info_criteria_list)

# Create the plot
plot <- ggplot(info_criteria_df, aes(x = p)) +
  geom_line(aes(y = AIC, color = "AIC"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = BIC, color = "BIC"), linetype = "solid", size = 0.5) +
  labs(title = "Information Criteria for AR(p) Models with Quarterly Data",
       y = "Information Criterion",
       x = "Order of AR Model (p)") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 1:p1_max) +  # To ensure integer x-axis labels
  scale_color_manual(values = c("AIC" = "red", "BIC" = "black")) +
  guides(color = guide_legend(title = "Information Criterion")) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),  # Adjust X-axis label size and weight
    axis.text.y = element_text(size = 14, face = "bold"),  # Adjust Y-axis label size and weight
    axis.title = element_text(size = 14, face = "bold"), # Adjust axis title (x and y) size and weight
    legend.text = element_text(size = 12, face = "bold"),  # Adjust legend text size and weight
    legend.title = element_text(size = 14, face = "bold") 
  )

# Display the plot
plot





#IC for Yearly Data
p2_max = 10
info_criteria_list <- lapply(1:p2_max, function(p) fit_arima_and_info_criteria(DataAnnual$ExcessReturn, p))

# Combine the results into a data frame
info_criteria_df <- do.call(rbind, info_criteria_list)

# Create the plot
plot <- ggplot(info_criteria_df, aes(x = p)) +
  geom_line(aes(y = AIC, color = "AIC"), linetype = "solid", size = 0.5) +
  geom_line(aes(y = BIC, color = "BIC"), linetype = "solid", size = 0.5) +
  labs(title = "Information Criteria for AR(p) Models with Annual Data",
       y = "Information Criterion",
       x = "Order of AR Model (p)") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = 1:p2_max) +  # To ensure integer x-axis labels
  scale_color_manual(values = c("AIC" = "red", "BIC" = "black")) +
  guides(color = guide_legend(title = "Information Criterion")) +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),  # Adjust X-axis label size and weight
    axis.text.y = element_text(size = 14, face = "bold"),  # Adjust Y-axis label size and weight
    axis.title = element_text(size = 14, face = "bold"), # Adjust axis title (x and y) size and weight
    legend.text = element_text(size = 12, face = "bold"),  # Adjust legend text size and weight
    legend.title = element_text(size = 14, face = "bold") 
  )

# Display the plot
plot











#AR(p) modelling and plot for each frequency
DataMonthly1 <- DataMonthly[complete.cases(DataMonthly$ExcessReturn), ]
DataQuarterly1 <- DataQuarterly[complete.cases(DataQuarterly$ExcessReturn), ]
DataAnnual1 <- DataAnnual[complete.cases(DataAnnual$ExcessReturn), ]

fmse_ar_monthly <- 0
fmse_ar_quarterly <- 0
fmse_ar_annual <- 0


ar_modelling <- function(input, order) {
  
  
  AR_p <- ar(input$ExcessReturn, aic = FALSE,
             order.max = order,
             method = c("yule-walker"),
             demean = FALSE, series = "AR(p)")
  
  # Calculate fitted values
  fitted_values <- input$ExcessReturn - AR_p$res
  
  plot_data <- data.frame(
    Date = input$Time,
    Excess_Returns = input$ExcessReturn,
    Fitted = fitted_values
  )
  
  AR_FMSE <- mean(AR_p$resid^2, na.rm = TRUE)^0.5
  
  options(repr.plot.width = 14, repr.plot.height = 6)
  
  # Plot data using ggplot2
  plot <- ggplot(plot_data, aes(x = Date)) +
    geom_line(aes(y = Excess_Returns, color = "Observed")) +
    geom_line(aes(y = Fitted, color = "Forecasted")) +
    labs(title = "Excess Returns with AR(2) Fitted Values") +
    theme_minimal() +
    scale_color_manual(name = "Excess Return", values = c(Observed = "grey", Forecasted = "blue")) + theme(legend.position = "top")
  # Display the plot
  ggsave("AR_Annualy.jpeg", plot, width = 14, height = 6)
  
  residuals = AR_p$res
  if (identical(input, DataMonthly)) {
    fmse_ar_monthly <<- AR_FMSE
    
  } else if (identical(input, DataQuarterly)) {
    fmse_ar_quarterly <<- AR_FMSE
    
  } else if (identical(input, DataAnnual)) {
    fmse_ar_annual <<- AR_FMSE
  }
  
  
  print(plot)
  return( FMSE = paste0("FMSE: ", AR_FMSE))
}

ar_modelling(DataMonthly1,1)
ar_modelling(DataQuarterly1,4)
ar_modelling(DataAnnual1,2)








#Linear predictor single predictor in turn
# Function to calculate RMSE for each predictor alone
RMSE_LM <- function(df) {
  excluded_columns <- c("SP500_Index", "Returns", "ExcessReturn", "Time")
  
  # Get the names of the columns to use as covariates
  covariate_names <- setdiff(names(df), excluded_columns)
  # Initialize an empty dataframe to store RMSE values
  rmse_df <- data.frame(Predictor = character(0), RMSE = numeric(0))
  
  # Loop through each covariate and fit a linear model
  for (covariate in covariate_names) {
    formula <- as.formula(paste("ExcessReturn ~", covariate))
    model <- lm(formula, data = df)
    
    # Calculate RMSE
    rmse <- mean(model$residuals^2)^0.5
    
    # Add RMSE to the dataframe
    rmse_df <- rbind(rmse_df, data.frame(Predictor = covariate, RMSE = rmse))
    
  }
  rmse_df <- rmse_df[order(rmse_df$RMSE), ]
  return()
}
RMSE_LM(DataMonthly)
RMSE_LM(DataQuarterly)
RMSE_LM(DataAnnual)


RMSE_LM_MultipleDatasets <- function(DataMonthly, DataQuarterly, DataAnnual) {
  datasets <- list(DataMonthly, DataQuarterly, DataAnnual)
  dataset_names <- c("DataMonthly", "DataQuarterly", "DataAnnual")
  excluded_columns <- c("SP500_Index", "Returns", "ExcessReturn", "Time")
  
  # Initialize an empty dataframe to store RMSE values
  rmse_df <- data.frame(Dataset = character(0), Predictor = character(0), RMSE = numeric(0))
  
  for (i in 1:length(datasets)) {
    df <- datasets[[i]]
    dataset_name <- dataset_names[i]
    
    # Get the names of the columns to use as covariates
    covariate_names <- setdiff(names(df), excluded_columns)
    
    for (covariate in covariate_names) {
      formula <- as.formula(paste("ExcessReturn ~", covariate))
      model <- lm(formula, data = df)
      
      # Calculate RMSE
      rmse <- mean(model$residuals^2)^0.5
      
      # Add RMSE to the dataframe
      rmse_df <- bind_rows(rmse_df, data.frame(Dataset = dataset_name, Predictor = covariate, RMSE = rmse))
    }
  }
  
  rmse_df <- rmse_df[order(rmse_df$Dataset, rmse_df$RMSE), ]
  return(rmse_df)
}

result <- RMSE_LM_MultipleDatasets(DataMonthly, DataQuarterly, DataAnnual)

# Reshape the result dataframe
result_wide <- result %>% pivot_wider(names_from = Dataset, values_from = RMSE)
colnames(result_wide)[-1] <- paste0(colnames(result_wide)[-1], "_RMSE")

ar_values <- c(fmse_ar_monthly, fmse_ar_quarterly, fmse_ar_annual)
ar_new_row <- data.frame(Predictor = "AR(p)", 
                         DataMonthly_RMSE = ar_values[1], 
                         DataQuarterly_RMSE = ar_values[2], 
                         DataAnnual_RMSE = ar_values[3])

# Add the new row to the result_wide dataframe
result_wide <- rbind(result_wide, ar_new_row)

# Print the updated result_wide dataframe
result_wide





#'Linear Model using All Predictors'
# Function for fitting Linear Model with all predictors
linear_models_all <- function(input){
  formula_all_predictors <- paste("ExcessReturn ~", paste(names(input) %>% setdiff(c("Time", "ExcessReturn")), collapse=" + ")) %>% as.formula()
  model <- lm(data = input, formula_all_predictors)
  summary(model) %>% print()
  
  RMSE <- mean(model$residuals^2)^0.5
  R2 <- summary(model)$r.squared
  
  
  return(list(RMSE = RMSE,R2 = R2))  
}

# Calculate Linear Model using all predictors for each dataset
M1result <- linear_models_all(input = DataMonthly)
Q1result <- linear_models_all(input = DataQuarterly)
A1result <- linear_models_all(input = DataAnnual)

# Combine results if necessary
# final_result <- rbind(results_lm_monthly_all, results_lm_quarterly_all, results_lm_annual_all)


#Returning DF for all predictors used so far
lm_all_values <- c(M1result$RMSE, Q1result$RMSE, A1result$RMSE)

# Create a new data frame with the values
lm_all_new_row <- data.frame(Predictor = "LM All Predictors", "DataMonthly_RMSE" = lm_all_values[1], "DataQuarterly_RMSE" = lm_all_values[2], "DataAnnual_RMSE" = lm_all_values[3])

# Add the new row to the result_wide dataframe
result_wide <- rbind(result_wide, lm_all_new_row)

# Print the updated result_wide dataframe
result_wide



#Best model selection using significant predictors with BIC as infor criteria
automatic_model_selection <- function(data, target_column, max_lag = 1) {
  data1 <- data
  excluded_columns <- c("Time", target_column)
  
  # Drop missing values from rows with NaN in ExcessReturn column
  data <- data[complete.cases(data$ExcessReturn),]
  
  predictors <- dplyr::select(data, -excluded_columns)
  
  # Extracting the target column
  target <- data[[target_column]]
  
  # Lag the predictors
  lagged_predictors <- cbind(predictors, dplyr::lag(predictors, 1:max_lag))
  
  # Build the initial formula
  formula_1 <- as.formula(paste("ExcessReturn ~", paste(names(lagged_predictors), collapse = " + ")))
  
  # Use lmSelect for automatic model selection
  selected_model <- lm(formula = lmSubsets::lmSelect(formula_1, data = data, penalty = "BIC") %>% formula(best = 1), data = data)
  
  # Make predictions on lag values
  lagged_data <- cbind(data, dplyr::lag(data, 1:max_lag))
  lagged_predictions <- predict(selected_model, newdata = lagged_data)
  
  # Print selected predictors
  cat("Selected Predictors:", attr(terms(selected_model), "term.labels"), "\n")
  
  # Print BIC of the selected model
  cat("BIC:", BIC(selected_model), "\n")
  
  rmse = sqrt(mean(selected_model$residuals^2))
  cat("RMSE:", rmse, "\n")
  
  model_summary <- summary(selected_model)
  
  # Plot real vs. predicted values
  #plot(data1$Time, target, type = "l", col = "grey", xlab = "Time", ylab = "Excess Return", main = "Observed vs. Predicted")
  #lines(data1$Time, lagged_predictions, col = "blue", lty = 2)
  #legend("topright", legend = c("Observed", "Predicted (lag values)"), col = c("grey", "blue"), lty = 1:2)
  
  # Return the selected model, predictions, RMSE, and model summary
  return(list(model = selected_model, predictions = lagged_predictions, rmse = rmse, Msummary = model_summary))
}

M2result <- automatic_model_selection(DataMonthly, "ExcessReturn", max_lag = 1)


M2result$Msummary

Q2result <- automatic_model_selection(DataQuarterly, "ExcessReturn", max_lag = 1)
Q2result$Msummary


A2result <- automatic_model_selection(DataAnnual, "ExcessReturn", max_lag = 1)
A2result$Msummary

best_pred <- c(M2result$rmse, Q2result$rmse, A2result$rmse)

# Create a new data frame with the values
best_all_new_row <- data.frame(Predictor = "Best Mulitple Predictors(BIC)", "DataMonthly_RMSE" = best_pred[1], "DataQuarterly_RMSE" = best_pred[2], "DataAnnual_RMSE" = best_pred[3])

# Add the new row to the result_wide dataframe
result_wide <- rbind(result_wide, best_all_new_row)

# Print the updated result_wide dataframe
result_wide

```



#Forward Stepwise model 
lag = 0
forward_stepwise_forecast <- function(data, target_column) {
  data1 <- data
  excluded_columns <-  "Time"  
  # Extracting the target column
  target <- data[[target_column]]
  
  # Lagged predictors
  predictors <- dplyr::select(data1, -{{target_column}}, -{{excluded_columns}})
  # Function to compute BIC
  compute_bic <- function(model) {
    n <- length(target)
    k <- length(coef(model))
    resid <- residuals(model)
    bic <- n * log(sum(resid^2) / n) + k * log(n)
    return(bic)
  }
  
  # Initialize an empty model and BIC
  selected_predictors <- c()
  selected_bic <- Inf
  
  # Perform forward stepwise selection
  for (predictor in names(predictors)) {
    
    # Add the predictor to the selected set
    selected_predictors <- c(selected_predictors, predictor)
    
    # Fit a linear model with the selected predictors
    model <- lm(paste(target_column, "~", paste(selected_predictors, collapse = " + ")), data = data1)
    
    # Compute BIC
    bic <- compute_bic(model)
    
    # Update the selected predictors if the current model has lower BIC
    if (bic < selected_bic) {
      selected_bic <- bic
    } else {
      # If adding the predictor increases BIC, remove it
      selected_predictors <- setdiff(selected_predictors, predictor)
    }
  }
  
  # Final model with selected predictors
  final_model <- lm(paste(target_column, "~", paste(selected_predictors, collapse = " + ")), data = data)
  
  # Make final predictions
  final_predictions <- predict(final_model, newdata = data)
  
  # Print selected predictors
  cat("Selected Predictors:", selected_predictors, "\n")
  
  # Print BIC of the final model
  cat("BIC:", selected_bic, "\n")
  
  # Calculate RMSE
  rmse <- sqrt(mean(final_model$residuals^2))
  cat("RMSE:", rmse, "\n")
  
  # Plot real vs. predicted values
  #plot(data1$Time, target, type = "l", col = "grey", xlab = "Time", ylab = "Excess Return", main = "Observed vs. Predicted")
  #lines(data1$Time, final_predictions, col = "blue")
  #legend("topright", legend = c("Observed", "Forecasted"), col = c("grey", "blue"), lty = 1)
  
  # Return the final model, predictions, selected predictors, BIC, and RMSE
  return(list(model = final_model, predictions = final_predictions, selected_predictors = selected_predictors, bic = selected_bic, rmse = rmse))
}
StepModelM <- forward_stepwise_forecast(DataMonthly, "ExcessReturn")
StepModelQ <- forward_stepwise_forecast(DataQuarterly, "ExcessReturn")
StepModelY <- forward_stepwise_forecast(DataAnnual, "ExcessReturn")





#Final Dataframe with MSFE of all predictors
step_pred <- c(StepModelM$rmse, StepModelQ$rmse, StepModelY$rmse)

# Create a new data frame with the values
step_new_row <- data.frame(Predictor = "Forward Stepwise Model", "DataMonthly_RMSE" = step_pred[1], "DataQuarterly_RMSE" = step_pred[2], "DataAnnual_RMSE" = step_pred[3])

# Add the new row to the result_wide dataframe
result_wide <- rbind(result_wide, step_new_row)

# Print the updated result_wide dataframe
result_wide

