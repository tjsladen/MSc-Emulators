library(tidyverse)
library(ggplot2)
library(reshape2)
library(DiceKriging)
load("C:\\Users\\tjsla\\OneDrive\\Desktop\\Masters\\Dissertation\\ensemble-jules-historical-timeseries.RData")

y1TR<- baresoilFrac_lnd_mean_ens_wave00[1:399, ]
y1TS<- baresoilFrac_lnd_mean_ens_wave00[400:499, ]

y2TR<- c3PftFrac_lnd_mean_ens_wave00[1:399, ]
y2TS<- c3PftFrac_lnd_mean_ens_wave00[400:499, ]

y3TR<- c4PftFrac_lnd_mean_ens_wave00[1:399, ]
y3TS<- c4PftFrac_lnd_mean_ens_wave00[400:499, ]

y4TR<- cSoil_ens_wave00[1:399, ]
y4TS<- cSoil_ens_wave00[400:499, ]

y5TR<- cVeg_ens_wave00[1:399, ]
y5TS<- cVeg_ens_wave00[400:499, ]

y6TR<- fHarvest_lnd_sum_ens_wave00[1:399, ]
y6TS<- fHarvest_lnd_sum_ens_wave00[400:499, ]

y7TR<- fLuc_lnd_sum_ens_wave00[1:399, ]
y7TS<- fLuc_lnd_sum_ens_wave00[400:499, ]

y8TR<- lai_lnd_mean_ens_wave00[1:399, ]
y8TS<- lai_lnd_mean_ens_wave00[400:499, ]

y9TR<- nbp_ens_wave00[1:399, ]
y9TS<- nbp_ens_wave00[400:499, ]

y10TR<- npp_ens_wave00[1:399, ]
y10TS<- npp_ens_wave00[400:499, ]

y11TR<- rh_lnd_sum_ens_wave00[1:399, ]
y11TS<- rh_lnd_sum_ens_wave00[400:499, ]

y12TR<- shrubFrac_lnd_mean_ens_wave00[1:399, ]
y12TS<- shrubFrac_lnd_mean_ens_wave00[400:499, ]

y13TR<- treeFrac_lnd_mean_ens_wave00[1:399, ]
y13TS<- treeFrac_lnd_mean_ens_wave00[400:499, ]

yTRL<- list(y1TR,y2TR,y3TR,y4TR,y5TR,y6TR,y7TR,y8TR,y9TR,y10TR,y11TR,y12TR,y13TR)
yTSL<- list(y1TS,y2TS,y3TS,y4TS,y5TS,y6TS,y7TS,y8TS,y9TS,y10TS,y11TS,y12TS,y13TS)

x_train<- X[1:399, ]
x_test<- X[400:499, ]

calculate_meansTR <- function(df_list) {
  meanL <- list()
  
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    
    Means <- colMeans(df, na.rm = TRUE)
    Means <- matrix(Means, nrow = 1, ncol = length(Means))
    Means_rep<- rep(Means, each = 399)
    Means_F<- matrix(Means_rep, nrow = 399, ncol = length(Means), byrow = F)
    colnames(Means_F)<- years
    meanL[[i]] <- Means_F
  }
  return(meanL)
}

calculate_meansTS <- function(df_list) {
  meanL <- list()
  
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    
    Means <- colMeans(df, na.rm = TRUE)
    Means <- matrix(Means, nrow = 1, ncol = length(Means))
    Means_rep<- rep(Means, each = 100)
    Means_F<- matrix(Means_rep, nrow = 100, ncol = length(Means), byrow = F)
    colnames(Means_F)<- years
    meanL[[i]] <- Means_F
  }
  return(meanL)
}

outputTR<- calculate_meansTR(yTRL)
outputTS<- calculate_meansTS(yTSL)

outputTR20<- lapply(outputTR, function(matrix) matrix[, 1:20])
outputTS20<- lapply(outputTS, function(matrix) matrix[, 1:20])

y_train1<- outputTR20[[1]]
y_train2<- outputTR20[[2]]
y_train3<- outputTR20[[3]]
y_train4<- outputTR20[[4]]
y_train5<- outputTR20[[5]]
y_train6<- outputTR20[[6]]
y_train7<- outputTR20[[7]]
y_train8<- outputTR20[[8]]
y_train9<- outputTR20[[9]]
y_train10<- outputTR20[[10]]
y_train11<- outputTR20[[11]]
y_train12<- outputTR20[[12]]
y_train13<- outputTR20[[13]]
y_test1<- outputTS20[[1]]
y_test2<- outputTS20[[2]]
y_test3<- outputTS20[[3]]
y_test4<- outputTS20[[4]]
y_test5<- outputTS20[[5]]
y_test6<- outputTS20[[6]]
y_test7<- outputTS20[[7]]
y_test8<- outputTS20[[8]]
y_test9<- outputTS20[[9]]
y_test10<- outputTS20[[10]]
y_test11<- outputTS20[[11]]
y_test12<- outputTS20[[12]]
y_test13<- outputTS20[[13]]

train_emulators <- function(y_train, x_train) {
  gp_models <- list()
  
  for (i in 1:ncol(y_train)) {
    gp_model <- km(design = x_train, response = y_train[, i], nugget.estim = T,
                   control = list(trace = FALSE))
    gp_models[[i]] <- gp_model
  }
  return(gp_models)
}

make_predictions <- function(gp_models, x_test) {
  predictions <- lapply(gp_models, function(model) predict.km(model, newdata = x_test, type = "UK"))
  return(predictions)
}

calculate_rmse <- function(predictions, actuals) {
  sqrt(mean((predictions$mean - actuals)^2, na.rm = TRUE))
}

extract_hyperparameters <- function(model) {
  list(
    length_scales = model@covariance@range.val,
    nugget = model@covariance@nugget,
    variance = model@covariance@sd2
  )
}

extract_hyperparameters_from_list <- function(model_list) {
  hyperparameters_list <- lapply(model_list, extract_hyperparameters)
  hyperparameters_df <- do.call(rbind, lapply(hyperparameters_list, as.data.frame))
  
  hyperparameters_df$model_index <- rep(1:length(model_list), each = nrow(hyperparameters_df) / length(model_list))
  
  hyperparameters_df <- hyperparameters_df[, c("model_index", setdiff(names(hyperparameters_df), "model_index"))]
  
  return(hyperparameters_df)
}

extract_predictions_to_df <- function(predictions, x_test) {
  n_models <- length(predictions)
  n_samples <- nrow(x_test)
  
  results <- data.frame(
    model_index = rep(1:n_models, each = n_samples),
    test_sample_index = rep(1:n_samples, times = n_models),
    predicted_mean = unlist(lapply(predictions, function(p) p$mean)),
    predicted_variance = unlist(lapply(predictions, function(p) p$sd^2))
  )
  
  return(results)
}

gp_models_y1 <- train_emulators(y_train1, x_train)
predictions_y1 <- make_predictions(gp_models_y1, x_test)
rmse_values1 <- numeric(length(predictions_y1))
for(i in seq_along(predictions_y1)){
  rmse_values1[i]<- calculate_rmse(predictions_y1[[i]], y_test1)
}
print(rmse_values1)
mean_observed1 <- apply(y_test1, 2, mean)
percentage_error1 <- (rmse_values1 / mean_observed1) * 100
print(percentage_error1)
print(min(percentage_error1))
print(max(percentage_error1))
hp_y1<- extract_hyperparameters_from_list(gp_models_y1)
predictions_df_y1 <- extract_predictions_to_df(predictions_y1, x_test)
average_predictions1 <- aggregate(cbind(predicted_mean, predicted_variance) ~ model_index, data = predictions_df_y1, FUN = mean)
print(average_predictions1)

gp_models_y2<- train_emulators(y_train2, x_train)
predictions_y2<- make_predictions(gp_models_y2, x_test)
rmse_values2<- numeric(length(predictions_y2))
for(i in seq_along(predictions_y2)){
  rmse_values2[i]<- calculate_rmse(predictions_y2[[i]], y_test2)
}
mean_observed2<- apply(y_test2, 2, mean)
percentage_error2<- (rmse_values2/mean_observed2)*100
print(rmse_values2)
print(percentage_error2)
print(min(percentage_error2))
print(max(percentage_error2))

gp_models_y3<- train_emulators(y_train3, x_train)
predictions_y3<- make_predictions(gp_models_y3, x_test)
rmse_values3<- numeric(length(predictions_y3))
for(i in seq_along(predictions_y3)){
  rmse_values3[i]<- calculate_rmse(predictions_y3[[i]], y_test3)
}
mean_observed3<- apply(y_test3, 2, mean)
percentage_error3<- (rmse_values3/mean_observed3)*100
print(rmse_values3)
print(percentage_error3)
print(min(percentage_error3))
print(max(percentage_error3))

gp_models_y4<- train_emulators(y_train4, x_train)
predictions_y4<- make_predictions(gp_models_y4, x_test)
rmse_values4<- numeric(length(predictions_y4))
for(i in seq_along(predictions_y4)){
  rmse_values4[i]<- calculate_rmse(predictions_y4[[i]], y_test4)
}
mean_observed4<- apply(y_test4, 2, mean)
percentage_error4<- (rmse_values4/mean_observed4)*100
print(rmse_values4)
print(percentage_error4)
print(min(percentage_error4))
print(max(percentage_error4))

gp_models_y5<- train_emulators(y_train5, x_train)
predictions_y5<- make_predictions(gp_models_y5, x_test)
rmse_values5<- numeric(length(predictions_y5))
for(i in seq_along(predictions_y5)){
  rmse_values5[i]<- calculate_rmse(predictions_y5[[i]], y_test5)
}
mean_observed5<- apply(y_test5, 2, mean)
percentage_error5<- (rmse_values5/mean_observed5)*100
print(rmse_values5)
print(percentage_error5)
print(min(percentage_error5))
print(max(percentage_error5))

gp_models_y6<- train_emulators(y_train6, x_train)
predictions_y6<- make_predictions(gp_models_y6, x_test)
rmse_values6<- numeric(length(predictions_y6))
for(i in seq_along(predictions_y6)){
  rmse_values6[i]<- calculate_rmse(predictions_y6[[i]], y_test6)
}
mean_observed6<- apply(y_test6, 2, mean)
percentage_error6<- (rmse_values6/mean_observed6)*100
print(rmse_values6)
print(percentage_error6)
print(min(percentage_error6))
print(max(percentage_error6))

gp_models_y7<- train_emulators(y_train7, x_train)
predictions_y7<- make_predictions(gp_models_y7, x_test)
rmse_values7<- numeric(length(predictions_y7))
for(i in seq_along(predictions_y7)){
  rmse_values7[i]<- calculate_rmse(predictions_y7[[i]], y_test7)
}
mean_observed7<- apply(y_test7, 2, mean)
percentage_error7<- (rmse_values7/mean_observed7)*100
print(rmse_values7)
print(percentage_error7)
print(min(percentage_error7))
print(max(percentage_error7))

gp_models_y8<- train_emulators(y_train8, x_train)
predictions_y8<- make_predictions(gp_models_y8, x_test)
rmse_values8<- numeric(length(predictions_y8))
for(i in seq_along(predictions_y8)){
  rmse_values8[i]<- calculate_rmse(predictions_y8[[i]], y_test8)
}
mean_observed8<- apply(y_test8, 2, mean)
percentage_error8<- (rmse_values8/mean_observed8)*100
print(rmse_values8)
print(percentage_error8)
print(min(percentage_error8))
print(max(percentage_error8))

#error model9
#Error in chol.default(R) : the leading minor of order 1 is not positive
#In addition: Warning message:
#In runif(n = ninit, min = 1/2 * angle.init, max = min(3/2 * angle.init,  :
                                                          #NAs produced
gp_models_y10<- train_emulators(y_train10, x_train)
predictions_y10<- make_predictions(gp_models_y10, x_test)
rmse_values10<- numeric(length(predictions_y10))
for(i in seq_along(predictions_y10)){
  rmse_values10[i]<- calculate_rmse(predictions_y10[[i]], y_test10)
}
mean_observed10<- apply(y_test10, 2, mean)
percentage_error10<- (rmse_values10/mean_observed10)*100
print(rmse_values10)
print(percentage_error10)
print(min(percentage_error10))
print(max(percentage_error10))

gp_models_y11<- train_emulators(y_train11, x_train)
predictions_y11<- make_predictions(gp_models_y11, x_test)
rmse_values11<- numeric(length(predictions_y11))
for(i in seq_along(predictions_y11)){
  rmse_values11[i]<- calculate_rmse(predictions_y11[[i]], y_test11)
}
mean_observed11<- apply(y_test11, 2, mean)
percentage_error11<- (rmse_values11/mean_observed11)*100
print(rmse_values11)
print(percentage_error11)
print(min(percentage_error11))
print(max(percentage_error11))

gp_models_y12<- train_emulators(y_train12, x_train)
predictions_y12<- make_predictions(gp_models_y12, x_test)
rmse_values12<- numeric(length(predictions_y12))
for(i in seq_along(predictions_y12)){
  rmse_values12[i]<- calculate_rmse(predictions_y12[[i]], y_test12)
}
mean_observed12<- apply(y_test12, 2, mean)
percentage_error12<- (rmse_values12/mean_observed12)*100
print(rmse_values12)
print(percentage_error12)
print(min(percentage_error12))
print(max(percentage_error12))

gp_models_y13<- train_emulators(y_train13, x_train)
predictions_y13<- make_predictions(gp_models_y13, x_test)
rmse_values13<- numeric(length(predictions_y13))
for(i in seq_along(predictions_y13)){
  rmse_values13[i]<- calculate_rmse(predictions_y13[[i]], y_test13)
}
mean_observed13<- apply(y_test13, 2, mean)
percentage_error13<- (rmse_values13/mean_observed13)*100
print(rmse_values13)
print(percentage_error13)
print(min(percentage_error13))
print(max(percentage_error13))

sensitivity_analysis <- function(gp_model, x_train, y_train, x_test, y_test, hyperparameter, initial_values, factor = 2, n_steps = 10) {
  results <- data.frame()
  
  if (hyperparameter == "length_scale") {
    for (i in seq_along(initial_values)) {
      values <- seq(initial_values[i] / factor, initial_values[i] * factor, length.out = n_steps)
      for (value in values) {
        modified_model <- gp_model
        modified_model@covariance@range.val[i] <- value
        
        # Refit the model with modified length scale
        refitted_model <- km(
          design = x_train,
          response = y_train,
          nugget = modified_model@covariance@nugget,
          covtype = "gauss",
          coef.cov = modified_model@covariance@range.val,
          coef.var = modified_model@covariance@sd2,
          control = list(trace = FALSE)
        )
        
        # Make predictions on the test set
        predictions <- predict.km(refitted_model, newdata = x_test, type = "UK")
        
        # Calculate RMSE
        rmse_value <- sqrt(mean((predictions$mean - y_test)^2, na.rm = TRUE))
        
        # Store results
        results <- rbind(results, data.frame(hyperparameter = paste0("length_scale_", i), hyperparameter_value = value, rmse = rmse_value))
      }
    }
  } else {
    values <- seq(initial_values / factor, initial_values * factor, length.out = n_steps)
    for (value in values) {
      modified_model <- gp_model
      
      if (hyperparameter == "nugget") {
        modified_model@covariance@nugget <- value
      } else if (hyperparameter == "variance") {
        modified_model@covariance@sd2 <- value
      }
      
      # Refit the model with modified hyperparameter
      refitted_model <- km(
        design = x_train,
        response = y_train,
        nugget = modified_model@covariance@nugget,
        covtype = "gauss",
        coef.cov = modified_model@covariance@range.val,
        coef.var = modified_model@covariance@sd2,
        control = list(trace = FALSE)
      )
      
      # Make predictions on the test set
      predictions <- predict.km(refitted_model, newdata = x_test, type = "UK")
      
      # Calculate RMSE
      rmse_value <- sqrt(mean((predictions$mean - y_test)^2, na.rm = TRUE))
      
      # Store results
      results <- rbind(results, data.frame(hyperparameter = hyperparameter, hyperparameter_value = value, rmse = rmse_value))
    }
  }
  
  return(results)
}
plot_sensitivity <- function(results, hyperparameter_name) {
  ggplot(results, aes(x = hyperparameter_value, y = rmse)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Sensitivity Analysis for", hyperparameter_name),
         x = hyperparameter_name,
         y = "RMSE") +
    theme_minimal()
}
gp_modely1_1 <- gp_models_y1[[1]]
y_train1_1 <- y_train1[, 1]
y_test1_1 <- y_test1[, 1]

initial_length_scale1_1 <- hp_y1$length_scales[hp_y1$model_index == 1]
initial_nugget1_1 <- hp_y1$nugget[hp_y1$model_index == 1][1]
initial_variance1_1 <- hp_y1$variance[hp_y1$model_index == 1][1]

length_scale_resultsy1_1 <- sensitivity_analysis(gp_modely1_1, x_train, y_train1_1, x_test, y_test1_1, "length_scale", initial_length_scale1_1)
nugget_resultsy1_1 <- sensitivity_analysis(gp_modely1_1, x_train, y_train1_1, x_test, y_test1_1, "nugget", initial_nugget1_1)
variance_resultsy1_1 <- sensitivity_analysis(gp_modely1_1, x_train, y_train1_1, x_test, y_test1_1, "variance", initial_variance1_1)

plot_sensitivity(length_scale_resultsy1_1, "Length Scale")
plot_sensitivity(nugget_resultsy1_1, "Nugget")
plot_sensitivity(variance_resultsy1_1, "Variance")
