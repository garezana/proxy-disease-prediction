
#Libraries
library(ggplot2)
library(gridExtra)
library(sdmTMB)
library(dplyr)

#Data
dengue.2010.training.dat = read.csv("data/PR_2010_dengue_training.csv")
zika.2016.pred.dat = read.csv("data/PR_2016_3weekdata.csv") #Suspected cases
zika.2016.tested.dat = read.csv("data/tested_static_zika_counts_allweeks.csv") #Reported cases

#Functions
density_plot = function(df_vector, feature_name){
  mean = mean(df_vector[[feature_name]])
  sd = sd(df_vector[[feature_name]])
  ggplot(df_vector) + 
    geom_density(aes(x = .data[[feature_name]])) +
    geom_vline(aes(xintercept= mean ),
               color="red", linetype="dashed") +
    geom_vline(aes(xintercept = sd ),
               color="blue", linetype="dashed") + 
    geom_text(aes(x = Inf, y = Inf, label = paste("mean =", round(mean, 2))),
              hjust = 1, vjust = 1, color = "red", size = 4) +
    geom_text(aes(x = Inf, y = Inf - 0.05, label = paste("sd =", round(sd, 2))),
              hjust = 1, vjust = 2.5, color = "blue", size = 4) 
}

#Train Dengue model 
  #Making mesh
  mesh_dengue_resp = make_mesh(dengue.2010.training.dat, xy_cols = c("X", "Y"), cutoff = 0.05)
  
  #Fitting model
  dengue_2010_model = sdmTMB(
    dengue_count_2010 ~ 1 + scale(population_by_square_km_2010) + scale(tmean_2010),
    data = dengue.2010.training.dat,
    mesh = mesh_dengue_resp,
    family = nbinom2(link = "log"),
    spatial = "on"
  )
  
  #Checking model params
  summary(dengue_2010_model)
  estim1 = tidy(dengue_2010_model, conf.int = TRUE) #confidence intervals on the fixed
  estim2 = tidy(dengue_2010_model, effects = "ran_pars", conf.int = TRUE) #random effect and variance parameters:
  rbind(estim1, estim2)
  sanity(dengue_2010_model)
  head(dengue_2010_model$tmb_data$X_ij[[1]])
  
  #New dataframe to predict dengue
  new_df_dengue = new_data = data.frame(
    population_by_square_km_2010 = dengue.2010.training.dat$population_by_square_km_2010,
    tmean_2010 = dengue.2010.training.dat$tmean_2010,
    X = dengue.2010.training.dat$X,
    Y = dengue.2010.training.dat$Y
  )
  
  #Predicting
  predicted_dengue = predict(dengue_2010_model, newdata = new_df_dengue, type = "response")
  predicted_dengue["dengue_count_2010"] = dengue.2010.training.dat$dengue_count_2010
  
  #Plotting
  ggplot() +
    geom_point(data = predicted_dengue , aes(x = est, y = as.numeric(dengue_count_2010)))+ labs(y = "Reported Cases", x = "Predicted number of cases") +
    ggtitle("Dengue Priors") +
    theme_bw() + geom_abline(intercept = 0, slope = 1)

#Training Zika models
  #Filtering wanted data week. 
    #Run this if you want suspected number of cases
        #Early (2016): week == 26
        #Mid (2016): week == 39
        #Late (2017): week == 1
        test_early_data = zika.2016.pred.dat %>% filter(week == 1) %>% dplyr::select(-c(week,data_week_class, MUNICIPIO))
    #Run this if you want estimated number of cases
        #Very early (2016): week ==5
        #Early (2016): week == 26
        #Mid (2016): week == 39
        #Late (2017): week == 1
        test_early_data = zika.2016.tested.dat %>% filter(week == 5) %>% dplyr::select(-c(week, MUNICIPIO, count_type, year)) %>% rename(zika_count_2016 = count)

 #Visualizing data
  density_plot(test_early_data, "zika_count_2016")
  
  # Making Zika mesh
  test_early_mesh = make_mesh(test_early_data, xy_cols = c("X", "Y"), cutoff = 0.05)
  
  #Training models
  #Parameters
  b_0 = estim1$estimate[1]
  b_1 = estim1$estimate[2]
  b_2 = estim1$estimate[3]
  s_0 = estim1$std.error[1]
  s_1 = estim1$std.error[2]
  s_2 = estim1$std.error[3]
  
  range_0 = estim2$estimate[1]
  sigma_0 = estim2$estimate[2]
  
    #Dengue priors
    test_dengue_priors = sdmTMB(
      zika_count_2016 ~ 1 + scale(population_by_square_km_2016) + scale(tmean_2016),
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on",
      priors = sdmTMBpriors(
        b = normal(c(b_0, b_1, b_2), c(s_0, s_1, s_2)),
        matern_s = pc_matern(range_gt = range_0, sigma_lt = sigma_0))
    )
    
    
    #Vague priors
    test_vague_priors = sdmTMB(
      zika_count_2016 ~ 1 + scale(population_by_square_km_2016) + scale(tmean_2016),
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on"
    )
    #Intercept only
    test_intercept = sdmTMB(
      zika_count_2016 ~ 1 ,
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on"
    )
    
    summary(test_dengue_priors)
    summary(test_vague_priors)
    summary(test_intercept)
    

    
  #New dataframe to predict
    new_df_predictions = new_data = data.frame(
      population_by_square_km_2016 = test_early_data$population_by_square_km_2016,
      tmean_2016 = test_early_data$tmean_2016,
      X = test_early_data$X,
      Y = test_early_data$Y
    )
    
  #Predicting
  predicted_dengue_priors = predict(test_dengue_priors, newdata = new_df_predictions, type = "response")
  predicted_dengue_priors["zika_count_2016"] = test_early_data$zika_count_2016
  predicted_vague_priors = predict(test_vague_priors, newdata = new_df_predictions, type = "response")
  predicted_vague_priors["zika_count_2016"] = test_early_data$zika_count_2016
  predicted_intercept = predict(test_intercept, newdata = new_df_predictions, type = "response")
  predicted_intercept["zika_count_2016"] = test_early_data$zika_count_2016
 
  
  #Plotting
    #Dengue priors
    ggplot() +
      geom_point(data = predicted_dengue_priors , aes(x = est, y = as.numeric(zika_count_2016)))+ labs(y = "Reported Cases", x = "Predicted number of cases") +
      ggtitle("Dengue Priors") +
      theme_bw() + geom_abline(intercept = 0, slope = 1)
    #Vague priors
    ggplot() +
      geom_point(data = predicted_vague_priors, aes(x = est , y = as.numeric(zika_count_2016)))+ labs(y = "Reported Cases", x = "Predicted number of cases") +
      ggtitle("Vague Priors") +
      theme_bw() + geom_abline(intercept = 0, slope = 1)
    #Intercept
    ggplot() +
      geom_point(data = predicted_intercept, aes(x = est , y = as.numeric(zika_count_2016)))+ labs(y = "Reported Cases", x = "Predicted number of cases") +
      ggtitle("Intercept") +
      theme_bw() + geom_abline(intercept = 0, slope = 1)

    #Cross validation
    
    #CV Priors dengue
    cv_early_dengue_priors = sdmTMB_cv(
      zika_count_2016 ~ 1 + scale(population_by_square_km_2016) + scale(tmean_2016),
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on",
      priors = sdmTMBpriors(
        b = normal(c(b_0, b_1, b_2), c(s_0, s_1, s_2)),
        matern_s = pc_matern(range_gt = range_0, sigma_lt = sigma_0)),
      k_folds = 2
    )

    #CV vague priors
    cv_early_dengue_vague = sdmTMB_cv(
      zika_count_2016 ~ 1 + scale(population_by_square_km_2016) + scale(tmean_2016),
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on",
      k_folds = 2
    )
    #CV intercept
    cv_early_dengue_intercept = sdmTMB_cv(
      zika_count_2016 ~ 1,
      data = test_early_data,
      mesh = test_early_mesh,
      family = nbinom2(link = "log"),
      spatial = "on",
      k_folds = 2
    )
    
    #fold log-likelihood
    cv_early_dengue_priors$fold_loglik 
    cv_early_dengue_vague$fold_loglik 
    cv_early_dengue_intercept$fold_loglik 
    
    #total log-likelihood
    cv_early_dengue_priors$sum_loglik 
    cv_early_dengue_vague$sum_loglik 
    cv_early_dengue_intercept$sum_loglik 

    