library(sdmTMB)
library(dplyr)

#Data
  #Dengue
    dengue_count <- c(74, 137, 279, 16, 3, 47, 429, 153, 35, 18, 761, 74, 248, 168, 78, 266, 89, 24, 29, 85, 16, 86, 13, 132, 72, 64, 52, 21, 309, 91, 235, 57, 214, 18, 218, 184, 96, 133, 197, 50, 209, 38, 141, 20, 40, 102, 61, 84, 226, 235, 112, 83, 86, 52, 311, 143, 437, 210, 75, 113, 30, 245, 58, 901, 119, 249, 100, 203, 197, 141, 73, 83, 172, 91, 132, 58)
    X <- c(1184.440, 739.952, 797.594, 1847.690, 1702.410, 797.183, 1268.620, 1922.780, 1389.600, 1654.160, 1803.710, 760.130, 1928.250, 1072.390, 2100.440,  2026.470, 1829.170, 1824.050, 2333.720, 1435.730, 1811.790, 1601.060,  1747.250, 1635.100, 1686.630, 2332.040, 1390.360, 1008.520, 1840.730, 1144.830, 1860.630, 2003.700, 1140.010, 803.228, 2182.590, 919.233, 1359.480, 1458.100, 2078.250, 880.738, 1065.280, 941.821, 2120.220,  2087.920, 2272.240, 1463.340, 985.087, 2065.210, 808.845, 839.449, 1537.420, 2241.980, 1714.690, 1522.880, 1969.550, 1219.690, 1331.790, 1003.090, 679.884, 2178.430, 983.444, 1711.260, 883.159, 1916.940, 2007.000, 955.083,  1570.360, 1721.170, 1754.970, 1982.490, 1239.040, 1625.180, 1561.580,  1481.840, 2091.740, 1073.270)
    Y <- c(385.929, 587.037, 696.794, 465.371, 331.879, 506.921, 637.887, 185.173, 669.460, 409.974, 573.836, 234.779, 421.656, 651.350, 550.186, 603.177, 671.157, 300.831, 468.047, 506.545, 379.109, 294.412, 435.464, 523.467, 670.277, 540.072, 587.515, 168.216, 195.174, 233.862, 567.847, 482.137, 642.102, 336.505, 348.495, 686.024, 419.821, 243.198, 434.822, 201.047, 484.514, 449.896, 395.022, 658.543, 566.923, 652.488, 377.869, 207.343, 418.044, 606.184, 537.597, 443.126, 505.959, 424.505, 222.276, 252.758,253.951, 673.970, 559.733, 571.091, 279.246, 200.866, 310.787, 616.167, 351.071, 551.287, 181.470, 587.978, 664.177, 559.522, 486.870, 641.614, 663.100, 328.546, 265.572, 275.867)
    tmean <-  c(22.18056, 25.95833, 25.68750, 24.20833, 22.82500, 25.75000, 25.40000, 26.11111, 26.16667, 22.40833, 25.84524, 26.52083, 25.09896, 25.02976,  25.03333, 26.56944, 26.87500, 22.97024, 26.19167, 23.82407, 23.63194,  24.57812, 23.80556, 24.24167, 26.40278, 26.51389, 24.91667, 26.55833,  25.67130, 25.40278, 25.97917, 25.56944, 25.21354, 26.20833, 26.28472,  25.05357, 22.02500, 26.19444, 25.64583, 26.61574, 23.56250, 24.50833,  25.43750, 26.77083, 25.72222, 25.90833, 23.12500, 25.54167, 25.77500,  25.40278, 24.54167, 24.77381, 24.31250, 21.89881, 25.13889, 24.67361,  25.10417, 25.31250, 25.83333, 25.26389, 24.88333, 26.18229, 25.64583,  26.59722, 24.69048, 25.00000, 26.67500, 26.02778, 26.64583, 26.00000,  23.72135, 25.76042, 26.11979, 23.90972, 25.69792, 24.13750)
    population_km2 <- c(112.79643, 525.13407, 644.19470, 367.86074, 319.38678, 287.54603, 295.75474, 503.52532, 512.65207, 341.77493, 1813.03402, 279.36725,941.48537, 292.87818, 559.68698, 1505.91078, 2244.80679, 357.76554,181.23052, 108.99951, 466.06454, 200.27783, 282.47866, 336.86964,638.17762, 478.33260, 321.87724, 202.44999, 269.49201, 197.12402, 1370.86636, 628.07416, 387.69913, 587.32202, 504.44123, 318.59216, 144.29559, 325.04042, 587.23989, 165.85894, 193.22612, 82.29202,440.74476, 599.18315, 300.20390, 369.21843, 66.17056, 224.01885, 442.93397, 307.63012, 323.91905, 199.70194, 428.40233, 142.15073,159.37596, 210.11406, 559.88595, 441.24056, 410.68760, 345.87257,272.25304, 172.97431, 251.68786, 3189.87505, 298.48446, 232.63617,264.14169, 1058.36089, 1488.72932, 1391.93281, 112.76513, 556.26024,502.30094, 282.45778, 265.33300, 238.05277)
    df.dengue0 <- data.frame(count = dengue_count, X = X, Y = Y, tmean = tmean, population_km2 = population_km2)
    
  #Zika
    zika.2016.dat <- read.csv("data/PR_2016_3weekdata.csv")
    zika.2016.dat <- rename(zika.2016.dat, count = zika_count_2016, tmean = tmean_2016, population_km2 = population_by_square_km_2016)
    df.z1 <- subset(zika.2016.dat, week == 26)[, !names(zika.2016.dat) %in% c("data_week_class","week", "MUNICIPIO")]
    df.z2 <- subset(zika.2016.dat, week == 39)[, !names(zika.2016.dat) %in% c("data_week_class","week", "MUNICIPIO")]
    df.z3 <- subset(zika.2016.dat, week == 1)[, !names(zika.2016.dat) %in% c("data_week_class","week", "MUNICIPIO")]
#Mesh
    #Dengue
      mesh_dengue_resp <- make_mesh(df.dengue0, xy_cols = c("X", "Y"), cutoff = 0.05)
    #Zika
      mesh_z1_resp <- make_mesh(df.z1, xy_cols = c("X", "Y"), cutoff = 0.05)
      mesh_z2_resp <- make_mesh(df.z2, xy_cols = c("X", "Y"), cutoff = 0.05)
      mesh_z3_resp <- make_mesh(df.z3, xy_cols = c("X", "Y"), cutoff = 0.05)
    #Meshes created for predictions
      mesh_dengue_pred <- make_mesh(df.dengue0, xy_cols = c("X", "Y"), cutoff = 0.01)
      mesh_z1_pred <- make_mesh(df.z1, xy_cols = c("X", "Y"), cutoff = 0.01)
      mesh_z2_pred <- make_mesh(df.z2, xy_cols = c("X", "Y"), cutoff = 0.01)
      mesh_z3_pred <- make_mesh(df.z3, xy_cols = c("X", "Y"), cutoff = 0.01)
  
#Modeling
    #Function
      fit_sdmTMB_model <- function(data, family, mesh, response, predictors, spatial = "on") {
        formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
        model <- sdmTMB(
          formula,
          data = data,
          mesh = mesh,
          family = family,
          spatial = spatial
        )
        return(model)
      }

  #Families to try
    families <- list(
      poisson(link = "log"),
      nbinom1(link = "log"),
      nbinom2(link = "log"),
      tweedie(link = "log"),
      Gamma(link = "log")
    )

  #Other input for models
    datasets <- datasets <- list(
      df_dengue = df.dengue0,
      df_zika1 = df.z1,
      df_zika2 = df.z2,
      df_zika3 = df.z3
    )
    
    response <- "count"
    predictors <- c("scale(population_km2)", "scale(tmean)")
    meshes <- list(mesh_dengue_resp, mesh_z1_resp, mesh_z2_resp, mesh_z3_resp)
    prediction_meshes <- list(mesh_dengue_pred, mesh_z1_pred, mesh_z2_pred, mesh_z3_pred)

# Running model for different families and datasets with tryCatch
    model_list <- list()
    error_log <- list()  
    plot_list <- list() 
      
    for (i in seq_along(datasets)) {
      data_name <- names(datasets)[i]  
      data <- datasets[[i]] 
      mesh <- meshes[[i]]
      
      for (j in seq_along(families)) {
        family <- families[[j]]
        
        model_name <- paste0("model_", data_name, "_", family$family)
        
        result <- tryCatch({
          model <- fit_sdmTMB_model(data, family, mesh, response, predictors)
          model_list[[model_name]] <- model
          
          # Predict using new data
          new_df_b <- data.frame(
            population_km2 = data$population_km2,
            tmean = seq(min(data$tmean), max(data$tmean), length.out = length(data$tmean)),
            X = data$X,
            Y = data$Y
          )
          
          predicted <- predict(model, newdata = new_df_b, mesh = prediction_mesh)
          predicted$est <- exp(predicted$est) 
          
          p <- ggplot() +
            geom_point(data = predicted, aes(x = est, y = data$count)) +
            labs(y = "Reported Cases", x = "Predicted number of cases") +
            ggtitle(paste("Prediction Plot for Model", model_name)) +
            theme_bw() +
            geom_abline(intercept = 0, slope = 1)
          
          plot_list[[model_name]] <- p
          
          NULL  # Return NULL for success
        }, error = function(e) {
          error_log[[model_name]] <- e  # Log error message
          return(e) 
        })
        
        if (!is.null(result)) {
          message("Error in ", model_name, ": ", result$message)
        }
      }
    }
    
    
    for (plot_name in names(plot_list)) {
      print(plot_list[[plot_name]])
    }
    
    