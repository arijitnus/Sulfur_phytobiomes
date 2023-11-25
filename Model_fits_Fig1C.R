library(readxl)
d1 <- read_excel("B1.xlsx")
d1
str(d1)
d1$sphere <- as.factor(d1$sphere)
names(d1)[names(d1) == "H2S production"] <- "H2S_production"
names(d1)[names(d1) == "DMS production"] <- "DMS_production"

library(MASS)
library(pscl)
library(broom)
library(dplyr)

fit_models_and_compare_all <- function(data, response_columns) {
  best_models <- list()
  best_model_summaries <- list()
  
  # Relevelin 'sphore' to 'Soil' is the reference 
  data$sphere <- relevel(data$sphere, ref = "Soil")
  
  for (response_column in response_columns) {
    # Creating formula
    formula <- as.formula(paste(response_column, "~ sphere"))
    
    #model fitting
    poisson_model <- glm(formula, data = data, family = "poisson")
    nb_model <- glm.nb(formula, data = data)
    zip_model <- zeroinfl(formula, data = data, dist = "poisson")
    zinb_model <- zeroinfl(formula, data = data, dist = "negbin")
    
    #now I create the list of models
    model_list <- list(
      poisson_model = poisson_model, 
      nb_model = nb_model, 
      zip_model = zip_model, 
      zinb_model = zinb_model
    )
    
    # Compare model AIC
    aic_values <- sapply(model_list, AIC)
    
    # Identufy  model that will have the lowest AIC
    best_model_index <- which.min(aic_values)
    best_model_name <- names(aic_values)[best_model_index]
    best_model <- model_list[[best_model_index]]
    
    # Save the best model and  summary
    best_models[[response_column]] <- best_model
    best_model_summaries[[paste(response_column, "best_model", sep = "_")]] <- summary(best_model)
  }
  
  return(list(BestModels = best_models, ModelSummaries = best_model_summaries))
}

column_names <- c("H2S_production", "DMS_production", "Sulf_Thio_Total", "Taurine_tot", "Sulfonate_tot", "Sulfutase", "SoxB")
results <- fit_models_and_compare_all(d1, column_names)

# Extracting the best models and their summaries
best_models <- results$BestModels
best_model_summaries <- results$ModelSummaries
