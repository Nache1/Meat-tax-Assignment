rm(list=ls())
library(readxl)
library(apollo)
library(dplyr)
library(tidyr)

apollo_initialise()
dat <- read.csv("data_cleaned_final_jan24.csv")

#Long to short format
database<-dat %>% 
  pivot_wider(
    id_cols=group,
    names_from =alt,
    values_from=-c(group,alt)
  )


# Dummy 
database <- database %>%
  mutate(
    # Age
    high_age = ifelse(age_1 >= 60, 1, 0),
    mid_age = ifelse(age_1 >= 40 & age_1 < 60, 1, 0),
    low_age = ifelse(age_1 < 40, 1, 0),
    
    # Gender
    male = ifelse(gender_1 == 1, 1, 0),
    
    # Income
    low_income = ifelse(income_1 == 1, 1, 0),
    mid_income = ifelse(income_1 == 2 | income_1 == 3, 1, 0),
    high_income = ifelse(income_1 == 4 | income_1 == 5, 1, 0),
    miss_income = ifelse(income_1 == 6, 1, 0),
    
    # Education
    low_education = ifelse(education_1 == 1 | education_1 == 2, 1, 0),
    mid_education = ifelse(education_1 == 3, 1, 0),
    high_education = ifelse(education_1 == 4 | education_1 == 5, 1, 0),
    
    # Meat
    high_meat = ifelse(meat_1 == 3 | meat_1 == 4, 1, 0),
    low_meat = ifelse(meat_1 == 1 | meat_1 == 2, 1, 0),
    
  )
# Global Model Configuration
pollo_control <- list(
  modelName = "LatentClassModel",
  modelDescr = "Latent class logit model",
  indivID = "numeric_id_1", # Individual ID variable
  outputDirectory= "output"
  outputDirectory= "output", 
  cores= 11
)


# Define apollo_beta with unique parameter names
apollo_beta <- c(
  asc_1 = 0,
  asc_2=0,
  prod2_beta_a = 0,
  prod2_beta_b = 0,
  prod2_beta_c = 0,
  prod3_beta_a = 0,
  prod3_beta_b = 0,
  prod3_beta_c = 0,
  rev2_beta_a = 0,
  rev2_beta_b = 0,
  rev2_beta_c = 0,
  rev3_beta_a = 0,
  rev3_beta_b = 0,
  rev3_beta_c = 0,
  rev4_beta_a = 0,
  rev4_beta_b = 0,
  rev4_beta_c = 0,
  rev5_beta_a = 0,
  rev5_beta_b = 0,
  rev5_beta_c = 0,
  eff2_beta_a = 0,
  eff2_beta_b = 0,
  eff2_beta_c = 0,
  eff3_beta_a = 0,
  eff3_beta_b = 0,
  eff3_beta_c = 0,
  cost_beta_a = 0,
  cost_beta_b = 0,
  cost_beta_c = 0,
  mid_age_beta_a=0,
  male_beta_a=0,
  high_age_beta_a=0,
  mid_education_beta_a=0,
  high_education_beta_a=0,
  mid_income_beta_a=0,
  high_income_beta_a=0,
  miss_income_beta_a=0,
  low_meat_beta_a=0,
  mid_age_beta_b=0,
  male_beta_b=0,
  high_age_beta_b=0,
  mid_education_beta_b=0,
  high_education_beta_b=0,
  mid_income_beta_b=0,
  high_income_beta_b=0,
  miss_income_beta_b=0,
  low_meat_beta_b=0,
  mid_age_beta_c=0,
  male_beta_c=0,
  high_age_beta_c=0,
  mid_education_beta_c=0,
  high_education_beta_c=0,
  mid_income_beta_c=0,
  high_income_beta_c=0,
  miss_income_beta_c=0,
  low_meat_beta_c=0,
  delta_a=0,
  delta_b=0,
  delta_c=0
)

#gamma_commute_a = 0,
#gamma_car_av_a  = 0,
#delta_b         = 0,
#gamma_commute_b = 0,
#gamma_car_av_b  = 0)

#apollo_fixed = c("asc_2","delta_b","gamma_commute_b","gamma_car_av_b")

# Fixed parameters 
#apollo_fixed <- c()

#Latent class components
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["prod2_beta"]] = list(prod2_beta_a, prod2_beta_b,prod2_beta_c)
  lcpars[["prod3_beta"]] = list(prod3_beta_a, prod3_beta_b,prod3_beta_c)
  lcpars[["rev2_beta"]] = list(rev2_beta_a, rev2_beta_b,rev2_beta_c)
  lcpars[["rev3_beta"]] = list(rev3_beta_a, rev3_beta_b,rev3_beta_c)
  lcpars[["rev4_beta"]] = list(rev4_beta_a, rev4_beta_b,rev4_beta_c)
  lcpars[["rev5_beta"]] = list(rev5_beta_a, rev5_beta_b,rev5_beta_c)
  lcpars[["eff2_beta"]] = list(eff2_beta_a, eff2_beta_b,eff2_beta_c)
  lcpars[["eff3_beta"]] = list(eff3_beta_a, eff3_beta_b,eff3_beta_c)
  lcpars[["cost_beta"]] = list(cost_beta_a, cost_beta_b,cost_beta_c)
  
  
  ### Utilities of class allocation model
  V=list()
  V[["class_a"]] = delta_a + mid_age_beta_a*mid_age + + high_age_beta_a*high_age + male_beta_a*male + mid_education_beta_a*mid_education+high_education_beta_a*high_education+mid_income_beta_a*mid_income+high_income_beta_a*high_income+ miss_income_beta_a*miss_income+low_meat_beta_a*low_meat
  V[["class_b"]] = delta_b + mid_age_beta_b*mid_age + high_age_beta_b*high_age + male_beta_b*male + mid_education_beta_b*mid_education+high_education_beta_b*high_education+mid_income_beta_b*mid_income+high_income_beta_b*high_income+ miss_income_beta_b*miss_income+low_meat_beta_b*low_meat
  V[["class_c"]] = delta_c + mid_age_beta_c*mid_age + high_age_beta_c*high_age + male_beta_c*male + mid_education_beta_c*mid_education+high_education_beta_c*high_education+mid_income_beta_c*mid_income+high_income_beta_c*high_income+ miss_income_beta_c*miss_income+low_meat_beta_c*low_meat
  
  
  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2, class_c=3), 
    utilities    = V  
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

# Validate inputs
apollo_inputs <- apollo_validateInputs()
#we need to defined the apollo_probabilities function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(chosen_1=1, chosen_2=2),
    avail        = list(chosen_1=1, chosen_2=1),
    choiceVar    = choice_1
  )
  
  ### Loop over classes
  for(s in 1:3){
    
    ### List of utilities (later integrated in mnl_settings below) the same that in apollo_beta
    V = list()
    V[['chosen_1']] = asc_1 + prod2_beta[[s]]*prod2_1 + prod3_beta[[s]] * prod3_1 + rev2_beta[[s]] * rev2_1 + rev3_beta[[s]] * rev3_1 + rev4_beta[[s]] * rev4_1 + rev5_beta[[s]] * rev5_1 + eff2_beta[[s]]*eff2_1 + eff3_beta[[s]]*eff3_1 + cost_beta[[s]]*cost_1
    V[['chosen_2']] = asc_2 + prod2_beta[[s]]*prod2_2 + prod3_beta[[s]] * prod3_2 + rev2_beta[[s]] * rev2_2 + rev3_beta[[s]] * rev3_2 + rev4_beta[[s]] * rev4_2 + rev5_beta[[s]] * rev5_2 + eff2_beta[[s]]*eff2_2 + eff3_beta[[s]]*eff3_2 + cost_beta[[s]]*cost_2
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    
  }
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, 
                        apollo_inputs,
                        #estimate_settings
                        )

### Show output in screen
apollo_modelOutput(model)
