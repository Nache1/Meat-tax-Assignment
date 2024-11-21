### Clear memory
rm(list = ls()) 

### Read and format data
### Update with correct wd and datafile
###setwd("C:\\")
data <- read.csv("data_cleaned_final_jan24.csv")

library(dplyr)
library(tidyr)

constant_vars <- data %>%
  group_by(group) %>%
  summarize(across(everything(), ~ n_distinct(.x) == 1)) %>%
  summarize(across(everything(), all)) %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "is_constant") %>%
  filter(is_constant) %>%
  pull(variable)

# Separate constant and varying variables
constant_data <- data %>%
  select(all_of(c("group", constant_vars))) %>%
  distinct()

varying_data <- data %>%
  select(-all_of(constant_vars))

# Pivot only the varying variables
pivoted_data <- varying_data %>%
  pivot_wider(
    id_cols = group,
    names_from = alt,
    values_from = -c(group, alt)
  )

# Recombine constant and pivoted data
database <- merge(pivoted_data, constant_data, by = "group")

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #


### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="MMNL_preference_space" ,
  modelDescr ="Table 3: Mixed logit model" ,
  indivID ="numeric_id",
  outputDirectory = "output"
)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c (   w_prod2    = 0 , 
                  w_prod3     = 0 , 
                  w_rev2     = 0 ,
                  w_rev3     = 0 ,
                  w_rev4    = 0 , 
                  w_rev5     = 0 , 
                  w_eff2     = 0 ,
                  w_eff3     = 0 ,
                  w_pcost     = 0 ,
                  sd_prod2    = 1 , 
                  sd_prod3     = 1 , 
                  sd_rev2     = 1 ,
                  sd_rev3     = 1 ,
                  sd_rev4    = 1 , 
                  sd_rev5     = 1 , 
                  sd_eff2     = 1 ,
                  sd_eff3     = 1 ,
                  sd_pcost     = 0 
                  
)

apollo_fixed=c("sd_pcost")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_prod2","draws_prod3","draws_rev2","draws_rev3","draws_rev4","draws_rev5","draws_eff2","draws_eff3","draws_pcost"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_prod2"]]   = w_prod2    +sd_prod2    * draws_prod2
  randcoeff[["b_prod3"]]   = w_prod3    +sd_prod3    * draws_prod3
  randcoeff[["b_rev2"]]    = w_rev2    +sd_rev2    * draws_rev2
  randcoeff[["b_rev3"]]    = w_rev3    +sd_rev3    * draws_rev3
  randcoeff[["b_rev4"]]    = w_rev4    +sd_rev4    * draws_rev4
  randcoeff[["b_rev5"]]    = w_rev5    +sd_rev5    * draws_rev5
  randcoeff[["b_eff2"]]    = w_eff2    +sd_eff2    * draws_eff2
  randcoeff[["b_eff3"]]    = w_eff3    +sd_eff3    * draws_eff3
  randcoeff[["b_pcost"]]   = exp( w_pcost    +sd_pcost     * draws_pcost) 
  
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()
# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs , functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  V[['alt1']] = b_prod2*prod2_1 + b_prod3*prod3_1 + b_rev2*rev2_1 + b_rev3*rev3_1 + b_rev4*rev4_1 + b_rev5*rev5_1 + b_eff2*eff2_1 + b_eff3*eff3_1 + b_pcost*pcost_1 
  V[['alt2']] = b_prod2*prod2_2 + b_prod3*prod3_2 + b_rev2*rev2_2 + b_rev3*rev3_2 + b_rev4*rev4_2 + b_rev5*rev5_2 + b_eff2*eff2_2 + b_eff3*eff3_2 + b_pcost*pcost_2 
  
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  =    c(alt1=1,alt2=0), 
    avail         = 1, 
    choiceVar     = chosen_1,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#Load model
#model = apollo_loadModel("wML_online_sqb")

model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
  #estimate_settings = list(
   # estimationRoutine="nr",                # Mimic Stata's Newton-Raphson method
    #maxIterations = 200,            # Default for Stata commands
    #convergenceCriterion = 1e-6     # Match Stata's stricter criterion
  #)
)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

apollo_modelOutput(model) 

