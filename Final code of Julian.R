
#Correlation in design 

library(readxl)
library(readxl)
library(ggplot2)
library(ggstatsplot)
library(dplyr)
library(reshape2)
library(pheatmap)
data<-read_xlsx("Final_experimentaldesign.xlsx")
frecuency1<-table(data$A1_1)
print(frecuency1)
frecuency2<-table(data$A1_2)
print(frecuency2)
frecuency3<-table(data$A1_3)
print(frecuency3)
frecuency4<-table(data$A1_4)
print(frecuency4)
frecuency5<-table(data$A2_1)
print(frecuency5)
frecuency6<-table(data$A2_2)
print(frecuency6)
frecuency7<-table(data$A2_3)
print(frecuency7)
frecuency8<-table(data$A2_4)
print(frecuency8)
data$A1_1_numeric<-as.numeric(factor(data$A1_1, levels = c("€  7 / maand","€  15 / maand","€  30 / maand","€  60 / maand")))
data$A2_1_numeric<-as.numeric(factor(data$A2_1, levels = c("€  7 / maand","€  15 / maand","€  30 / maand","€  60 / maand")))
data$A1_2_numeric<-as.numeric(factor(data$A1_2, levels = c("Alle soorten vlees worden belast","Alle soorten vlees worden belast, behalve gevogelte","Alle soorten vlees worden belast, behalve biologisch geproduceerd vlees")))
data$A2_2_numeric<-as.numeric(factor(data$A2_2, levels = c("Alle soorten vlees worden belast","Alle soorten vlees worden belast, behalve gevogelte","Alle soorten vlees worden belast, behalve biologisch geproduceerd vlees")))
data$A1_3_numeric<-as.numeric(factor(data$A1_3, levels = c("Algemene uitgaven van de overheid (staatskas)","Verduurzaming van de veehouderij","Dierenwelzijnsverbeteringen in veehouderij","Verlaging BTW op groente en fruit","Compensatie huishoudens met laag inkomen")))
data$A2_3_numeric<-as.numeric(factor(data$A2_3, levels = c("Algemene uitgaven van de overheid (staatskas)","Verduurzaming van de veehouderij","Dierenwelzijnsverbeteringen in veehouderij","Verlaging BTW op groente en fruit","Compensatie huishoudens met laag inkomen")))
data$A1_4_numeric<-as.numeric(factor(data$A1_4, levels = c("Doorgaan met de vleesbelasting, onafhankelijk van wat andere landen doen","Alleen doorgaan als alle andere EU-landen ook een vleesbelasting invoeren","Alleen doorgaan als sommige andere EU-landen ook een vleesbelasting invoeren")))
data$A2_4_numeric<-as.numeric(factor(data$A2_4, levels = c("Doorgaan met de vleesbelasting, onafhankelijk van wat andere landen doen","Alleen doorgaan als alle andere EU-landen ook een vleesbelasting invoeren","Alleen doorgaan als sommige andere EU-landen ook een vleesbelasting invoeren")))

install.packages("fastDummies")
library(fastDummies)
# Dummy
data_dummies <- dummy_cols(data, 
                           select_columns = c("A1_1_numeric", "A2_1_numeric", 
                                              "A1_2_numeric", "A2_2_numeric", 
                                              "A1_3_numeric", "A2_3_numeric", 
                                              "A1_4_numeric", "A2_4_numeric"))

# Visualitation 
head(data_dummies)

# Selection of variables
variables <- data_dummies[, c("A1_1_numeric_1", "A1_1_numeric_2", "A1_1_numeric_3", "A1_1_numeric_4",
                              "A1_2_numeric_1", "A1_2_numeric_2", "A1_2_numeric_3",
                              "A1_3_numeric_1", "A1_3_numeric_2", "A1_3_numeric_3", "A1_3_numeric_4", "A1_3_numeric_5",
                              "A1_4_numeric_1", "A1_4_numeric_2", "A1_4_numeric_3",
                              "A2_1_numeric_1", "A2_1_numeric_2", "A2_1_numeric_3", "A2_1_numeric_4",
                              "A2_2_numeric_1", "A2_2_numeric_2", "A2_2_numeric_3",
                              "A2_3_numeric_1", "A2_3_numeric_2", "A2_3_numeric_3", "A2_3_numeric_4", "A2_3_numeric_5",
                              "A2_4_numeric_1", "A2_4_numeric_2", "A2_4_numeric_3")]

# Matrix correlation using Spearman
correlaciones <- cor(variables, method = "spearman")

# See the correlation
print(correlaciones)

# Conversion of matrix in long format
cor_melt <- melt(correlaciones)

# Creation of figure with  ggplot2
ggplot(data = cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()








#MIXED LOGIT 

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
#This output shows the parameter for the cost before estimating what e raised to the power of the coeffient is, which is the value we presented in the table, but we didn´t update the table with the standard error, which is why it was weird. So, this output shows us the result as it should, and if you take e raised to the power of the cost coefficient, then you get a similar value to what they present in the paper.


#Latent class model with EM algorithm

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
apollo_control <- list(
  modelName = "LatentClassModel",
  modelDescr = "Latent class logit model",
  indivID = "numeric_id_1", # Individual ID variable
  noValidation=TRUE,
  noDiagnostic=TRUE,
  outputDirectory= "output", 
  cores= 11
)


# Define apollo_beta with unique parameter names
apollo_beta <- c(
  asc_1_a = 0,
  asc_1_b = 0,
  asc_1_c = 0,
  asc_2_a=0,
  asc_2_b=0,
  asc_2_c=0,
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

# Fixed parameters 
apollo_fixed <- c()

#Latent class components
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["asc_1"]] = list(asc_1_a, asc_1_b,asc_1_c)
  lcpars[["asc_2"]] = list(asc_2_a, asc_2_b,asc_2_c)
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
  for(s in 1:length(pi_values)){
    
    
    ### List of utilities (later integrated in mnl_settings below) the same that in apollo_beta
    V = list()
    V[['chosen_1']] = asc_1[[s]] + prod2_beta[[s]]*prod2_1 + prod3_beta[[s]] * prod3_1 + rev2_beta[[s]] * rev2_1 + rev3_beta[[s]] * rev3_1 + rev4_beta[[s]] * rev4_1 + rev5_beta[[s]] * rev5_1 + eff2_beta[[s]]*eff2_1 + eff3_beta[[s]]*eff3_1 + cost_beta[[s]]*cost_1
    V[['chosen_2']] = asc_2[[s]] + prod2_beta[[s]]*prod2_2 + prod3_beta[[s]] * prod3_2 + rev2_beta[[s]] * rev2_2 + rev3_beta[[s]] * rev3_2 + rev4_beta[[s]] * rev4_2 + rev5_beta[[s]] * rev5_2 + eff2_beta[[s]]*eff2_2 + eff3_beta[[s]]*eff3_2 + cost_beta[[s]]*cost_2
    
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
model = apollo_lcEM(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,lcEM_settings = list(EMmaxIterations=100, stoppingCriterion = 0.00000000000000000000000000000000000000000000000001))

### Show output in screen
apollo_modelOutput(model)

#We haven´t obtained a result and we know that maybe is something wrong in our code, but we don´t find the solution so we have taken such as initial values the output of stata.


#Latent class model/ Second part
library(readxl)
library(apollo)
# library
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
apollo_control <- list(
  modelName = "LatentClassModel",
  modelDescr = "Latent class logit model",
  indivID = "numeric_id_1", # Individual ID variable
  outputDirectory= "output"
)

# Define apollo_beta with unique parameter names/ These values come from stata result (forvalues c = 2/4 {
###lclogit2 chosen, rand(prod2 prod3 rev2 rev3 rev4 rev5 eff2 eff3 cost) id(numeric_id) group(group) nclasses(`c') membership(male mid_age high_age mid_education high_education mid_income high_income miss_income low_meat) 
##	matrix b = e(b)
#	matrix ic = nullmat(ic) \ `e(nclasses)', `e(ll)',`=colsof(b)', `e(aic)', `e(bic)'
#	}
#matrix colnames ic = "Classess" "LL" "Nparam" "AIC" "BIC"
#matlist ic, name(columns) 
#lclogit2 chosen, rand(prod2 prod3 rev2 rev3 rev4 rev5 eff2 eff3 cost) id(numeric_id) group(group) nclasses(3) membership(male mid_age high_age mid_education high_education mid_income high_income miss_income low_meat) tolcheck
#matrix starta= e(b))

apollo_beta <- c(
  prod2_beta_a = -0.2559649,
  prod2_beta_b = 0.2783297,
  prod2_beta_c = 0.2762211,
  prod3_beta_a = 0.0594497,
  prod3_beta_b = 1.199785,
  prod3_beta_c = 0.1085642,
  rev2_beta_a = 1.223047,
  rev2_beta_b = 2.580808,
  rev2_beta_c = -0.0880984,
  rev3_beta_a = 1.178644,
  rev3_beta_b = 2.663,
  rev3_beta_c = -0.4449542,
  rev4_beta_a = 1.758921,
  rev4_beta_b = 2.433412,
  rev4_beta_c = 0.2720105,
  rev5_beta_a = 1.245557,
  rev5_beta_b = 3.4313,
  rev5_beta_c = -1.019053,
  eff2_beta_a = 0.3162111,
  eff2_beta_b = 1.071913,
  eff2_beta_c = 0.3574255,
  eff3_beta_a = 0.0382616,
  eff3_beta_b = 0.10826,
  eff3_beta_c = 1.09978,
  cost_beta_a = -0.003699,
  cost_beta_b = -0.1511691,
  cost_beta_c = -0.0209156,
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

# Fixed parameters 
apollo_fixed <- c()

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
    V[['chosen_1']] = prod2_beta[[s]]*prod2_1 + prod3_beta[[s]] * prod3_1 + rev2_beta[[s]] * rev2_1 + rev3_beta[[s]] * rev3_1 + rev4_beta[[s]] * rev4_1 + rev5_beta[[s]] * rev5_1 + eff2_beta[[s]]*eff2_1 + eff3_beta[[s]]*eff3_1 + cost_beta[[s]]*cost_1
    V[['chosen_2']] =prod2_beta[[s]]*prod2_2 + prod3_beta[[s]] * prod3_2 + rev2_beta[[s]] * rev2_2 + rev3_beta[[s]] * rev3_2 + rev4_beta[[s]] * rev4_2 + rev5_beta[[s]] * rev5_2 + eff2_beta[[s]]*eff2_2 + eff3_beta[[s]]*eff3_2 + cost_beta[[s]]*cost_2
    
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
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(estimationroutine = "nr"))


### Show output in screen
apollo_modelOutput(model)
