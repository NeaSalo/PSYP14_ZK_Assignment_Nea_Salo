
# Packages used.
library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse) # for tidy code	
library(lm.beta) #Adds standardized regression coefficients to objects created by lm
library(sandwich) #model-robust covariance matrix estimators
library(sjPlot) #table output functions for data visualization
library (caret) #contains functions to streamline the model training process for complex regression and classification problems
library(leaps) #exhaustive search for the best subsets of the variables 
library(sjmisc) #recoding, setting and replacing missing values

# Custom function for the coefficient table made by Zoltan Kekecs.
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

# Research Question 1

# The data file is brought into R and saved as an object under data_file_1.

data_file_1 <- read.csv("https://tinyurl.com/yxm5rd89")

# Next the data_file_1 is viewed.
View(data_file_1)

# Before starting anything, I wanted to see the summary of descriptive data.
data_file_1 %>% 
  summary()

# There are two odd data points I saw in the summary:
# 1. Pain is rated from 1-10 but there is a datapoint of 55, which I will recode to 5.
# 2. STAI_trait is between 20-80 and there is a datapoint of 4.2, which I will recode to 42.

data_file_1 <- data_file_1 %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait=="4.2", 42)) %>% 
  mutate(pain = replace(pain, pain == "55", 5))

# Histograms looking at all variables that will be used in the hierarchical regression: age, sex, STAI_traits, pain_cat,
# mindfulness, cortisol serum, cortisol saliva and pain.
data_file_1 %>% 
  ggplot()+
  aes(x = age)+
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()

data_file_1 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()


# Hierarchical regression models:
# Model 1: pain predicted through variables age and sex.
model_1 = lm(pain ~ age + sex, data = data_file_1)
summary(model_1)

AIC(model_1)

confint(model_1)
lm.beta(model_1)

# Identifying extreme cases
model_1 %>% 	
  plot(which = 5)	

model_1 %>% 	
  plot(which = 4)	

# Choosing the extreme cases and looking at their descriptive data.
# No outliers found.
data_file_1 %>%
  slice(c(8,23,47))

# No outliers in the data of the extreme cases so they will be left as it.

# Assumptions of regression:

#1. Normality = all points follow the diagonal line so normality is found.
model_1 %>%
  plot(which = 2)

# Next, a histogram was created to further look at extreme cases.
residuals_model_1 = enframe(residuals(model_1))

residuals_model_1 %>%
  ggplot() + aes(x = value) + geom_histogram()

# Skew and kurtosis was also looked at. 
# Model 1 skew = 0.11 and kurtosis = 0.01 and since they are between -1 and 1, there is no violation.
describe(residuals(model_1))

# Coefficients for model 1
coef_table(model_1)

# Coefficients table for model 1
tab_model(model_1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)


# Model 2: pain is predicted by the following variables age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness
model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_file_1)
summary(model_2)

AIC(model_2)

confint(model_2)
lm.beta(model_2)

# Identifying extreme cases
model_2 %>% 	
  plot(which = 5)	

model_2 %>% 	
  plot(which = 4)	

# Choosing the extreme cases and looking at their descriptive data.
# No odd outliers.
data_file_1 %>%
  slice(c(47,74,86))

# Assumptions of regression:

#1. Normality
model_2 %>%
  plot(which = 2)

# Next, a histogram was created to further look at extreme cases.
residuals_model_2 = enframe(residuals(model_2))

residuals_model_2 %>%
  ggplot() + aes(x = value) + geom_histogram()

# Skew and kurtosis was also looked at. Model 2 within the range -1 and 1: skew= -0.14 and kurtosis 0, thus no 
# violation.
describe(residuals(model_2))

# Linearity

model_2 %>%
  residualPlots()

# There is some curvature on all plots, but the test results are not significant so linearity is true for our model_2. 

# Next, homoscedasticity is tested.

model_2 %>%
  plot(which = 3)

model_2 %>%
  ncvTest()
model_2 %>%
  bptest()

# Since both p values in the Non-constant Variance Score Test 0.90374 and in the studentized Breusch-Pagan test 0.8765 
# are bigger than 0,05 it means that there is no violation of homoscedasticity and we can continue with this data.

# Next we check multicollinearity

model_2 %>% 
  vif()

# Both cortisol measures are above 3 meaning that there is multicollinearity to be detected. Due to cortisol_serum and cortisol_saliva being very similar in terms of it including cortisol, I chose to disregard 
# cortisol_saliva from the regression analysis, because cortisol serum is often regarded as more reliably related to stress in medical research

model_2_final = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_file_1)
summary(model_2_final)

# VIF is checked again and removing cortisol_saliva from the analysis lead to a non problematic multicollinearity.
vif(model_2_final)

# Now I'm going trough all the steps again to make sure that model_2_final corresponds to all the checks.

# Normality
model_2_final %>%
  plot(which = 2)

#Normality histogram
residuals_model_2_new = enframe(residuals(model_2_final))
residuals_model_2_new %>%
  ggplot() + aes(x = value) + geom_histogram() + stat_bin()

#Skew and kurtosis checked and no violation is found as skew = -0.18 and kurtosis = 0.06
describe(residuals(model_2_final))

# Linearity = all findings are aboe 0.05 thus no violation on linearity
model_2_final %>%
  residualPlots()

# Homoscedasticity
model_2_final %>%
  plot(which = 3)

# Non-constatnt Variance Score Test = 0.81.
model_2_final%>%
  ncvTest()
# Breush-Pagan test = 0.89.
model_2_final%>%
  bptest()

# Multicollinearity = all are under 3.
vif(model_2_final)

# AIC value
AIC(model_2_final)

# Coefficients table for the final model 2.
coef_table(model_2_final)

# Final table to save 
tab_model(model_2_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# Summary
summary(model_2_final)

# ANOVA = model 2 is better than model 1
anova(model_1, model_2_final)




# ASSIGNMENT 2

# Data to be used is data_file_1 where data points 88 and 34 have been replaced.

# Pain was predicted using the following age, sex, STAI_trait, pain_cat, mindfulness, serum_cortisol, weight, IQ, household income.

# Diagnostics are run again, because we are working with different variables.

assignment2_forward = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_file_1)

assignment2_forward %>% 
  plot(which = 5)

assignment2_forward %>% 
  plot(which = 4)

# Normality
assignment2_forward %>%
  plot(which = 2)

#There is no big outliers that I see so I will not exclude any. Even though they are not exactly in line with normality, it is close enough.

#Normality histogram
residuals_model_assignment2 = enframe(residuals(assignment2_forward))
residuals_model_assignment2 %>%
  ggplot() + aes(x = value) + geom_histogram() + stat_bin()

summary(assignment2_forward)

#Skew and kurtosis checked
describe(residuals(assignment2_forward))

# Linearity
assignment2_forward %>%
  residualPlots()

# Homoscedasticity
assignment2_forward %>%
  plot(which = 3)

# Non-constatnt Variance Score Test
assignment2_forward%>%
  ncvTest()
# Breush-Pagan test
assignment2_forward%>%
  bptest()

# Multicollinearity
vif(assignment2_forward)

# AIC value
AIC(assignment2_forward)

# Coefficients table for the final model 2.
coef_table(assignment2_forward)

tab_model(assignment2_forward, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# Stepwise backwards regression
step(assignment2_forward, direction = "backward")

backward_model <- lm(pain ~ age + mindfulness + cortisol_serum + pain_cat, data = data_file_1)
summary(backward_model)

# Normality
backward_model %>%
  plot(which = 2)

#Normality histogram
residuals_backward_model = enframe(residuals(backward_model))
residuals_backward_model %>%
  ggplot() + aes(x = value) + geom_histogram() + stat_bin()

#Skew and kurtosis checked
describe(residuals(backward_model))

# Linearity
backward_model %>%
  residualPlots()

# Homoscedasticity
backward_model %>%
  plot(which = 3)

# Non-constatnt Variance Score Test
backward_model%>%
  ncvTest()
# Breush-Pagan test
backward_model%>%
  bptest()

# Multicollinearity
vif(backward_model)

# AIC value
AIC(backward_model)

# Coefficients table for the final model 2.
coef_table(backward_model)

# Final table to save 
tab_model(backward_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#Changing the name of the final regression model arrived at in assignment 1
theory_based_model <- model_2_final
theory_based_model

# Anova checked and there is no significant difference between the two models
# AIC was lower for the backward model thus it is a better model

AIC(assignment2_forward)
AIC(backward_model)
AIC(assignment2_forward)-AIC(backward_model)

summary(assignment2_forward)$adj.r.squared
summary(backward_model)$adj.r.squared

anova(assignment2_forward, backward_model)

AIC(theory_based_model)
AIC(backward_model)

AIC(theory_based_model)-AIC(backward_model)

summary(theory_based_model)$adj.r.squared
summary(backward_model)$adj.r.squared

anova(theory_based_model, backward_model)

# Anova is not significant, however the AIC was significant due it being over 2. Therefore the backward model is more robust.

# New data file brought in called data_file_2

data_file_2 <- read.csv("https://tinyurl.com/87v6emky")
View(data_file_2)
summary(data_file_2)
describe(data_file_2)

#The summary and description showed no quirky data so I will be keeping all data from the 160pps.

# Histograms 
data_file_2 %>% 
  ggplot()+
  aes(x = age)+
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()

data_file_2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()


# Predictions made from the theory based model and backward model found earlier in assignment 2

data_file_2_prediction_theory <-predict(theory_based_model,data_file_2)
data_file_2_prediction_theory
data_file_2_prediction_backward <- predict(backward_model,data_file_2)
data_file_2_prediction_backward

# These regression equations should be applied on the new data (data file 2), to predict pain.) 
# Compare the predicted values with the actual pain ratings. 
# Which model was able to predict the actual pain ratings in data file 2 better?

RSS_prediction <- sum((data_file_2[, "pain"] - data_file_2_prediction_theory)^2)
RSS_backward <- sum((data_file_2[, "pain"] - data_file_2_prediction_backward)^2)

RSS_prediction
RSS_backward

# The difference
RSS_backward - RSS_prediction

# The theory based model has less error than the backward model suggesting that the theory based model is better at predicting the actual pain ratings in 
# data file 2.



# ASSIGNMENT 3 

# Loading more packages:
library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(gridExtra)

# Custom function for standardized beta coefficients from linear mixed models
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

# Loading data
data_file_3 <- read.csv("https://tinyurl.com/b385chpu")
view(data_file_3)

data_file_4 <-read.csv("https://tinyurl.com/4f8thztv")
view(data_file_4)

# First, build a linear mixed model on data file 3,
# accounting for the clustering of the data at different hospital sites.

# Data file 3 descriptives are looked at for any possible outliers.
summary(data_file_3)

# Mutate sex and hospital variables into factors. 

data_file_3 = data_file_3 %>% 
  mutate(sex = factor(sex))

is.factor(data_file_3$sex)

data_file_3 = data_file_3 %>% 
  mutate(hospital = factor(hospital))

is.factor((data_file_3$hospital))

summary(data_file_3$sex)
summary(data_file_3$hospital)

# There was a minus income for ID 2, which I will be keeping even though it seemed odd, but it has been said that
# unusual or NA data in mixed linear models does not lead to problems for our mixed model analyses (Winter,).
# There is one participant who has put woman, which I will be changing into female.

data_file_3 <- data_file_3 %>% 
  mutate(sex = replace(sex, sex=="woman", "female"))
  
summary(data_file_3)
describe(data_file_3)


# Ordering the hospitals

data_file_3 = data_file_3 %>%
  mutate(hospital = factor(hospital, levels = (c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7",
                                                 "hospital_8", "hospital_9", "hospital_10"))))

# Viewing data on plots

dev.off() # To get rid of ggplot error message I kept receiving.

DT3_plot1 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = age, color = hospital) + 
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)                                         

DT3_plot1


DT3_plot2 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = sex, color = hospital)+
      geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot2

DT3_plot3 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = STAI_trait, color = hospital)+
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot3

DT3_plot4 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = pain_cat, color = hospital)+
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot4

DT3_plot5 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = mindfulness, color = hospital)+
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot5

DT3_plot6 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = cortisol_serum, color = hospital)+
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot6

DT3_plot7 = data_file_3 %>% 
  ggplot()+aes(y = pain, x = cortisol_saliva, color = hospital)+
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)

DT3_plot7

# A random intercept model created

mod_random_data3 = lmer(pain ~ age + sex + pain_cat + STAI_trait + mindfulness + cortisol_serum + (1 | hospital), data = data_file_3)
mod_random_data3

# Checking model coefficients and confidence intervals.
summary(mod_random_data3)
confint(mod_random_data3)
anova(mod_random_data3)

# Comparing the random intercept model mod_random_data3 findings with the assignment 1's final model theory_based_model.
mod_random_data3_table <- tab_model(mod_random_data3, theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
mod_random_data3_table

# compute the variance explained by the fixed effect predictors using marginal R2, 
# and the variance explained by the fixed and random effect terms combined using conditional R2.

# Marginal R squared with confidence interval:
r2beta(mod_random_data3,method = "nsj", data = data_file_3)

# Marginal and conditional R squared values:
r.squaredGLMM(mod_random_data3)

# Model fit of the random intercept checked:
cAIC(mod_random_data3)$caic


# Now use the regression equation obtained on data file 3 to predict pain in data file 4. 
# (IMPORTANT: just like in assignment part 2, do not fit the regression models on data file 4 (don???t re-train your models), 
# just use the regression equation you derived based on data file 3.

# Data file 4's descriptive data checked and nothing looks odd.
view(data_file_4)
summary(data_file_4)
describe(data_file_4)


data_file_4_predict <-  predict(mod_random_data3, data_file_4, allow.new.levels = TRUE)
data_file_4_predict

RSS_prediction_file_4 <- sum((data_file_4[, "pain"] - data_file_4_predict)^2)
RSS_prediction_file_4

# Now compute the variance explained by the model on data file 4. You can do this by using the formula we learned in class: 1-(RSS/TSS). 
# Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3.

# TSS is the total sum of squares associated with the outcome variable, which is the sum of the squares of the measurements minus their mean.
# sum((data_file_4_prediction$pain - mean(data_file_4_prediction$pain))^2)
# Let's calculate the mean 

data_file_4_mean = lm(pain ~ 1, data = data_file_4 )
data_file_4_mean

TSS_file_4 = sum((data_file_4$pain - predict(data_file_4_mean))^2)
TSS_file_4

R2 = 1 - (RSS_prediction_file_4 / TSS_file_4)
R2

# R2 equals 0.3797384

# Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3.



# Build a new linear mixed effects model on dataset 3 predicting pain. 
# However, instead of including all predictors, you should only include the most influential predictor from the previous model. 
# Allow for both random intercept and random slope. 
# Now visualize the fitted regression lines for each hospital separately.

# Bringing my data model from data file three and figuring out what the most influential predictor was.
mod_random_data3
summary(mod_random_data3)
ranova(mod_random_data3)

# Looking at the summary output and the estimates, it shows that cortisol serum is the most influential predictor.

mod_random_int_paincat = lmer(pain ~ cortisol_serum + (1|hospital), data = data_file_3)
mod_random_int_paincat

coef(mod_random_int_paincat)

# Now I am going to do the random slope model with cortisol serum.

mod_random_slope_paincat= lmer(pain ~ cortisol_serum + (cortisol_serum| hospital),
                     data = data_file_3)

mod_random_slope_paincat
coef(mod_random_slope_paincat)

# boundary (singular) fit: see ?isSingular message received, which could be due to the small sample size, meaning the current study design does not support the complex modelling ambitions.
# More observatiosn are needed.

# Visualisation

dev.off()

data_file_3 = data_file_3 %>%
  mutate(pred_intercept = predict(mod_random_int_paincat), pred_slope = predict(mod_random_slope_paincat))

grid_1 <- data_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 5) + geom_line(color = "red",
  aes(y = pred_intercept, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2) 

grid_1

grid_2 <- data_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 5) + geom_line(color = "red",
  aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2) 

grid_2

grid.arrange(grid_1, grid_2, ncol=2)


# Comparing model fit indices

sum(residuals(mod_random_data3)^2)
sum(residuals(mod_random_int_paincat)^2)
sum(residuals(mod_random_slope_paincat)^2)


# cAIC checked.
cAIC(mod_random_int_paincat)$caic
cAIC(mod_random_slope_paincat)$caic

# Anova
anova(mod_random_int_paincat, mod_random_slope_paincat)

# marginal R squared with confidence intervals
r2beta(mod_random_int_paincat,method = "nsj", data = data_file_3)
r2beta(mod_random_slope_paincat, method = "nsj", data = data_file_3)

# marginal and conditional R squared values
r.squaredGLMM(mod_random_int_paincat)
r.squaredGLMM(mod_random_slope_paincat)

stdCoef.merMod(mod_random_int_paincat)
stdCoef.merMod(mod_random_slope_paincat)


