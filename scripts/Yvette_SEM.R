# Analysis woody cover
#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
library(dplyr)
# load the lavaan library
# install.packages("lavaan")
library(lavaan)

# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/edit?usp=sharing")
# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTZ5ObFosqVNTOMG6P87CCcqE4oiXYkuSQixNGR6qex1E4TbvT4wDsn6w5zC-hOVIOQRifzsbZXxuss/pub?gid=964721627&single=true&output=csv")

names(SEMdata)
# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% dplyr::select(burnfreq,dist2river,cec,
                                            rainfall,elevation,hills),
                    stars = T, ellipses = F)

psych::pairs.panels(SEMdatastd %>% dplyr::select(burnfreq,dist2river,cec,
                                          rainfall,elevation,hills,CorProtAr,woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach
multreg_std<-lm(woody~burnfreq+dist2river+cec+
                rainfall+elevation+hills+CorProtAr,data=SEMdatastd)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Woody_model<-"woody~burnfreq+dist2river+cec+
                rainfall+elevation+hills+CorProtAr
burnfreq~dist2river+cec+
                rainfall+elevation+hills+CorProtAr
dist2river~cec+rainfall+elevation+hills+CorProtAr
cec~rainfall+elevation+hills+CorProtAr
rainfall~elevation+hills+CorProtAr"
Woody_model
Woody_fit<-lavaan::sem(Woody_model,data=SEMdatastd)

# show the model results
summary(Woody_fit,standardized=T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# visualize the model
