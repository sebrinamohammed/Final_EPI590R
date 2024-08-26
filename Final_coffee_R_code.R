install.packages("gtsummary")
install.packages("tidyverse")
install.packages("here")
install.packages("janitor")
install.packages("fs")
install.packages("readr")
install.packages("dply")
library(dplyr)
library(gtsummary)
library(readr)
library(tidyverse)
library(janitor)
library(here)
library(fs)

#load data
coffee_survey <- readr::read_csv(here::here("coffee_survey.csv"))
View(coffee_survey)
summary(coffee_survey)
#table sumamry
######Create a {gtsummary} table of descriptive statistics about your data (1 pt)

tbl_summary(
	coffee_survey,
	by = age,
	include = c(
		employment_status, strength, caffeine, gender
	)
)
########Fit a regression and present well-formatted results from the regression (1 pt)
####The regression doesn’t have to be of any particular scientific interest, and you don’t have to interpret it in any particular way
####You may use {broom} or {gtsummary} or both
###IDK

model <- multinom(age ~ coffee_a_personal_preference +
										coffee_b_personal_preference +
										coffee_c_personal_preference +
										coffee_d_personal_preference,
									data = coffee_survey)
tbl_regression(model)

###multinomial model
library(nnet)
model <- multinom(employment_status ~ age + gender + strength + caffeine, data = coffee_survey)
tbl_regression(model)












