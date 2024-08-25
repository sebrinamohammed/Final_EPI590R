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
		cups,where_drink, brew, purchase,favorite, additions,
		dairy, sweetener, strength, style, roast_level, caffeine, expertise, coffee_a_bitter,
		coffee_a_acid,coffee_a_pref, coffee_b_bitter, coffee_b_acid,
		coffee_b_pref, coffee_c_bitter, coffee_c_acid, coffee_c_pref, coffee_c_bitter, coffee_d_acid,
		coffee_d_pref, prefer_abc,	prefer_ad, prefer_overall, wfh,	total_spend,	why_drink,	taste, know_source, most_paid,
		most_willing, value_cafe,	spent_equipment,	value_equipment,	gender, education_level,
		ethnicity_race, employment_status,	number_children, political_affiliation
		)
	)
########Fit a regression and present well-formatted results from the regression (1 pt)
####The regression doesn’t have to be of any particular scientific interest, and you don’t have to interpret it in any particular way
####You may use {broom} or {gtsummary} or both
###IDK
tbl_uvregression(
	coffee_survey,
	y = strength,
	include = c(
		education_level,
		ethnicity_race, employment_status,	number_children, political_affiliation
	),
	method = glm, # Generalized linear model
	method.args = list(family = binomial) # Logistic regression for categorical outcomes
)













