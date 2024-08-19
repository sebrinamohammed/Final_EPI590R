install.packages("gtsummary")
library(gtsummary)

library(readr)
coffee_survey <- read_csv("coffee_survey.csv")
View(coffee_survey)

tbl_summary(
	coffee_survey,
	by = roast_level,
	include = c(caffeine, style, strength,
							total_spend, spent_equipment,wfh,gender, prefer_abc, prefer_ad, prefer_overall))
#label = list()
#statistic=list()
#digits=listis()



