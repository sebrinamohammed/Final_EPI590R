

library(tidyverse)
library(janitor)
library(here)
library(fs)

working_dir <- here::here()

url <- "https://bit.ly/gacttCSV"

coffee_survey_raw <- readr::read_csv(url)

# Grab the raw questions for the dictionary.
coffee_survey_raw |>
	colnames() |>
	cat(sep = "\n")

coffee_survey <- coffee_survey_raw |>
	janitor::clean_names() |>
	# Get rid of one-hot encoding; users can do that if they'd like. Also,
	# "flavorings" columns are empty.
	dplyr::select(
		submission_id,
		age = what_is_your_age,
		cups = how_many_cups_of_coffee_do_you_typically_drink_per_day,
		where_drink = where_do_you_typically_drink_coffee,
		brew = how_do_you_brew_coffee_at_home,
		brew_other = how_else_do_you_brew_coffee_at_home,
		purchase = on_the_go_where_do_you_typically_purchase_coffee,
		purchase_other = where_else_do_you_purchase_coffee,
		favorite = what_is_your_favorite_coffee_drink,
		favorite_specify = please_specify_what_your_favorite_coffee_drink_is,
		additions = do_you_usually_add_anything_to_your_coffee,
		additions_other = what_else_do_you_add_to_your_coffee,
		dairy = what_kind_of_dairy_do_you_add,
		sweetener = what_kind_of_sugar_or_sweetener_do_you_add,
		style = before_todays_tasting_which_of_the_following_best_described_what_kind_of_coffee_you_like,
		strength = how_strong_do_you_like_your_coffee,
		roast_level = what_roast_level_of_coffee_do_you_prefer,
		caffeine = how_much_caffeine_do_you_like_in_your_coffee,
		expertise = lastly_how_would_you_rate_your_own_coffee_expertise,
		starts_with("coffee"),
		prefer_abc = between_coffee_a_coffee_b_and_coffee_c_which_did_you_prefer,
		prefer_ad = between_coffee_a_and_coffee_d_which_did_you_prefer,
		prefer_overall = lastly_what_was_your_favorite_overall_coffee,
		wfh = do_you_work_from_home_or_in_person,
		total_spend = in_total_much_money_do_you_typically_spend_on_coffee_in_a_month,
		why_drink = why_do_you_drink_coffee,
		why_drink_other = other_reason_for_drinking_coffee,
		taste = do_you_like_the_taste_of_coffee,
		know_source = do_you_know_where_your_coffee_comes_from,
		most_paid = what_is_the_most_youve_ever_paid_for_a_cup_of_coffee,
		most_willing = what_is_the_most_youd_ever_be_willing_to_pay_for_a_cup_of_coffee,
		value_cafe = do_you_feel_like_you_re_getting_good_value_for_your_money_when_you_buy_coffee_at_a_cafe,
		spent_equipment = approximately_how_much_have_you_spent_on_coffee_equipment_in_the_past_5_years,
		value_equipment = do_you_feel_like_you_re_getting_good_value_for_your_money_with_regards_to_your_coffee_equipment,
		gender,
		gender_specify = gender_please_specify,
		education_level,
		ethnicity_race,
		ethnicity_race_specify = ethnicity_race_please_specify,
		employment_status,
		number_children = number_of_children,
		political_affiliation
	)

readr::write_csv(
	coffee_survey,
	fs::path(working_dir, "coffee_survey.csv")
)



#table sumamry ######Create a {gtsummary} table of descriptive statistics about
#your data (1 pt)

install.packages("tidyverse")
install.packages("gtsummary")
install.packages("here")

library(tidyverse)
library(gtsummary)
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





########Fit a regression andpresent well-formatted results from the regression (1 pt)
####The regression doesn’t have to be of any particular scientific interest,
#and you don’t have to interpret it in any particular way ####You may use {broom} or
#{gtsummary} or both ###IDK


install.packages("nnet")

library(nnet)  # For multinomial logistic regression
library(gtsummary)  # For creating nice regression tables
model <- multinom(age ~ coffee_a_personal_preference +
										coffee_b_personal_preference +
										coffee_c_personal_preference +
										coffee_d_personal_preference,
									data = coffee_survey)

tbl_regression(model)



#CREATE A FIGURE.
#i used coffee_preference b/c those were the only numerical values in my data

# par() sets graphical parameters in R.
#mfrow() means multiple figure row-wise
#(2, 2) first 2 = number of rows, second 2= number of columns so you can display four plots in one

par(mfrow = c(2, 2))


create_hist <- function(coffee_survey, title) {
	hist(coffee_survey,
			 main = title,
			 xlab = "Preference Score",  #x-axis label
			 ylab = "Frequency",    #y-axis label
			 xlim = c(0.5, 5.5),  # Set x-axis limits
			 breaks = seq(0.5, 5.5, by = 1),  # Breaks for 1-5 scale
			 col = "skyblue",
			 xaxt = "n")  # Remove x-axis labels temporarily
	axis(1, at = 1:5)  # Add x-axis labels for 1-5
}

# I will Create histograms for each variable
create_hist(coffee_survey$coffee_a_personal_preference, "Coffee A Preference")
create_hist(coffee_survey$coffee_b_personal_preference, "Coffee B Preference")
create_hist(coffee_survey$coffee_c_personal_preference, "Coffee C Preference")
create_hist(coffee_survey$coffee_d_personal_preference, "Coffee D Preference")

# Reset plotting window
par(mfrow = c(1, 1))



#Write and use a function that does something with the data (1 pt)

#It could be as simple as, for example, a new function that you write by hand
#to calculate the standard deviation of a variable (like we did with the mean)

#you're making a new function called 'get_mode'
get_mode <- function(x) {
	unique_x <- unique(x)
	unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Calculate mode for each coffee preference
mode_coffee_a_preference <- get_mode(coffee_survey$coffee_a_personal_preference)
mode_coffee_b_preference <- get_mode(coffee_survey$coffee_b_personal_preference)
mode_coffee_c_preference <- get_mode(coffee_survey$coffee_c_personal_preference)
mode_coffee_d_preference <- get_mode(coffee_survey$coffee_d_personal_preference)


# Print the results
print(paste("Mode of Coffee A Preference:", mode_coffee_a_preference))
print(paste("Mode of Coffee B Preference:", mode_coffee_b_preference))
print(paste("Mode of Coffee C Preference:", mode_coffee_c_preference))
print(paste("Mode of Coffee D Preference:", mode_coffee_d_preference))
