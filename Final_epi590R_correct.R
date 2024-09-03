


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

# Create histograms for each variable
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





















