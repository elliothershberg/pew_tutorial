library(tidyverse)
library(infer)

#in order to read this in, this needs to be in your working directory
setwd("~/Documents/Data Science Projects/January 3-10, 2018 - Core Trends Survey")
jan_core_trends_survey <- read_csv("January 3-10, 2018 - Core Trends Survey - CSV.csv")

#looking at the rows, and types of data
str(jan_core_trends_survey)

#dimensions: 70 variables, 2002 observations
length(jan_core_trends_survey)
nrow(jan_core_trends_survey)

#every option from survey is present:
# About how often do you use the internet? [READ] {PIAL Trend, most recently May 2016 – Cybersecurity}
# 1	Almost constantly
# 2	Several times a day
# 3	About once a day
# 4	Several times a week, OR
# 5	Less often?
#         8	(VOL.) Don’t know
# 9	(VOL.) Refused
# 
unique(jan_core_trends_survey$intfreq)

unique(jan_core_trends_survey$age)

#let's see how age is distributed

age_hist <- ggplot(jan_core_trends_survey, aes(age)) +
        geom_histogram(bins = 20)

age_hist

#interesting, it looks like the distribution is skewed left. When you think about it, this is expected for the age of a population

# Please tell me if you ever use any of the following social media sites online or on your cell phone. 
#Do you ever use... [INSERT ITEMS; RANDOMIZE]? {Modified PIAL Trend, most recently April 2016 - Libraries}
# a.	Twitter
# b.	Instagram
# c.	Facebook 
# d.	Snapchat
# e.	YouTube
# f.	WhatsApp
# g.	Pinterest
# h.	LinkedIn
# CATEGORIES
# 1	Yes, do this
# 2	No, do not do this
# 8	(VOL.) Don't know
# 	9	(VOL.) Refused

unique(jan_core_trends_survey$web1a)

#Function to calculate average age of users vs. non users

avg_user_ages <- function(df, group, var) {
        group <- enquo(group)
        var <- enquo(var)
        df %>%
                select(!!group, !!var) %>%
                filter(!!group == 1 | !!group == 2) %>%
                group_by(!!group) %>%
                summarize(avg_age = mean(!!var))
}

twitter_age <- avg_user_ages(jan_core_trends_survey, web1a, age)

twitter_age

instagram_age <- avg_user_ages(jan_core_trends_survey, web1b, age)

instagram_age

facebook_age <- avg_user_ages(jan_core_trends_survey, web1c, age)

facebook_age

snapchat_age <- avg_user_ages(jan_core_trends_survey, web1d, age)

snapchat_age

unique(jan_core_trends_survey$web1d)

#making a dataframe of snapchat users and non-users and renaming factor

snap_df <- jan_core_trends_survey %>%
        filter(web1d == 1 | web1d == 2)

unique(snap_df$web1d)

snap_df$web1d <- as.factor(snap_df$web1d)

snap_df$web1d <- plyr::revalue(snap_df$web1d, c("1"="user", "2"="non_user"))

levels(snap_df$web1d)

#now I have the data that I want to plot. Here is the plot:

snap_age_plot <- ggplot(snap_df, aes(x = web1d, y = age)) +
        geom_boxplot() +
        xlab("Snapchat Usage") +
        ylab("Age")

snap_age_plot

## Conduct a hypothesis test using infer for diff of means


obs_diff <- snapchat_age$avg_age[2] - snapchat_age$avg_age[1]

diff_age_mean <- snap_df %>%
        specify(age ~ web1d) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 10000, type = "permute") %>%
        calculate(stat = "diff in means", order = c("non_user", "user"))

p <- diff_age_mean %>%
        filter(stat > obs_diff) %>%
        summarize(p = n() / 10000)

p

age_mean_conf <- snap_df %>%
        # Specify the variable of interest
        specify(age ~ web1d) %>%  
        # Generate 15000 bootstrap samples
        generate(reps = 10000, type = "bootstrap") %>% 
        # Calculate the median of each bootstrap sample
        calculate(stat = "diff in means", order = c("non_user", "user"))

head(age_mean_conf)

age_mean_conf %>%
        summarize(lower = quantile(stat, 0.025),
                  upper = quantile(stat, 0.975))

