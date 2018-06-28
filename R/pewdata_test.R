library(tidyverse)
library(infer)

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

#interesting, it looks like the distribution is somewhat bimodial, with a peak around 30, and another around 70

# Please tell me if you ever use any of the following social media sites online or on your cell phone. Do you ever use... [INSERT ITEMS; RANDOMIZE]? {Modified PIAL Trend, most recently April 2016 - Libraries}
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

#calculate average age of twitter users vs. non users

#refactor calculating average ages:
#currently not working, but I can work on it tomorrow

avg_user_ages <- function(platform) {
        jan_core_trends_survey %>%
                select(platform, age) %>%
                filter(!is.na(platform)) %>%
                group_by(platform) %>%
                summarize(avg_age = mean(age))
}

avg_user_ages(jan_core_trends_survey$web1a)

twitter_age <- jan_core_trends_survey %>%
        select(web1a, age) %>%
        filter(!is.na(web1a)) %>%
        group_by(web1a) %>%
        summarize(avg_age = mean(age))

twitter_age

instagram_age <- jan_core_trends_survey %>%
        select(web1b, age) %>%
        filter(!is.na(web1b)) %>%
        group_by(web1b) %>%
        summarize(avg_age = mean(age))

instagram_age

facebook_age <- jan_core_trends_survey %>%
        select(web1c, age) %>%
        filter(!is.na(web1c)) %>%
        group_by(web1c) %>%
        summarize(avg_age = mean(age))

facebook_age

snapchat_age <- jan_core_trends_survey %>%
        select(web1d, age) %>%
        filter(!is.na(web1d)) %>%
        group_by(web1d) %>%
        summarize(avg_age = mean(age))

snapchat_age

unique(jan_core_trends_survey$web1d)


snap_df <- jan_core_trends_survey %>%
        filter(web1d == 1 | web1d == 2)

unique(snap_df$web1d)

snap_df$web1d <- as.factor(snap_df$web1d)


snap_age_plot <- ggplot(snap_df, aes(x = web1d, y = age, group = web1d)) +
        geom_boxplot()


snap_age_plot

## I need to figure out how to fix the boxplot of age by snapchat (user = 1, non-user = 2)
## Jk I fixed it :)
## But I need to clean it up and make it look nice, labels, etc.




## Conduct a hypothesis test using infer for diff of means



