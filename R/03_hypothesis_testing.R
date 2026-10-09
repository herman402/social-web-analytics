# COMP3020 Social Web Analytics
# Assessment 2 - Group Project
# Group 19
#
# Project:
# Generative AI Discussions on Bluesky
#
# File:
# 03_hypothesis_testing.R
#
# Purpose:
# Test whether receiving at least one like is associated
# with the generative AI topic of a Bluesky post.
#
# Lecture:
# Module 3 - Simple Exposure Analysis

# Research Question:
# Is receiving at least one like independent of the generative-AI topic 
# ...being discussed (ChatGPT, Claude AI or Google Gemini)?

# “We want to test whether the AI topic has any relationship with whether 
# ...a post received at least one like.”

# Load the raw Bluesky datasets
load("data/raw/bluesky_raw_data.RData")

# Create topic variable
# This tells us whether each post belongs to ChatGPT, Claude or Gemini
topic = c(
  rep("ChatGPT", nrow(search_skeets_ChatGPT)),
  rep("Claude", nrow(search_skeets_Claude)),
  rep("Gemini", nrow(search_skeets_Gemini))
)

# Determine whether each post received at least one like
# TRUE  = the post received at least one like
# FALSE = the post received no likes
liked = c(
  search_skeets_ChatGPT$like_count > 0,
  search_skeets_Claude$like_count > 0,
  search_skeets_Gemini$like_count > 0
)

# Create contingency table
tab = table(topic, liked)

# Hypotheses:
# H0: AI topic and receiving at least one like are independent.
# HA: AI topic and receiving at least one like are associated.

# The contingency table counts how many posts received
# no likes (FALSE) or at least one like (TRUE) for each AI topic.
tab

# Chi-squared test for independence
chi_test = chisq.test(
  tab,
  simulate.p.value = TRUE
)

# Observed frequencies = "what actually happened in our dataset"
# Observed counts:
# These are the actual numbers from our raw Bluesky dataset. They show how many posts in each AI topic received
# no likes (FALSE) or at least one like (TRUE)
chi_test$observed

# ChatGPT:
# 184 posts received (no likes) and 113 received at (least one like).
# Total = 297 posts.

# Claude:
# 144 posts received (no likes) and 56 received at (least one like).
# Total = 200 posts.

# Gemini:
# 137 posts received (no likes) and 63 received at (least one like).
# Total = 200 posts.

# Calculating the percentage of posts that received at least one like.
# TRUE is the number of posts with at least one like.
# rowSums(tab) gives the total number of posts for each AI topic.
like_percent = tab[, "TRUE"] / rowSums(tab) * 100

barplot(
  like_percent,
  col = c("skyblue", "orange", "lightgreen"),
  ylim = c(0, 45),
  ylab = "Posts Receiving at Least One Like (%)",
  xlab = "AI Topic",
  main = "Posts Receiving at Least One Like"
)
like_percent

# Results:
# ChatGPT = 38.0%
# Claude  = 28.0%
# Gemini  = 31.5%
#
# ChatGPT had the highest percentage of posts receiving at least
# one like in our sample, followed by Gemini and then Claude.
#
# These percentages describe what we observed in the sample.
# The chi-squared test checks whether the differences are statistically significant.

# Expected frequencies under independence = "What we would expect under independence (not related)"
# These are the counts we would expect if AI topic and receiving a like were not related.
# The chi-squared test compares these with the observed counts.
chi_test$expected

# Expected <-- calculated assuming the topic is unrelated to likes
# Observed <-- what actually occurred in our collected data 

# ChatGPT: expected about 99 liked posts, but actually had observed 113
# Claude: expected about 67 liked posts, but actually had observed 56
# Gemini: expected about 67 liked posts, but actually had observed 63


# Why we think chi-squared test is suitable:
# We are comparing two categorical variables:
# 1. AI topic: ChatGPT, Claude or Gemini
# 2. Whether the post received at least one like: TRUE or FALSE
#
# Also a chi-squared test for independence is appropriate because it checks
# whether two categorical variables are associated with each other.

# Now, Comparing the observed counts with the expected counts to check whether AI topic 
# ...and receiving a like are associated.
chi_test

# Hence, the Chi-squared test / x-squared = 5.8357, and the p-value of 0.05447 (may vary)
# slightly above 0.05, so we fail to reject the null hypothesis, meaning there is 
# ...not enough evidence to show an association.
# Therefore, there is not enough statistical evidence to say that receiving a like is associated 
# ...with whether the post is about ChatGPT, Claude, or Gemini. So, H0 remains.

# Additional: Show the p-value from the test
chi_test$p.value

# The p-value is approximately 0.054.
# Since p > 0.05, we fail to reject H0.

# Summary:
# ChatGPT had the highest proportion of posts receiving at least one like
# at 38.0%, followed by Gemini at 31.5% and Claude at 28.0%.
#
# The observed counts were different from the expected counts,
# but the simulated p-value was approximately 0.054.
#
# Since p > 0.05, we fail to reject H0.
# Therefore, the differences observed in the sample were not
# statistically significant at the 5% significance level.
#
# This means there is not enough evidence to conclude that
# AI topic is associated with receiving at least one like.
#
# Limitation:
# The posts were collected over a short time window,
# so engagement may change as posts receive more likes over time.
