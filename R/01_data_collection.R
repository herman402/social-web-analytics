# COMP3020 Social Web Analytics
# Assessment 2 - Group Project
# Group 19
#
# Project:
# Generative AI Discussions on Bluesky
#
# File:
# 01_data_collection.R
#
# Purpose:
# Retrieve and inspect Bluesky posts for the project.
#
# Lecture / Lab Source:
# Module 4 - Text Mining 1, Part 1


# Bluesky authentication is completed locally before running this script.
# Do not store the app password in the shared GitHub repository, R code is
# sending request(s) to the Bluesky API through atrrr is working/functioning. 
  
# Install once only if not already installed
# install.packages("atrrr")

library(atrrr)

# ChatGPT
search_skeets_ChatGPT = search_post(
  "ChatGPT",
  sort = "latest",
  since = "2025-10-04",
  limit = 200
)

# Claude AI
search_skeets_Claude = search_post(
  "Claude AI",
  sort = "latest",
  since = "2025-10-04",
  limit = 200
)

# Google Gemini
search_skeets_Gemini = search_post(
  "Google Gemini",
  sort = "latest",
  since = "2025-10-04",
  limit = 200
)


# Check the number of rows and columns
dim(search_skeets_ChatGPT)
dim(search_skeets_Claude)
dim(search_skeets_Gemini)

# Check the available variables
names(search_skeets_ChatGPT)

# Preview the first few records
head(search_skeets_ChatGPT)

# View the dataset
View(search_skeets_ChatGPT)

# Save raw Bluesky datasets
save(
  search_skeets_ChatGPT,
  search_skeets_Claude,
  search_skeets_Gemini,
  file = "data/raw/bluesky_raw_data.RData"
)
