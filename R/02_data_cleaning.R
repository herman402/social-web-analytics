# Data Cleaning ====
# Load libraries:
library("tm")
library("SnowballC")
library("jsonlite")

# Load data:
load("data/raw/bluesky_raw_data.RData")

# Text gathering:
chatgpt_text = search_skeets_ChatGPT$text
claude_text = search_skeets_Claude$text
gemini_text = search_skeets_Gemini$text

# Combine all texts into one:
all_texts = c(chatgpt_text, claude_text, gemini_text)

# Make corpus:
corpus = Corpus(VectorSource(all_texts))

# ASCII conversion:
corpus = tm_map(corpus, function(x) iconv(x, to = "ASCII"))

# URL removal:
remove_url = content_transformer(function(x) {
  gsub("(https?://|www\\.)[^[:space:]<>]+", "", x, perl = TRUE)
})

# Data transformation:
names = c("ChatGPT", "Claude", "Gemini")
bad_words = fromJSON("data/words.json")
  
corpus = tm_map(corpus, remove_url)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeWords, tolower(names))
corpus = tm_map(corpus, removeWords, bad_words)
corpus = tm_map(corpus, stemDocument)

# Inspect corpus
inspect(corpus)

# Save processed texts:
save(corpus, file = "data/processed/bluesky_processed_data.RData")
