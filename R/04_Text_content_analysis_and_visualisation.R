#COMP 3020 Social Web Analytics
# Group 19
## 2.3 Text/Content Analysis and Visualisation

##Loading libraries
library("tm")
library("SnowballC")
library("wordcloud")

load("~/tttt/social-web-analytics/data/processed/bluesky_processed_data.RData")
corpus.text = sapply(corpus,as.character) # Convert documents into text

# Checking for other characters
sum(corpus.text == "NA") # Checking how many are "NA"
NAcorpus = which(corpus.text == "NA")
length(NAcorpus)
newcorpus = corpus[-NAcorpus] # New corpus without NA

# Starting the creation of visualisations
tdm = TermDocumentMatrix(newcorpus) 
M = as.matrix(tdm)
word.frequency = rowSums(M)

# Doing wordcloud (First visualisation)
word.frequency = sort(word.frequency, decreasing = TRUE)
word.frequency[1:20]
# Alot of the terms in the top 20 talk about ai with openai and anthropic
# Words suggest that they discuss the capability of ai and the use of AI 
# related tools.
# Words such as "bill" and "percent" might also suggest discussions 
# of statistics, costs and technology.

wordcloud(
  words = names(word.frequency),
  freq = word.frequency,
  min.freq = 6,
  max.words = 100,
  random.order = FALSE,
    colors=brewer.pal(8, "Set2"),
  scale =c(2, .5)
)
title("Most Frequent Words (Word Cloud)")

# Interpretation:
# The larger words seen in the word cloud present the most frequent words 
# from the posts. From the visualisation, we see noticeable words like openai,
# anthropic, model, code.
# This mostly suggests that alot of the discussion of ai relates to mostly 
# the companies. Furthermore, it seems that alot of the orange and green words
# relate to interacting with the ai and the various ways that ai is used such as
# "prompt", "check", "report", "help".

# Second visualisation (TF-IDF Weighted Word Cloud)======================
# Applying TF-IDF weighting to tdm
M2 = as.matrix(weightTfIdf(tdm)) 
weighted.frequency = rowSums(M2) 

# Remove any missing weights
weighted.frequency = weighted.frequency[!is.na(weighted.frequency)] 

# Sorting from highest to lowest
weighted.frequency = sort(weighted.frequency, decreasing = TRUE) 

# Creating TF-IDF weighted word cloud
wordcloud(
  words = names(weighted.frequency),
  freq = weighted.frequency,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(1.5, .7)
)
# Title
title("TF-IDF Weighted Word Cloud")

# Interpretation:
#The weighted word cloud shows a different amount of words at the top.
# At the top, its relatively similar to the unweighted word cloud with words 
# such as "google" and "use" being at the top, suggesting that those terms are 
# often repeated and important parts of AI discussion on the site.
# Other changes are that some words like "bill", "discount", and "source" 
# is more prominent. While other general words like "can", "ask" and "work"
# are less prominent compared to the unweighted word cloud.
# However, a large difference seen in this visualisation is the large words 
# that appear to be website names or URLS like "appealnewsaacblbqqui" and 
# opensubstackcompubthebulwa".


# Third Visualisation MDS
# This is if the second visualisation (Weighted Word Cloud) dosen't count as one
# Apply TF-IDF weighting 
posts.matrix = t(as.matrix(weightTfIdf(tdm)))

# Find posts that contain no useful weighted terms
empties = which(rowSums(abs(posts.matrix)) == 0)
posts.matrix = posts.matrix[-empties, ]

# Normalise each post vector
norm.posts.matrix = diag(
  1/sqrt(rowSums(posts.matrix^2))
) %*% posts.matrix

# Calculating cosine distance
D = dist(norm.posts.matrix, method = "euclidean")^2/2

# Creating 2D MDS projection  
mds.posts.matrix = cmdscale(D, k = 2)

# Plotting the MDS visualisation
plot(
  mds.posts.matrix,
  pch = 16,
  cex = 0.7,
  xlab = "MDS 1",
  ylab = "MDS 2",
  main = "MDS of Bluesky Post Content"
)

# Interpretation:
# The MDS visualisation dosen't show much variety, and shows that are of 
# points on the dot are close together below 0.2 on both y and x axis.
# There are few outliers above this range, suggesting that those posts are more
# distinct than the rest.
# This also can suggest that the posts clustered together are similar in word
# use, fitting in with the discussion of AI topics and how its used.

# Overall findings and contribution to our investigations:
# These findings help explain whats being discussed about AI on Bluesky,
# where alot of discussions revolves around talking about the models and the 
# practical uses of AI such as assisting users with statistics and the economy.
# From the visualisations, it shows that alot of words relating to practical 
# usage of ai is a dominant point of discussion, and that alot of the posts use
# overlapping vocabulary, with only few outliers.
