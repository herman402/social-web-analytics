# COMP3020 Social Web Analytics
# Assessment 2 - Group Project
# Group 19
#
# Project:
# Generative AI Discussions on Bluesky
#
# File:
# 06_network_analysis.R
#
# Purpose:
# Construct and analyse a directed Bluesky follow network
# surrounding generative AI discussions.
#
# Lecture / Lab Source:
# Module 7 - Graphs 1
# Module 7 Lab - Constructing a Bluesky Network

# Research Question:
# Which users are the most structurally important within
# ...a Bluesky follow network surrounding generative-AI discussions?

# "In the network of Bluesky users connected through follow relationships, 
# ...which users seem to have the most important positions?"

# Network definition:
# Node = a Bluesky user/account.
# Edge = one user follows another user.
# The graph is directed because following has a direction:
# User A can follow User B without User B following User A.


# atrrr is used to access Bluesky data such as user profiles
# and follow relationships.
library(atrrr)

# igraph is used to build, visualise and analyse the network.
library(igraph)

# Load the raw Bluesky datasets collected earlier.
load("data/raw/bluesky_raw_data.RData")


# Combine the unique authors from all three AI topics.
# unique() removes users who appear more than once.
all_authors = unique(c(
  search_skeets_ChatGPT$author_handle,
  search_skeets_Claude$author_handle,
  search_skeets_Gemini$author_handle
))

# Count the number of unique authors in the collected AI posts.
length(all_authors)

# Hence, there are 549 unique authors in our combined dataset sample.

# Preview the first few authors.
head(all_authors)

# Retrieve profile information for every unique Bluesky author
# found in our ChatGPT, Claude and Gemini post data-sets.

# all_authors contains 549 unique Bluesky handles.
# get_user_info() looks up each handle and returns profile information
# such as:
# - actor_handle
# - actor_name
# - followers_count
# - follows_count
author_info = get_user_info(all_authors)
# Check how many author profiles were returned.
nrow(author_info)
author_info


# Sorting the authors by their number of followers.order() creates an ordering based on followers_count.
# decreasing = TRUE means the highest follower count comes first.

# We then select only the columns that are useful for comparing users:
# - actor_handle = Bluesky username
# - actor_name = display name
# - followers_count = how many users follow them
# - follows_count = how many users they follow
# head(..., 5) keeps only the top 5 authors after sorting.
top_candidates = head(
  author_info[
    order(author_info$followers_count, decreasing = TRUE),
    c("actor_handle", "actor_name", "followers_count", "follows_count")
  ],
  5
)
# Display the five most-followed authors.This lets us inspect the possible users that could be used
# as the starting point of the network.
top_candidates



# Selecting the author with the highest follower count.
# which.max() finds the position of the largest value inside the followers_count column.
# We then use that position to retrieve the matching actor_handle.
# This author becomes the seed user, meaning the user we start from when building the follow network.
seed_user_handle = author_info$actor_handle[
  which.max(author_info$followers_count)
]

# Display the selected seed user's Bluesky handle.
seed_user_handle 

# Quick sum so far...
# The profile lookup returned 545 user profiles (may vary) from the 549 unique
# author handles in the dataset. This means that four author profiles
# were not returned by the Bluesky API.

# The Guardian had the highest follower count among the retrieved
# authors, with 792,987 followers.

# Therefore, theguardian.com is selected as the seed user.
# The seed user is the starting point used to construct the
# friends-of-friends follow network.

# Retrieve up to 10 users followed by the seed user.
# get_follows() returns accounts that the seed user follows.
# The $actor_handle part keeps only their Bluesky usernames.



# These users form the first level of connections in our network.
friends_handles = get_follows(seed_user_handle, limit = 10)$actor_handle

# Display the accounts followed by the seed user.
friends_handles

# Check how many first-level users were returned.
length(friends_handles)


# Logical next steps:
# The Guardian --> 10 users The Guardian follows --> For each of those 10 users... 
# --> retrieve up to 20 users they follow --> build the second level of the network 
# (profile data) --> keep only the usernames --> use usernames to build network 


# Retrieve up to 20 accounts followed by each of the 10 first-level users.
# lapply() repeats get_follows() for every handle stored inside friends_handles.
# Each result contains the follow information for one first-level user.
more_friends = lapply(friends_handles, get_follows,limit = 20)

# From each result, keep only the Bluesky usernames.`[[` extracts the actor_handle column from each
# get_follows() result.
# more_friends_handles will therefore contain one list of followed users for each first-level account.
more_friends_handles = lapply(more_friends,`[[`,"actor_handle")

# Check how many second-level users were returned
# for each of the 10 first-level accounts.
sapply(more_friends_handles, length)
# This means, each of the 
# First-level user 1 follows → 20 returned users
# First-level user 2 follows → 20
# ...
# First-level user 7 follows → 3
# ...
# First-level user 9 follows → 9


# Create edges from the seed user to the 10 first-level users.

# Each row represents one directed follow relationship.
# "from" = the user doing the following.
# "to" = the user being followed.

# The seed user is repeated once for every first-level user because 
# ...The Guardian follows all 10 of these accounts.
el_seed = cbind(
  from = rep(seed_user_handle, length(friends_handles)),
  to = friends_handles
)

# Display these first-level relationships.
el_seed


# Now creating the edges from each first-level user to their second-level users:
# Create edges from each first-level user to the users they follow.

# seq_along(friends_handles) gives us the positions 1 to 10.
# lapply() goes through each first-level user one at a time.

# For each user:
# "from" = that first-level user's handle.
# "to" = each account that user follows.
el_friends = lapply(
  seq_along(friends_handles),
  function(i) {cbind(
      from = rep( friends_handles[i], length(more_friends_handles[[i]])),
      to = more_friends_handles[[i]]
    )
  }
)


# Combining the first-level and second-level relationships into one complete edge list.

# do.call(rbind, el_friends) joins all the second-level edge tables together.
# rbind() then adds the seed-user edges. unique() removes any exact duplicate relationships.
el = unique( rbind( el_seed, do.call(rbind, el_friends)))


# Remove self-follows. Ex. userA → userB (kept) / userA → userA (removed)
# A self-follows would mean a user is shown as following themselves.
# We remove these because we only want relationships between different users.
# Remove self-follows, where a user would point to itself.
el <- el[el[, "from"] != el[, "to"], , drop = FALSE]


# Creating the full directed network from the completed edge list.

# graph_from_edgelist() turns the "from" and "to" relationships
# stored in el into an igraph network object.

# directed = TRUE is used because a follow has a direction:
# User A following User B does not mean User B follows User A.
g = graph_from_edgelist(el, directed = TRUE)

# Display the graph object.
g

# Count the number of users/nodes in the network.
vcount(g)

# Count the number of follow relationships/edges in the network.
ecount(g)

# The directed graph contains 177 users (nodes)
# and 182 follow relationships (edges).
# The graph was successfully created from the edge list.
# Each displayed arrow represents the direction of a follow relationship.


# Visualise the full directed follow network (Full Network)

# Each point represents a Bluesky user.
# Each line/arrow represents a follow relationship.

# layout_with_fr() positions connected users closer together
# so the structure of the network can be seen more clearly.
# Plot the full network.

# Colour users based on their role in the network.
# Gold = seed user
# Sky blue = first-level users
# Light green = second-level users
V(g)$color = ifelse(
  V(g)$name == seed_user_handle,
  "gold",
  ifelse(
    V(g)$name %in% friends_handles,
    "skyblue",
    "lightgreen"
  )
)

plot(
  g,
  layout = layout_with_fr(g),
  vertex.size = 7,
  vertex.label.cex = 0.25,
  edge.arrow.size = 0.3,
  main = "Full Bluesky Follow Network"
)

# Add a key explaining the node colours.
legend(
  "topright",
  legend = c(
    "Seed user",
    "First-level user",
    "Second-level user"
  ),
  col = c(
    "gold",
    "skyblue",
    "lightgreen"
  ),
  pch = 19,
  pt.cex = 1.5,
  cex = 0.75,
  y.intersp = 0.9,
  bty = "o"
)


# Visualise the filtered directed follow network (Filtered Network)
# Essentially, hides users who only have one connection and show me the users 
# ...that are more connected within this network.”

# Also for colour based role
V(g)$color = ifelse(
  V(g)$name == seed_user_handle,
  "gold",
  ifelse(
    V(g)$name %in% friends_handles,
    "skyblue",
    "lightgreen"
  )
)

g2 = induced_subgraph(g, V(g)[degree(g) > 1])

plot(
  g2,
  layout = layout_with_kk(g2),
  vertex.size = 15,
  main = "Key Connected Users in the Bluesky Follow Network"
)

# Add a key explaining the node colours.
legend(
  "topright",
  legend = c(
    "Seed user",
    "First-level user",
    "Second-level user"
  ),
  col = c(
    "gold",
    "skyblue",
    "lightgreen"
  ),
  pch = 19,
  pt.cex = 1.5,
  cex = 0.75,
  y.intersp = 0.9,
  bty = "o"
)

# dpcarrington.bsky.social, robynvinter.bsky.social, 
# and severincarrell.bsky.social are more connected than the many second-level 
# users that only appear once in the sampled network.



# NETWORK PROPERTIES ====

# Calculate network density.
# Density measures how many follow relationships exist
# compared with how many relationships could possibly exist.
round(edge_density(g), 4)

# The network density is 0.0058. This indicates a sparse network, because only 
# a small proportion of all possible directed follow relationships exist. 
# This is consistent with the friends-of-friends construction,
# ...where many users are connected through only one or a few links.


# Plot the degree distribution.
# Degree is the number of connections a user has.
# This plot shows how common different numbers of
# connections are across the network.
plot(
  degree_distribution(g),
  type = "h",
  xlab = "Degree",
  ylab = "Proportion",
  main = "Bluesky Degree Distribution"
)

# The degree distribution shows that most users in the network have only a small 
# number of connections. A much smaller number of users have higher degree values,
# ...meaning that they are more connected within the sampled network.
 
# This reflects the friends-of-friends construction, where many
# second-level users appear only once, while the seed and first-level
# users have multiple follow relationships.



# CENTRALITY MEASURES ====

# Degree centrality: 
# Degree = "who has the most direct connections?"
# Degree measures how many connections each user has in the network.
# Higher degree means the user has more direct connections.

# Sort users from highest degree to lowest degree, displaying the top 5.
head(sort(degree(g), decreasing = TRUE),5)
# Degree centrality findings:
# bibivanderzee.bsky.social had the highest degree (22),
# meaning it had the most direct connections in the sampled network.


# Closeness centrality:
# Closeness = "who is positioned closest to other users through short paths?"
# Closeness measures how close a user is to other users based on the shortest
# paths through the network.

# Higher closeness means a user can reach other users through relatively fewer steps.
head(sort(closeness(g, mode = "all"), decreasing = TRUE),5)
# Closeness centrality findings:
# theguardian.com had the highest closeness score,
# meaning it was positioned closest to other users through short paths.

# Betweenness centrality:
# Betweenness = "who acts as a bridge between different parts of the network?"
# Betweenness measures how often a user lies on the shortest paths between other 
# users in the network.

# A higher betweenness score means the user may act as a bridge between 
# different parts of the network.
head(sort(betweenness(g, directed = TRUE), decreasing = TRUE),5)
# Betweenness centrality findings:
# theguardian.com had the highest betweenness score (323),
# meaning it acted as the strongest bridge between parts of the network.



# NETWORK LIMITATIONS ====

# This network is a sample rather than the complete Bluesky network.
# It was constructed from users appearing in the collected generative-AI posts.

# The most-followed author was selected as the seed user, which influences
# the shape of the network and may also affect centrality results.

# The network was limited to 10 first-level follows and up to 20
# second-level follows for each first-level user.

# Some user profiles or follow information may not have been returned
# by the Bluesky API, so the network may not include every possible connection.

# The filtered graph g2 only keeps users with degree > 1 for clearer
# visualisation. Centrality measures were calculated on the full graph g.

