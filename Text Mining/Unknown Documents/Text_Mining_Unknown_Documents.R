library(tidyverse)
library(ggwordcloud)
library(tidytext)
library(tm)
library(topicmodels)
library(GGally)
library(network)
library(sna)
library(RColorBrewer)
library(factoextra)

# Set seed
set.seed(1)

#getwd()

# Create Functions to be used in code
toSpace <- content_transformer(function(x,pattern) gsub(pattern," ", x))
tokenizer <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
cosineSim <- function(x) as.dist(x%*%t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))

# Need to exclude additional words from those in stopwords list
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot","man","say","well")

# Load corpus - encoding UTF-8
docs <- VCorpus(DirSource("./docs", encoding = "UTF-8"))

# There are 41 documents in total
print(docs)

# The first file is a plain text document - so are the other 40
class(docs[[1]])

# Show metadata and content of the first file - seems to be about risk management
docs[[1]]$meta
docs[[1]]$content

######################
## Format Documents ##
######################
# Remove punctuation - ucp specifies whether to use unicode character properties for determining punctuation
# Need both as some punctuation not removed (like "<") when ucp = TRUE
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removePunctuation, ucp = TRUE)

# Transform characters to lower case
docs <- tm_map(docs, content_transformer(tolower))

# As first document seems to be about risk management we'll remove digits
docs <- tm_map(docs, removeNumbers)

# Remove standard stopwords and from bespoke list
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, myStopwords)

# Stem document to remove plurals and to bring similar words to a single base
docs <- tm_map(docs,stemDocument)

# Strip whitespace
docs <- tm_map(docs, stripWhitespace)

# Review output
writeLines(as.character(docs[[1]]))

#################
## Word Counts ##
#################
# Create document-term matrix
dtm <- DocumentTermMatrix(docs)

# Summary of Document Term Matrix
dtm

# Collapse matrix by summing over columns - returns counts for each word over all documents.
freq <- colSums(as.matrix(dtm))

# Length  returns the total number of distinct words used in the documents - 4408
length(freq)

# Order words in descending based on frequency
ord <- order(freq, decreasing = TRUE)

# View most frequently occurring terms
freq[head(ord)]

# View least frequently occurring terms
freq[tail(ord)]

# List most frequent terms - lowfreq filters terms with a total count of that value or higher
findFreqTerms(dtm, lowfreq = 100)

# Add terms and counts to a data frame 
df = data.frame(term = names(freq), occurrences = freq)

# Create histogram of most commonly used terms - ordered by frequency
bplot <- ggplot(subset(df, occurrences > 200), aes(reorder(term,occurrences), occurrences)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bplot

# Create wordcloud of most common words in the documents
bcloud <- df %>% 
  arrange(desc(occurrences)) %>% 
  mutate(angle = 90*(runif(nrow(df))>.6)) %>% 
  slice(1:50) %>% 
  ggplot(aes(label = term, size = occurrences, colour = occurrences, angle = angle)) +
  geom_text_wordcloud() +
  scale_size(range = c(4, 16)) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  scale_colour_gradient(low = "#0b77e3", high = "#ff6b0f") +
  theme(panel.background = element_blank())
bcloud

############
## Ngrams ##
############
# Find word sequences using ngrams - Do word sequences provide more insights
# create new Document Term Matrix for word sequencing
ngram_dtm <- DocumentTermMatrix(docs, control = list(tokenize = tokenizer))

# Create matrix and sum sequence frequencies
ngram_freq <- colSums(as.matrix(ngram_dtm))

# Length gives the total number of word sequences - 33134
length(ngram_freq)

# Order matrix by frequency
ngram_ord <- order(ngram_freq, decreasing = TRUE)

# View most frequently occurring word sequences
ngram_freq[head(ngram_ord)]

# Create data frame for word sequences
df_ngram = data.frame(term = names(ngram_freq), occurrences = ngram_freq)

ncloud <- df_ngram %>% 
  arrange(desc(occurrences)) %>% 
  mutate(angle = 90*(runif(nrow(df_ngram))>.6)) %>% 
  slice(1:20) %>% 
  ggplot(aes(label = term, size = occurrences, colour = occurrences, angle = angle)) +
  geom_text_wordcloud() +
  scale_size(range = c(4, 16)) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  scale_colour_gradient(low = "#0b77e3", high = "#ff6b0f") +
  theme(panel.background = element_blank())
ncloud

############
## TD_IDF ##
############
# TD-IDF is a weighting method - terms that occur less often and more likely to be describe a document
# Create new dtm
td_dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))

# Add terms and sum weightings to matrix
td_tot <- colSums(as.matrix(td_dtm))

# Length gives the total number of terms - 4408
length(td_tot)

# Order the matrix with summed weightings
td_ord <- order(td_tot, decreasing = TRUE)

# Inspect the most frequent terms
td_tot[head(td_ord)]

# Create a data frame with terms and the summed weights
df_tdidf = data.frame(term = names(td_tot), weights = td_tot)

# Create a wordcloud of important words based on weighting - seems elephants are important
tplot <- df_tdidf %>% 
  arrange(desc(weights)) %>% 
  mutate(angle = 90*(runif(nrow(df_tdidf))>.6)) %>% 
  slice(1:50) %>% 
  ggplot(aes(label = term, size = weights, colour = weights, angle = angle)) +
  geom_text_wordcloud() +
  scale_size(range = c(4, 16)) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  scale_colour_gradient(low = "#0b77e3", high = "#ff6b0f") +
  theme(panel.background = element_blank())
tplot

#############################
## Heirarchical Clustering ##
#############################
# Create dtm for heirarchical clustering
h_dtm <- as.matrix(dtm)

# compute distance between document vectors
h_dist <- dist(h_dtm)

# Run hierarchical clustering method = Ward's
h_groups <- hclust(h_dist, method = "ward.D")

# Plot cluster dendrogram
plot(h_groups, hang = -1, main = "Document Cluster Dendrogram", sub = "", xlab = "Documents")

# Cut into subtrees - 4 categories seems to give a good result
rect.hclust(h_groups,4)

# Determine optimal number of clusters to use
h_clust <- fviz_nbclust(h_dtm, FUN = hcut, method = "wss") +
  ggtitle("Optimal Number of Clusters") +
  xlab(" Number of Clusters") +
  theme(plot.title = element_text(hjust = 0.5))

h_clust

#####################
## Topic Modelling ##
#####################
# Group documents and determine "topics" for each group

# Variables to set
burnin <- 1000 # Burn-in period used to ensure that we start from a representative point
iter <- 2000 # Sets the number of iterations
thin <- 500 # take every x one for further use - thinning ensures samples are not corrrelated
nstart <- 5 # 5 different random starting points
seed <- list(1,77,2012,9999,910000) # Set seed for random nstart
best <- TRUE # take the best result
k <- 4 # Number of topics to return

## LDA on ngrams using Gibbs method
# Create Document Term Matrix
nlda_dtm <- DocumentTermMatrix(docs, control = list(tokenize = tokenizer))

# LDA - Latent Dirichlet Allocation using Gibbs method
nlda <- LDA(nlda_dtm
            ,k
            ,method = "Gibbs"
            ,control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin)
)

# Show which documents are in which group
topics(nlda)
nlda_topics <- as.matrix(topics(nlda))

# Show top 10 terms by Group
terms(nlda,10)
nlda_terms <- as.matrix(terms(nlda,10))

# Create chart showing most common topics
nlda_tidy <- tidy(nlda)
top_topics <- nlda_tidy %>% 
  group_by(topic) %>% 
  top_n(8, beta) %>% 
  ungroup %>% 
  arrange(topic, -beta)

nplot <- top_topics %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 13)
        ,panel.border = element_rect(fill = NA, colour = "black")
        ,panel.background = element_blank()
        ,panel.grid.major = element_line(colour = "light grey")
        )
nplot

###################
## Network Graph ##
###################
# Create network graph matrix
ng_matix <- as.matrix(dtm)

# Map filenames to matrix row numbers - numbers will be used to reference files in the network graph
doc_key <- cbind(1:length(docs),rownames(ng_matix))
rownames(ng_matix) <- 1:length(docs)

# compute cosine similarity between document vectors
ng_cosine <- cosineSim(ng_matix)

# Adjacency matrix: set entries below threshold to 0.
ng_cosine[ng_cosine < max(ng_cosine)/2] <- 0
ng_cosine <- round(ng_cosine,3)

# Create network graph from the ng_cosine matrix - directed = FALSE and similarity is bi-directional
net <- network(as.matrix(ng_cosine), directed = FALSE)

# Create dataframe and add group column from topic modelling
x = data.frame(docs = network.vertex.names(net))
x = cbind(x,nlda_topics)

# Add groups from topic modelling to network graph
net %v% "topic group" = as.character(x$nlda_topics)

# Set seed to keep graph in same format
set.seed(10)

# Using ggnet2 create network graph and overlay topic modelling groups
ngraph <- ggnet2(net
       ,color = "topic group"
       ,palette = "Set1"
       ,alpha = 0.8
       ,size = 6
       ,edge.alpha = 0.5
       ) +
  theme(plot.title = element_text(hjust = 0.5)
        ,legend.text = element_text(size = 12)
        ,legend.title = element_text(size = 14))
ngraph

