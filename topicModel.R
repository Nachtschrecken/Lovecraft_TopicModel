# This file executes the topic modeling on the provided csv and language options

options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)

#--------------------------------------------------------------------------------------------------------------------------------
# 00 - Prepare the Data

textdata <- read.csv("./data/paragraphs.csv", sep = ",", encoding = "UTF-8")

corpus <- corpus(textdata$text, docnames = textdata$doc_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("./data/baseform_en.tsv", encoding = "UTF-8")

# extended stopword list
stopwords_extended <- readLines("./data/stopwords_en.txt", encoding = "UTF-8")

# Create a DTM (may take a while)
corpus_tokens <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 25)
collocations <- collocations[1:250, ]
corpus_tokens <- tokens_compound(corpus_tokens, collocations)

#--------------------------------------------------------------------------------------------------------------------------------
# 01 - Model Calculation

# Create DTM, but remove terms which occur in less than 1% of all documents
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

# have a look at the number of documents and terms in the
# matrix

dim(DTM)

top10_terms <- c("unite_state", "past_year", "year_ago", "year_end",
                 "government", "state", "country", "year", "make", "seek")

DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

# due to vocabulary pruning, we have empty rows in our DTM 
# LDA does not like this. So we remove those docs from the 
# DTM and the metadata

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

# load package topicmodels
require(topicmodels)

# number of topics
K <- 20

# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

# format of the resulting object
attributes(tmResult)

ncol(DTM) # lengthOfVocab

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms # get beta from results
dim(beta) # K distributions over ncol(DTM) terms

rowSums(beta) # rows in beta sum to 1

nrow(DTM) # size of collection

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics
dim(theta) # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10] # rows in theta sum to 1

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

#--------------------------------------------------------------------------------------------------------------------------------
# 02 - Visualization

# LDAvis browser
library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

#--------------------------------------------------------------------------------------------------------------------------------
# 03 - Filtering Documents

# you can set this manually ...
topicToFilter <- 6

# ... or have it selected by a term in the topic name
topicToFilter <- grep("mexico ", topicNames)[1]

# minimum share of content must be attributed to the

# selected topic
topicThreshold <- 0.1
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- sotu_corpus %>%
  corpus_subset(subset = selectedDocumentIndexes)

# show length of filtered corpus
filteredCorpus

#--------------------------------------------------------------------------------------------------------------------------------
# 04 - Topic Proportions over time

# append decade information for aggregation
textdata$decade <- paste0(substr(textdata$date, 0, 3), "0")

# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = textdata$decade), mean)

# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# plot topic proportions per deacde as bar plot
library(ggplot2)
library(reshape2)

require(pals)
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

require(wordcloud2)

# visualize topics as word cloud
topicToViz <- 11 # change for your own topic of interest

# Or select a topic by a term contained in its name
topicToViz <- grep("mexico", topicNames)[1]

# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)

# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:40]

# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

