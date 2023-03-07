options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)

textdata <- read.csv("./data/paragraphs.csv", sep = ",", encoding = "UTF-8")
corpus <- corpus(textdata$text, docnames = textdata$doc_id)
lemma_data <- read.csv("./data/baseform_en.tsv", encoding = "UTF-8")
stopwords_extended <- readLines("./data/stopwords.txt", encoding = "UTF-8")

corpus_tokens <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 6)
corpus_tokens <- tokens_compound(corpus_tokens, collocations)

# Create DTM, but remove terms which occur in less than 1% of all documents
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 5)

top_terms <- c("great", "man", "make", "hear", "find", "em", "leave",
                 "light", "mr", "ye", "akeley", "willett", "year", "side", "aout", "whateley",
                 "john", "chapter", "give", "long", "open",  "back", "git", "malone", "part",
                 "ammi", "ward", "nahum", "fer", "danforth", "ermengarde",
                 "kind", "faint", "hideous", "singular")

DTM <- DTM[, !(colnames(DTM) %in% top_terms)]

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

require(topicmodels)

# number of topics
K <- 15

# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 100000,
  seed = 1,
  verbose = 10,
  alpha = 0.01))

# format of the resulting object
attributes(tmResult)
ncol(DTM)

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms
dim(beta)
rowSums(beta)
nrow(DTM)

theta <- tmResult$topics
dim(theta)
rowSums(theta)[1:10]
terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse = " ")

library(ggplot2)
library(reshape2)
require(pals)

textdata$work <- textdata$text_id
topic_proportion_per_work <- aggregate(theta, by = list(work = textdata$work), mean)
colnames(topic_proportion_per_work)[2:(K+1)] <- topicNames
vizDataFrame <- melt(topic_proportion_per_work, id.vars = "work")

# plot topics per work
ggplot(vizDataFrame, aes(x=work, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "work") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# LDAvis visualisation
library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)