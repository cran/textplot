## ----preliminaries, echo=FALSE, results="hide"--------------------------------
options(prompt = "R> ", continue = "+   ")
options(prompt = " ", continue = "   ")
set.seed(123456789)
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.align = "center")
library(textplot)

## ----eval=(require(udpipe, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(igraph, quietly = TRUE)), fig.width=10, fig.height=5----
library(textplot)
library(udpipe)
library(ggraph)
library(ggplot2)
library(igraph)
x <- udpipe("His speech about marshmallows in New York is utter bullshit",
            "english")
plt <- textplot_dependencyparser(x, size = 4)
plt

## ----eval=(require(udpipe, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(igraph, quietly = TRUE)), fig.width=12, fig.height=6, out.width = '1\\textwidth', out.height = '0.5\\textwidth'----
x <- udpipe("UDPipe provides tokenization, tagging, lemmatization and
             dependency parsing of raw text", "english")
plt <- textplot_dependencyparser(x, size = 4)
plt

## ----eval=(require(BTM, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggforce, quietly = TRUE) && require(concaveman, quietly = TRUE) && require(igraph, quietly = TRUE)), fig.width=8, fig.height=6, out.width = '\\textwidth'----
library(BTM)
library(ggplot2)
library(ggraph)
library(ggforce)
library(concaveman)
library(igraph)
data(example_btm, package = 'textplot')
model <- example_btm
plt <- plot(model, title = "BTM model", top_n = 5)
plt

## ----eval=(require(BTM, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggforce, quietly = TRUE) && require(concaveman, quietly = TRUE) && require(igraph, quietly = TRUE)), fig.width=8, fig.height=6----
plt <- plot(model, title = "Biterm topic model", subtitle = "Topics 2 to 8",
            which = 2:8, top_n = 7)
plt

## ----eval=(require(data.table, quietly = TRUE) && require(udpipe, quietly = TRUE)), results="hide", fig.width=8, fig.height=6----
library(data.table)
library(udpipe)
## Annotate text with parts of speech tags
data("brussels_reviews", package = "udpipe")
anno <- subset(brussels_reviews, language %in% "nl")
anno <- data.frame(doc_id = anno$id, text = anno$feedback, stringsAsFactors = FALSE)
anno <- udpipe(anno, "dutch", trace = 10)
## Get cooccurrences of nouns / adjectives and proper nouns
biterms <- as.data.table(anno)
biterms <- biterms[, cooccurrence(x = lemma,
                                  relevant = upos %in% c("NOUN", "PROPN", "ADJ"),
                                  skipgram = 2),
                     by = list(doc_id)]

## ----eval=(require(BTM, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggforce, quietly = TRUE) && require(concaveman, quietly = TRUE) && require(igraph, quietly = TRUE) && require(data.table, quietly = TRUE) && require(udpipe, quietly = TRUE)), results="hide", fig.width=8, fig.height=6----
library(BTM)
library(ggplot2)
library(ggraph)
library(ggforce)
library(concaveman)
library(igraph)
## Build the BTM model
set.seed(123456)
x <- subset(anno, upos %in% c("NOUN", "PROPN", "ADJ"))
x <- x[, c("doc_id", "lemma")]
model <- BTM(x, k = 5, beta = 0.01, iter = 2000, background = TRUE,
             biterms = biterms, trace = 100)
plt <- plot(model)
plt

## ----eval=(require(BTM, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggforce, quietly = TRUE) && require(concaveman, quietly = TRUE) && require(igraph, quietly = TRUE) && require(data.table, quietly = TRUE) && require(udpipe, quietly = TRUE)), fig.width=8, fig.height=8----
library(BTM)
library(ggplot2)
library(ggraph)
library(ggforce)
library(concaveman)
library(igraph)
library(data.table)
library(udpipe)
x <- merge(anno, anno,
            by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
            by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
            all.x = TRUE, all.y = FALSE, suffixes = c("", "_parent"), sort = FALSE)
x <- subset(x, dep_rel %in% c("obj", "amod"))
x$topic <- factor(x$dep_rel)
topiclabels <- levels(x$topic)
x$topic <- as.integer(x$topic)
## Construct biterms/terminology inputs to the plot
biterms <- data.frame(term1 = x$lemma, term2 = x$lemma_parent,
                      topic = x$topic, stringsAsFactors = FALSE)
terminology <- document_term_frequencies(x, document = "topic",
                                         term = c("lemma", "lemma_parent"))
terminology <- document_term_frequencies_statistics(terminology)
terminology <- terminology[order(terminology$tf_idf, decreasing = TRUE), ]
terminology <- terminology[, head(.SD, 50), by = list(topic = doc_id)]
terminology <- data.frame(topic = terminology$topic,
                          token = terminology$term,
                          probability = 1, stringsAsFactors = FALSE)
plt <- textplot_bitermclusters(terminology, biterms,
                               labels = topiclabels,
                               title = "Objects of verbs and adjectives-nouns",
                               subtitle = "Top 50 by group")
plt

## ----eval=(require(udpipe, quietly = TRUE)), fig.width=5.5, fig.height=5.5----
library(udpipe)
data("brussels_reviews_anno", package = "udpipe")
x   <- subset(brussels_reviews_anno, xpos %in% "JJ")
x   <- sort(table(x$lemma))
plt <- textplot_bar(x, top = 20,
                    panel = "Adjectives", xlab = "Frequency",
                    col.panel = "lightblue", cextext = 0.75,
                    addpct = TRUE, cexpct = 0.5)
plt

## ----eval=(require(Rgraphviz, quietly = TRUE) && require(udpipe, quietly = TRUE) && require(data.table, quietly = TRUE) && require(graph, quietly = TRUE)), fig.width=5, fig.height=5----
library(graph)
library(Rgraphviz)
library(udpipe)
dtm <- subset(anno, upos %in% "ADJ")
dtm <- document_term_frequencies(dtm, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
textplot_correlation_lines(dtm, top_n = 25, threshold = 0.01, lwd = 5, label = TRUE)

## ----eval=(require(udpipe, quietly = TRUE) && require(data.table, quietly = TRUE) && require(qgraph, quietly = TRUE) && require(glasso, quietly = TRUE)), fig.width=6, fig.height=6----
library(glasso)
library(qgraph)
library(udpipe)
dtm <- subset(anno, upos %in% "NOUN")
dtm <- document_term_frequencies(dtm, document = "doc_id", term = "token")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 20)
dtm <- dtm_remove_tfidf(dtm, top = 100)
term_correlations <- dtm_cor(dtm)
textplot_correlation_glasso(term_correlations, exclude_zero = TRUE)

## ----eval=(require(udpipe, quietly = TRUE) && require(igraph, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggplot2, quietly = TRUE)), fig.width=6, fig.height=6, out.width = '0.75\\textwidth', out.height = '0.75\\textwidth'----
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
data(brussels_reviews_anno, package = 'udpipe')
x <- subset(brussels_reviews_anno, xpos %in% "JJ" & language %in% "fr")
x <- cooccurrence(x, group = "doc_id", term = "lemma")

plt <- textplot_cooccurrence(x,
                             title = "Adjective co-occurrences", top_n = 25)
plt

## ----eval=(require(udpipe, quietly = TRUE) && require(igraph, quietly = TRUE) && require(ggraph, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(data.table, quietly = TRUE)), fig.width=8, fig.height=6, out.width = '0.8\\textwidth', out.height = '0.6\\textwidth'----
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library(data.table)
biterms <- merge(anno, anno,
            by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
            by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
            all.x = TRUE, all.y = FALSE, suffixes = c("", "_parent"), sort = FALSE)
biterms <- setDT(biterms)
biterms <- subset(biterms, dep_rel %in% c("obj", "amod"))
biterms <- biterms[, list(cooc = .N), by = list(term1 = lemma, term2 = lemma_parent)]
plt <- textplot_cooccurrence(biterms,
                             title = "Objects of verbs + Adjectives-nouns",
                             top_n = 75,
                             vertex_color = "orange", edge_color = "black",
                             fontface = "bold")
plt

## ----eval=(require(uwot, quietly = TRUE) && require(ggplot2, quietly = TRUE) && require(ggrepel, quietly = TRUE) && require(ggalt, quietly = TRUE) ), fig.width=9, fig.height=7, out.width = '0.9\\textwidth', out.height = '0.7\\textwidth'----
library(uwot)
set.seed(1234)

## Put embeddings in lower-dimensional space (2D)
data(example_embedding, package = "textplot")
embed.2d <- umap(example_embedding,
                 n_components = 2, metric = "cosine", n_neighbors = 15,
                 fast_sgd = TRUE, n_threads = 2, verbose = FALSE)
embed.2d <- data.frame(term = rownames(example_embedding),
                       x = embed.2d[, 1], y = embed.2d[, 2],
                       stringsAsFactors = FALSE)
head(embed.2d, n = 5)

## Get a dataset with words assigned to each cluster with a certain probability weight
data(example_embedding_clusters, package = "textplot")
terminology <- merge(example_embedding_clusters, embed.2d, by = "term", sort = FALSE)
terminology <- subset(terminology, rank <= 7 & cluster %in% c(1, 3, 4, 10, 15, 19, 17))
head(terminology, n = 10)

## Plot the relevant embeddings
library(ggplot2)
library(ggrepel)
library(ggalt)
plt <- textplot_embedding_2d(terminology, encircle = TRUE, points = TRUE,
                             title = "Embedding Topic Model clusters",
                             subtitle = "embedded in 2D using UMAP")
plt

