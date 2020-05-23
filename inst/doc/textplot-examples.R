### R code from vignette source 'textplot-examples.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+   ")
options(prompt = " ", continue = "   ")
set.seed(123456789)


###################################################
### code chunk number 2: textplot-examples.Rnw:47-55
###################################################
library(udpipe)
library(textplot)
library(ggraph)
library(igraph)
x <- udpipe("His speech about marshmallows in New York is utter bullshit",
            "english")
plt <- textplot_dependencyparser(x, size = 4)
plt


###################################################
### code chunk number 3: textplot-examples.Rnw:58-59
###################################################
print(plt)


###################################################
### code chunk number 4: textplot-examples.Rnw:64-68
###################################################
x <- udpipe("UDPipe provides tokenization, tagging, lemmatization and
             dependency parsing of raw text", "english")
plt <- textplot_dependencyparser(x, size = 4)
plt


###################################################
### code chunk number 5: textplot-examples.Rnw:71-72
###################################################
print(plt)


###################################################
### code chunk number 6: textplot-examples.Rnw:79-87
###################################################
library(BTM)
library(ggraph)
library(concaveman)
library(igraph)
data(example_btm, package = 'textplot')
model <- example_btm
plt <- plot(model, title = "BTM model", top_n = 5)
plt


###################################################
### code chunk number 7: textplot-examples.Rnw:90-91
###################################################
print(plt)


###################################################
### code chunk number 8: textplot-examples.Rnw:94-97
###################################################
plt <- plot(model, title = "Biterm topic model", subtitle = "Topics 2 to 8",
            which = 2:8, top_n = 7)
plt


###################################################
### code chunk number 9: textplot-examples.Rnw:100-101
###################################################
print(plt)


###################################################
### code chunk number 10: textplot-examples.Rnw:105-128
###################################################
library(BTM)
library(data.table)
library(udpipe)
library(igraph)
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
## Build the BTM model
set.seed(123456)
x <- subset(anno, upos %in% c("NOUN", "PROPN", "ADJ"))
x <- x[, c("doc_id", "lemma")]
model <- BTM(x, k = 5, beta = 0.01, iter = 2000, background = TRUE,
             biterms = biterms, trace = 100)
plt <- plot(model)
plt


###################################################
### code chunk number 11: textplot-examples.Rnw:131-132
###################################################
print(plt)


###################################################
### code chunk number 12: textplot-examples.Rnw:139-166
###################################################
library(udpipe)
library(data.table)
library(ggraph)
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
                               title = "Objects of verbs and adjectives modifying nouns",
                               subtitle = "Top 50 by group")
plt


###################################################
### code chunk number 13: textplot-examples.Rnw:169-170
###################################################
print(plt)


###################################################
### code chunk number 14: textplot-examples.Rnw:178-187
###################################################
library(udpipe)
data("brussels_reviews_anno", package = "udpipe")
x   <- subset(brussels_reviews_anno, xpos %in% "JJ")
x   <- sort(table(x$lemma))
plt <- textplot_bar(x, top = 20,
                    panel = "Adjectives", xlab = "Frequency",
                    col.panel = "lightblue", cextext = 0.75,
                    addpct = TRUE, cexpct = 0.5)
plt


###################################################
### code chunk number 15: textplot-examples.Rnw:190-191
###################################################
print(plt)


###################################################
### code chunk number 16: textplot-examples.Rnw:199-207
###################################################
library(graph)
library(Rgraphviz)
library(udpipe)
dtm <- subset(anno, upos %in% "ADJ")
dtm <- document_term_frequencies(dtm, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
textplot_correlation_lines(dtm, top_n = 25, threshold = 0.01, lwd = 5, label = TRUE)


###################################################
### code chunk number 17: textplot-examples.Rnw:213-223
###################################################
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


###################################################
### code chunk number 18: textplot-examples.Rnw:230-240
###################################################
library(udpipe)
library(ggraph)
library(igraph)
data(brussels_reviews_anno, package = 'udpipe')
x <- subset(brussels_reviews_anno, xpos %in% "JJ" & language %in% "fr")
x <- cooccurrence(x, group = "doc_id", term = "lemma")

plt <- textplot_cooccurrence(x,
                             title = "Adjective co-occurrences", top_n = 25)
plt


###################################################
### code chunk number 19: textplot-examples.Rnw:243-244
###################################################
print(plt)


###################################################
### code chunk number 20: textplot-examples.Rnw:248-256 (eval = FALSE)
###################################################
## library(udpipe)
## library(ggraph)
## library(igraph)
## library(data.table)
## data("brussels_reviews", package = "udpipe")
## anno <- subset(brussels_reviews, language %in% "nl")
## anno <- data.frame(doc_id = anno$id, text = anno$feedback, stringsAsFactors = FALSE)
## anno <- udpipe(anno, "dutch", trace = 10)


###################################################
### code chunk number 21: textplot-examples.Rnw:259-272
###################################################
biterms <- merge(anno, anno,
            by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
            by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
            all.x = TRUE, all.y = FALSE, suffixes = c("", "_parent"), sort = FALSE)
biterms <- setDT(biterms)
biterms <- subset(biterms, dep_rel %in% c("obj", "amod"))
biterms <- biterms[, list(cooc = .N), by = list(term1 = lemma, term2 = lemma_parent)]

plt <- textplot_cooccurrence(biterms,
                             title = "Objects of verbs and Adjectives modifying nouns", top_n = 75,
                             vertex_color = "orange", edge_color = "black",
                             fontface = "bold")
plt


###################################################
### code chunk number 22: textplot-examples.Rnw:275-276
###################################################
print(plt)


