### R code from vignette source 'textplot-examples.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+   ")
options(prompt = " ", continue = "   ")
set.seed(123456789)


###################################################
### code chunk number 2: textplot-examples.Rnw:47-54
###################################################
library(udpipe)
library(textplot)
library(ggraph)
x <- udpipe("His speech about marshmallows in New York is utter bullshit",
            "english")
plt <- textplot_dependencyparser(x, size = 4)
plt


###################################################
### code chunk number 3: textplot-examples.Rnw:57-58
###################################################
print(plt)


###################################################
### code chunk number 4: textplot-examples.Rnw:63-67
###################################################
x <- udpipe("UDPipe provides tokenization, tagging, lemmatization and
             dependency parsing of raw text", "english")
plt <- textplot_dependencyparser(x, size = 4)
plt


###################################################
### code chunk number 5: textplot-examples.Rnw:70-71
###################################################
print(plt)


###################################################
### code chunk number 6: textplot-examples.Rnw:78-85
###################################################
library(BTM)
library(ggraph)
library(concaveman)
data(example_btm, package = 'textplot')
model <- example_btm
plt <- plot(model, title = "BTM model", top_n = 5)
plt


###################################################
### code chunk number 7: textplot-examples.Rnw:88-89
###################################################
print(plt)


###################################################
### code chunk number 8: textplot-examples.Rnw:92-95
###################################################
plt <- plot(model, title = "Biterm topic model", subtitle = "Topics 2 to 8",
            which = 2:8, top_n = 7)
plt


###################################################
### code chunk number 9: textplot-examples.Rnw:98-99
###################################################
print(plt)


###################################################
### code chunk number 10: textplot-examples.Rnw:103-125
###################################################
library(BTM)
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
## Build the BTM model
set.seed(123456)
x <- subset(anno, upos %in% c("NOUN", "PROPN", "ADJ"))
x <- x[, c("doc_id", "lemma")]
model <- BTM(x, k = 5, beta = 0.01, iter = 2000, background = TRUE,
             biterms = biterms, trace = 100)
plt <- plot(model)
plt


###################################################
### code chunk number 11: textplot-examples.Rnw:128-129
###################################################
print(plt)


###################################################
### code chunk number 12: textplot-examples.Rnw:136-145
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
### code chunk number 13: textplot-examples.Rnw:148-149
###################################################
print(plt)


###################################################
### code chunk number 14: textplot-examples.Rnw:157-165
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
### code chunk number 15: textplot-examples.Rnw:171-181
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
### code chunk number 16: textplot-examples.Rnw:188-197
###################################################
library(udpipe)
library(ggraph)
data(brussels_reviews_anno, package = 'udpipe')
x <- subset(brussels_reviews_anno, xpos %in% "JJ" & language %in% "fr")
x <- cooccurrence(x, group = "doc_id", term = "lemma")

plt <- textplot_cooccurrence(x,
                             title = "Adjective co-occurrences", top_n = 25)
plt


###################################################
### code chunk number 17: textplot-examples.Rnw:200-201
###################################################
print(plt)


