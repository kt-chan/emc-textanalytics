# Clean the enviroment
rm(list=ls())

# Load up R packages including a few we only need later:
library(tm)
library(R.utils)
library(data.table)
library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
## library(qdap) 	# Quantitative discourse analysis of transcripts. 
## library(qdapDictionaries) 	
## library(dplyr) 	# Data preparation and pipes %>%. 
## library(Rgraphviz) 	# Correlation plots


## Customization
# file directory
setwd("/home/admin/Test")
#docs.dir <- file.path(getwd(), "textmining")
docs.dir <- file.path(getwd(), "sample")

## Clear all plots and graphs
if(!is.null(dev.list())) dev.off()
unlink(c("*.png"))

# user define variables
my.stopwords <- c("can","dont","didnt","also","as","just", "im", "the", "one", "will", "related", "perform")
stemming = FALSE

print("ingesting files and performing text pre-processing")

# load files into corpus, get listing of .txt files in directory
files.names <- list.files(docs.dir,pattern="*.csv")

## # Copute the size of the files
## files.sizes <- sapply(X = files.names, FUN = function(file) { file.info(file.path(docs.dir, file))$size/(10^3) } )
## 
## # compute the number of lines
## files.lines <- sapply(X = files.names, FUN = function(file) { countLines(file.path(docs.dir, file)) } )
## 
## # compute the number of chars
## files.chars <- sapply(X = files.names, FUN = function(file) { sum(count.fields(file.path(docs.dir, file), sep=" ")) } )
## 
## # summary statistics
## file.stats <- data.frame(files.sizes, files.lines, files.chars)
## names(file.stats) <- c("size(KB)", "lines", "chars")
## 
## par(mfrow=c(1, 2))
## 
## hist(file.stats$lines, main = paste("Histogram of lines"))
## hist(file.stats$chars, main = paste("Histogram of chars"))
## 
## par(mfrow=c(1, 1))

#create corpus from vector
src <- read.csv(file.path(docs.dir, files.names), header = TRUE, stringsAsFactors=FALSE)[,c("sk", "posted_date", "responsibilities", "requirements", "qualifications", "industry", "Class"), ]
tgt <- rep(0, nrow(src))
tgt[src[src$responsibilities == src$requirements,"sk"]] = 1
src[tgt==1,]$responsibilities = NULL
src<-src[,c("sk", "responsibilities", "requirements", "qualifications")]
## src<-src[,c("sk", "responsibilities")]
docs <- Corpus(DataframeSource(src))

## #start preprocessing

# remove non-ascii characters, and url
docs <- tm_map(docs, function(x) gsub("http[[:alnum:]]*", "", x))
docs <- tm_map(docs, function(x) iconv(x,to="ASCII"))

## Remove punctuations, stopwords, to lower case, remove extra whitespace, and stemming
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, c(stopwords("english"), my.stopwords))

## Transformation to document term matrix
docs <- Corpus(VectorSource(docs))

## Calculate the DTM
dtm <- DocumentTermMatrix(docs, control = list(stemming = stemming, stopwords = TRUE, minWordLength = 3))

## Remove sparse terms which happen at least for 10% of the documents, 
dtm <- removeSparseTerms(dtm, .95)

## Take the most freq at 2 sigma level and remove all docs without words
tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) * log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
dtm <- as.DocumentTermMatrix(dtm[, tfidf >= quantile(tfidf, 0.50)])
dtm <-  dtm[apply(dtm , 1, sum)> 0, ]  



## free some memory
rm(list=setdiff(ls(), c("docs", "dtm")))

## #Word Stemming
## docs_tmp <- tm_map(docs, stemDocument, language =  "english")
## 
## stemCompletion2 <- function(x, dictionary) {
##   x <- unlist(strsplit(as.character(x), " "))
##   # Unexpectedly, stemCompletion completes an empty string to
##   # a word in dictionary. Remove empty string to avoid above issue.
##   x <- x[x != ""]
##   x <- stemCompletion(x, dictionary=dictionary)
##   x <- paste(x, sep="", collapse=" ")
##   PlainTextDocument(stripWhitespace(x))
## }
## 
## docs <- tm_map(docs_tmp, stemCompletion2, dictionary = docs)