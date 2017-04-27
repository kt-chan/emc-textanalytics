# Clean the enviroment
rm(list=ls())

# Start
library(tm)                 # Framework for text mining.
library(SnowballC)
library(R.utils)
library(corrplot)
library(wordcloud)
library(ggplot2)

## Customization
# file directory
setwd("/home/admin/Test")
docs.dir <- file.path(getwd(), "textmining")
#docs.dir <- file.path(getwd(), "sample")

# stop words
my.stopwords <- c("can","dont","didnt","also","as","just", "im", "the", "one", "will")

# load files into corpus, get listing of .txt files in directory
files.names <- list.files(docs.dir,pattern="*.txt")

# Copute the size of the files
files.sizes <- sapply(X = files.names, FUN = function(file) { file.info(file.path(docs.dir, file))$size/(10^3) } )

# compute the number of lines
files.lines <- sapply(X = files.names, FUN = function(file) { countLines(file.path(docs.dir, file)) } )

# compute the number of chars
files.chars <- sapply(X = files.names, FUN = function(file) { sum(count.fields(file.path(docs.dir, file), sep=" ")) } )

# summary statistics
file.stats <- data.frame(files.sizes, files.lines, files.chars)
names(file.stats) <- c("size(KB)", "lines", "chars")

par(mfrow=c(1, 2))

hist(file.stats$lines, main = paste("Histogram of lines"))
hist(file.stats$chars, main = paste("Histogram of chars"))

par(mfrow=c(1, 1))

#create corpus from vector
docs.origin <- Corpus(DirSource(docs.dir))
docs <- Corpus(DirSource(docs.dir))

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


#Word Stemming
docs_tmp <- tm_map(docs, stemDocument, language = "english")

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

docs <- tm_map(docs_tmp, stemCompletion2, dictionary = docs)

## Transformation to document term matrix
docs <- Corpus(VectorSource(docs))
dtm <- DocumentTermMatrix(docs, control = list(minWordLength = c(1, Inf), stemming = TRUE, tolower = TRUE, stopwords = TRUE, removeNumbers = TRUE, removePunctuation = TRUE))
dtm <- removeSparseTerms(dtm, .30)

# WordCloud
## findFreqTerms(dtm[1,], 10)
## freq1 = data.frame(sort(colSums(as.matrix(dtm[1,])), decreasing=TRUE))
## wordcloud(rownames(freq1), freq1[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
## 
## findFreqTerms(dtm[2,], 10)
## freq2 = data.frame(sort(colSums(as.matrix(dtm[2,])), decreasing=TRUE))
## wordcloud(rownames(freq2), freq2[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

# =========================================================================
# INSPECT FREQUENT WORDS AND PLOT THEM 
# =========================================================================
termFrequency <- colSums(as.matrix(dtm))
df <- data.frame(term=names(termFrequency), freq=termFrequency)
df$id <- c(1:nrow(df))
ggplot(df, aes(x=reorder(term,-freq), y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()

## Plot associations between words
corrplot(cor(as.matrix(dtm)), method="color")

## Plot terms agains docs
TDM.dense = melt(as.matrix(dtm), value.name = "count")
ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
  geom_tile() +
  xlab("Docs") + ylab("Terms") +
  scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  theme(panel.background = element_blank()) 
# # 
# #inspect a particular document in corpus
# write.csv(as.matrix(dtm), "dtm.csv")
# writeLines(as.character(docs[[1]]))
# writeLines(as.character(docs.origin[[1]]))
# writeLines('vs')
# writeLines(as.character(docs[[1]]))
# writeLines('----------')
# writeLines(as.character(docs.origin[[2]]))
# writeLines('vs')
# writeLines(as.character(docs[[2]]))
