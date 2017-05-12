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

loadData <- function(dir=file.path(getwd()), pattern="*.txt", top) {
  
  print("ingesting file")
  
  # load files into corpus, get listing of .txt files in directory
  files.names <- list.files(path=dir,pattern=pattern)
  
  #create corpus from vector
  src <- read.csv(file.path(dir, files.names), header = TRUE, stringsAsFactors=FALSE)[,c("sk", "posted_date", "responsibilities", "requirements", "qualifications", "industry", "Class"), ]
  tgt <- rep(0, nrow(src))
  tgt[src[src$responsibilities == src$requirements,"sk"]] = 1
  src[tgt==1,]$responsibilities = NULL
  src<-src[,c("sk", "responsibilities", "requirements", "qualifications")]
  if(!missing(top)) src<-src[1:top,]
  docs <- Corpus(DataframeSource(src))

  return(docs) 
}

processText <- function(docs, stopwords = c(stopwords("english"), my.stopwords)){
  
  print("performing text pre-processing")
  
  # remove non-ascii characters, and url
  docs <- tm_map(docs, function(x) gsub("http[[:alnum:]]*", "", x))
  docs <- tm_map(docs, function(x) iconv(x,to="ASCII"))
  
  ## Remove punctuations, stopwords, to lower case, remove extra whitespace, and stemming
  docs <- tm_map(docs, PlainTextDocument)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removeWords, stopwords)
  
  ## Transformation to document term matrix
  docs <- Corpus(VectorSource(docs))
  
  return(docs)
}


processDTM <- function(docs, stemming = TRUE, sparse=0.95, tfidf_limit=0.5){

  print("performing calculation on DTM")

  ## Calculate the DTM
  dtm <- DocumentTermMatrix(docs, control = list(stemming = stemming, stopwords = TRUE, minWordLength = 3))
  
  ## Remove sparse terms which happen at least for 10% of the documents, 
  dtm <- removeSparseTerms(dtm, sparse)
  
  ## Take the most freq at 2 sigma level
  tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) * log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
  dtm <- as.DocumentTermMatrix(dtm[, tfidf >= quantile(tfidf, tfidf_limit)])
  
  ## Remove all docs without words
  dtm <-  dtm[apply(dtm , 1, sum)> 0, ]  

  return(dtm)
}

validateLDA <- function(dtm, alpha=1, folds=5, iter=2000, thin=2000, burnin=1000, keep= 50) {
  
  
  print("performing cross-validation on LDA...")
  
  
  #----------------N-fold cross-validation, different numbers of topics----------------

  cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
  registerDoParallel(cluster)
  
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  splitfolds <- sample(1:folds, dtm$nrow, replace = TRUE)
  candidate_k <- seq(2, dtm$ncol, 2) # candidates for how many topics
  
  clusterExport(cluster, c("dtm", "alpha", "burnin", "iter", "keep", "thin","splitfolds", "folds", "candidate_k"), envir=environment())
  
  # we parallelize by the different number of topics.  A processor is allocated a value
  # of k, and does the cross-validation serially.  This is because it is assumed there
  # are more candidate values of k than there are cross-validation folds, hence it
  # will be more efficient to parallelise
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- dtm[splitfolds != i , ]
        valid_set <- dtm[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs", control = list(alpha = alpha, prefix = tempfile(), keep=keep, iter=iter, burnin=burnin, thin=thin))
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  
  stopCluster(cluster)
  
  results_df <- as.data.frame(results)
  opt <- results_df$k[which.min(results_df$perplexity)]
  print(
    ggplot(results_df, aes(x = k, y = perplexity)) +
      geom_point() +
      geom_smooth(se = FALSE) +
      ggtitle(paste(folds, "fold cross-validation of topic modelling",  "\n", "Optimum number for K=",  opt )) +
      labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
  )
  
  ## return optimized K
  return(opt)
}



printGraphs <- function(dtm, k, method= "Gibbs", alpha=1, iter=2000, thin=2000, burnin=1000, keep= 50) {
  
  ## Clear all plots and graphs
  if(!is.null(dev.list())) dev.off()
  unlink(c("*.png"))
  
  lda_model = LDA(dtm, k, method = method, control = list(alpha = alpha, prefix = tempfile(), keep=keep, iter=iter, burnin=burnin, thin=thin))
  
  p <- posterior(lda_model)
  w1 <- as.data.frame(t(p$terms)) 
  w2 <- w1 %>% mutate(word = rownames(w1)) %>% gather(topic, weight, -word) 
  
  print("Visualizing the topic model in graphs ... ")
  
  # =========================================================================
  # Wordcloud
  # =========================================================================
  
  # approach to drawing word clouds of all topics from an object created with LDA,
  n <- 100
  box <- 3
  palette = "Greens"
  
  ## set temporary directory
  ## wd <- setwd(tempdir())
  i<-1
  j<-k
  if(j >box) {j=min(box, j); i=ceiling(lda_model@k/box)}
  par(mfrow=c(i,j), bg = "grey95", oma=c(1,1,7,1))
  
  # create a PNG for each frame of the animation
  for(i in 1:ncol(w1)){
    ## png(paste0(i + 1000, ".png"), 8 * 100, 8 * 100, res = 100)
    w3 <- w2 %>% filter(topic == i) %>% arrange(desc(weight))
    
    if(n > nrow(w3)) 
      n <- nrow(w3)
    
    w3 <- w3[1:n,]
    
    wordcloud(w3$word, freq = w3$weight, random.order = FALSE, ordered.colors = TRUE, colors = rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1])
  }
  title("The Key Topics", outer=TRUE)
  
  # =========================================================================
  # heatmap
  # =========================================================================
  
  par(mfrow=c(1,1))
  
  d1 <- w2[order(-w2$weight), ]
  d2 <- by(d1, d1["topic"], head, n=10)
  d2 <- Reduce(rbind, d2)
  print(ggplot(data = d2, aes(y = word, x = topic)) +geom_tile(aes(fill = weight)) + scale_fill_gradient(low = "white", high = "steelblue"))
  
  # =========================================================================
  # INSPECT FREQUENT WORDS AND PLOT THEM 
  # =========================================================================
  ## par(mfrow=c(1,1))
  ##     
  ## termFrequency <- colSums(as.matrix(dtm))
  ## dataframe <- data.frame(term=names(termFrequency), freq=termFrequency)
  ## dataframe <- dataframe[order(-dataframe[,2]), ]
  ## dataframe <- dataframe[1:50,]
  ## print(ggplot(dataframe, 
  ##              aes(x=reorder(term,freq), y=freq)) + 
  ##         geom_bar(stat="identity") + 
  ##         xlab("Terms") + ylab("tf") + 
  ##         coord_flip())
  
}


getDTM <- function(stemming = FALSE, alpha =1, folds = 3, top){
  
  ## Clear all plots and graphs
  if(!is.null(dev.list())) dev.off()
  unlink(c("*.png"))
  
  # file directory
  setwd("/home/admin/Test")

  # user define variables
  stopwords <- c("can","dont","didnt","also","as","just", "im", "the", "one", "will")
  
  
  docs <- loadData(file.path(getwd(), "sample"), "*.csv", top)
  docs <- processText(docs, stopwords)
  dtm <- processDTM(docs, stemming, .90, .50)
  
  return(dtm)
  
}

dtm <- getDTM()
print(dtm)
k <- validateLDA(dtm, alpha = 1, folds = 3)
## printGraphs(dtm, alpha=1, k=6) 

## free some memory
## rm(list=setdiff(ls(), c("docs", "dtm")))



