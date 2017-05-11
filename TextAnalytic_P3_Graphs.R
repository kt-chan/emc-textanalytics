## Generate DTM from source files
source('/home/admin/Test/scripts/TextAnalytic_P2_LDA.R')

k=10
LDA_Gibbs <- list(alpha = alpha, verbose=0, prefix = tempfile(), save=0,  seed=as.integer(Sys.time()), keep=keep, iter=iter, burnin=burnin, thin=thin)
lda_model = LDA(dtm, k, method= "Gibbs", control =LDA_Gibbs)

p <- posterior(lda_model)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>% mutate(word = rownames(w1)) %>% gather(topic, weight, -word) 

# =====================Presentation of results===============

# =========================================================================
# Wordcloud
# =========================================================================

# approach to drawing word clouds of all topics from an object created with LDA,
n <- 100
palette = "Greens"

## set temporary directory
## wd <- setwd(tempdir())
i<-1
j<-lda_model@k
if(j >5) {j=5; i=round(lda_model@k/5)}
par(mfrow=c(i,j), bg = "grey95", oma=c(0,0,2,0))

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
# print out the first 10th terms ## print(terms(lda_model, 10)) ##
# =========================================================================

par(mfrow=c(1,1))

d1 <- w2[order(-w2$weight), ]
d2 <- by(d1, d1["topic"], head, n=10)
d2 <- Reduce(rbind, d2)
print(ggplot(data = d2, aes(y = word, x = topic)) +geom_tile(aes(fill = weight)))

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
## 
