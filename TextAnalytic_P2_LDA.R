## Generate DTM from source files
source('/home/admin/Test/scripts/TextAnalytic_P1_DTM.R')

iter=2000     ## number of Gibbs iterations, by default equals 2000.
thin=2000     ## number of omitted in-between Gibbs iterations, by default equals iter.
burnin=1000   ## number of omitted Gibbs iterations at beginning, by default equals 0.
keep= 50     ## if a positive integer, the log-likelihood is saved every keep iterations.
alpha= 1   ## initial value for alpha.


#----------------N-fold cross-validation, different numbers of topics----------------

print("performing cross-validation on LDA...")

cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, dtm$nrow, replace = TRUE)
candidate_k <- seq(2, dtm$ncol, 2) # candidates for how many topics

clusterExport(cluster, c("dtm", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

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
      
      fitted <- LDA(train_set, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
stopCluster(cluster)

results_df <- as.data.frame(results)

print(
  ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle(paste(folds, "fold cross-validation of topic modelling")) +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
)

## Run with optimized K
print(paste("best k value = ", results_df$k[which.min(results_df$perplexity)])) ## this take the minimum

