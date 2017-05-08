source('./scripts/TextAnalytic_P2_Graph.R')

iter = 1000
thin=1000
burnin = 100
keep = 50

LDA_Gibbs <- list(alpha = 50/k, estimate.beta= TRUE, verbose=1, prefix = tempfile(), save=0,  seed=as.integer(Sys.time()), keep=keep, iter=iter, burnin=burin, thin=thin)
lda_model = LDA(dtm, k, method= "Gibbs", control =LDA_Gibbs)
print(terms(lda_model, 5))
