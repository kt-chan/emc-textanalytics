## Generate DTM from source files
source('./scripts/TextAnalytic_P1_DTM.R')



# =========================================================================
# INSPECT FREQUENT WORDS AND PLOT THEM 
# =========================================================================
termFrequency <- colSums(as.matrix(dtm))
print(ggplot(data.frame(term=names(termFrequency), freq=termFrequency), 
             aes(x=reorder(term,freq), y=freq)) + 
        geom_bar(stat="identity") + 
        xlab("Terms") + ylab("tf-idf") + 
        coord_flip())




# WordCloud
## findFreqTerms(dtm[1,], 10)
## freq1 = data.frame(sort(colSums(as.matrix(dtm[1,])), decreasing=TRUE))
## wordcloud(rownames(freq1), freq1[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
## 
## findFreqTerms(dtm[2,], 10)
## freq2 = data.frame(sort(colSums(as.matrix(dtm[2,])), decreasing=TRUE))
## wordcloud(rownames(freq2), freq2[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

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

