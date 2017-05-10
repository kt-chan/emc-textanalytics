## Pre-requsit: 
## yum groupinstall "Development tools" -y
## yum -y install dejavu-sans-fonts dejavu-serif-fonts

# Clean the enviroment
rm(list=ls())

list.of.packages_base <- c("tm", "SnowballC", "R.utils")
list.of.packages_extend <- c("doParallel", "tidyverse", "corrplot", 'wordcloud', 'topicmodels', "devtools", "curl", "httr", "dplyr", "RColorBrewer", "ggplot2", "scales")
new.packages <- list.of.packages_extend[!(list.of.packages_extend %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos=c("http://cran.us.r-project.org", "http://R-Forge.R-project.org"))

## install.packages("devtools", repos=c("http://cran.us.r-project.org", "http://R-Forge.R-project.org"))
## library(devtools)
## install_github("trinker/qdapDictionaries")
## install_github("trinker/qdapRegex")
## install_github("trinker/qdapTools")
## install_github("trinker/qdap")


## source("http://bioconductor.org/biocLite.R")
## biocLite("Rgraphviz")
## biocLite("graph")
