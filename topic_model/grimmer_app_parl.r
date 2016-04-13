# This script fits a topic model (grimmer.R) to the splitted and cleaned data

rm(list=ls())
library(xtable)
library(tm)   # Not available anymore, make tdms in python

setwd("~/Dropbox/btspeech/")

CB_corpus <- Corpus(DirSource("data/c_splitted"), fileEncoding="UTF-8", 
                    language="de")
CB_corpus <- tm_map(CB_corpus, stripWhitespace)

CB_TDM <- TermDocumentMatrix(CB_corpus)
backup_TDM <- CB_TDM
CB_TDM <- removeSparseTerms(CB_TDM, 0.99)

parl_tdm <- as.matrix(CB_TDM)

# Remove Docs with less than minwords words
minwords <- 100
parl_tdm <- parl_tdm[,-which(colSums(parl_tdm) < minwords)]

# Get author matrix:
docnames <- colnames(parl_tdm)
last <- ''
firsts <- c()
lasts <- c()
i <- 1

for(name in docnames){
  if(grepl('list',name)) check <- gsub('\\(\\d\\d\\)\\(list\\)_\\d\\d\\d\\d\\d_.txt',
                                       '',name)
  if(grepl('direct',name)) check <- gsub('\\(\\d\\d\\)\\(direct\\)_\\d\\d\\d\\d\\d_.txt',
                                         '',name)
  #if(grepl('\\(NA#########\\)',name)) check <- gsub('\\(\\d\\d\\)\\(direct\\)_\\d\\d\\d\\d\\d_.txt','',name)
  if(check != last){
    firsts <- c(firsts, i)
    lasts <- c(lasts, i-1)
  } 
  last <- check
  i <- i + 1
}
lasts <- c(lasts,ncol(parl_tdm))[-1]


authors <- cbind(firsts,lasts)

source('grimmer.r')
kk <- 40
results_grimmer <- exp.agenda.vonmon(term.doc = t(parl_tdm), authors = authors, 
                                     n.cats = kk, verbose = T, kappa = 500)
#rm(list=setdiff(ls(),c('results_grimmer_20','authors','outdir','kk','docnames')))
outdir <- '~/Dropbox/btspeech'

beta_words <- 20 
## Beta Importance	
r <- c()
r$beta <- results_grimmer$mus
source("beta.importance2.r")
sink(file=paste(outdir,"BetaImportance", kk, ".txt", sep=""), type="output")
beta <- beta.importance2(r, beta_words, method="SD", sortby="both", xtable=TRUE)
sink()
indic <- rep(seq(1,beta_words),kk)
beta <- cbind(beta, indic)
topic.w3 <- paste(rownames(beta[beta[,8]==1,])
                  , rownames(beta[beta[,8]==2,])
                  , rownames(beta[beta[,8]==3,])
)


# Indicator (first word)
topic.w1 <- rownames(beta[beta[,8]==1,])



topic.w3 <- paste(
  rownames(beta[beta[,8]==1,])
  , rownames(beta[beta[,8]==2,])
  , rownames(beta[beta[,8]==3,])
  , sep=", ")


# 10 most important words for each topic
topic.w10 <- paste(
  #rownames(beta[beta[,8]==1,]), 
  rownames(beta[beta[,8]==2,])
  , rownames(beta[beta[,8]==3,])
  , rownames(beta[beta[,8]==4,])
  , rownames(beta[beta[,8]==5,])
  , rownames(beta[beta[,8]==6,])
  , rownames(beta[beta[,8]==7,])
  , rownames(beta[beta[,8]==8,])
  , rownames(beta[beta[,8]==9,])
  , rownames(beta[beta[,8]==10,]), sep=", ")



#####  use MB modul instead
beta <- r$beta
colnames(beta) <- topic.w3
d.beta <- dist(t(beta))
hclust.beta <- hclust(d.beta, method="complete")  ## complete linkage, shouldn't really matter what
par(mfrow=c(1,1))
pdf(paste(outdir,'clustering.pdf',sep=''),width=12,height=7)
plot(hclust.beta)
dev.off()


## Topic Table
topics <- as.matrix(topic.w10)  
topic.table <- cbind(topic.w1,topics)
sink(paste(outdir,'topic_table.tex',sep=''))
cat('\\documentclass{article} \n')
cat('\\usepackage{pdflscape} \n')
cat('\\usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} \n')
cat('\\begin{document} \n')
cat('\\landscape \n')
cat('\\footnotesize \n')
xtable(topic.table)  
cat('\n \\end{document} \n')
sink()



## Topic Table small

topics <- as.matrix(topic.w3)  
sink(paste(outdir,'topic_table_small.tex',sep=''))
cat('\\documentclass{article} \n')
cat('\\usepackage{pdflscape} \n')
cat('\\usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} \n')
cat('\\begin{document} \n')
cat('\\landscape \n')
cat('\\footnotesize \n')
xtable(topics)  
cat('\n \\end{document} \n')
sink()

# how topics and documents are related
topicdoc <- results_grimmer$rs
topicdoc <- round(topicdoc,0)
topicdoc <- topicdoc %*% c(1:kk)
rownames(topicdoc) <- docnames


barplot(table(topicdoc))


topic.w1

sink( paste(outdir,'topicdocs.txt',sep=''))
fnames <- rownames(topicdoc)
for(i in 1:nrow(topicdoc)){
  line <- paste(fnames[i],',',topicdoc[i,1],sep='','\n')
  cat(line)
}
sink()

topic.table

rm(list=setdiff(ls(),c('topicdoc','topics','results_grimmer_20-sep')))
save.image('C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/grimmer/output/grimmer_out_20.RData')
