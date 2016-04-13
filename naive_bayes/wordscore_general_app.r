stop('')
rm(list=ls())


setwd("C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/Wordscore")
indir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/TDMs/'

#load('C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/TDMs/TDM_parl_by_topic.RData')

load('C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/TDMs/TDM_parl_by_topic_nouns.RData')


setwd("C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/Wordscore")
source('C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/getTopic.R')
source('wordscore_general.r')
source('make_dataset_from_ws.r')

references = data.frame('X1'=c('LINKE')
                       ,'X2'=c('FDP'))

# Estimate Position for metatopics

topics = c(8,11,13,14,20,38) 
tdm = getTopic(parl_tdm,topics)
results = wordscore(input=tdm,references)
wordscores_alleco = results$wordscores
data_alleco = makeDataset(results)

topics = c(1:40) 
tdm = getTopic(parl_tdm,topics)
results = wordscore(input=tdm,references)
wordscores_all = results$wordscores
data_all = makeDataset(results)


rm('parl_tdm')
save.image('wordscore_app_40_nouns.RData')
rm(list=ls())
setwd("C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/Wordscore")
load('wordscore_app_40_nouns.RData')



###################
# Topwords Table  #
###################

topwords = matrix(NA,nr=20,nc=6)
colnames(topwords) = c('Left','trans','Score','Right','trans','Score')

# All topics
wordscores_all = wordscores_all[order(wordscores_all[,3],decreasing=T),]
topwords[1:10,4] =  rownames(wordscores_all)[1:10]
topwords[1:10,6] =  round(wordscores_all[1:10,3],3)
wordscores_all = wordscores_all[order(wordscores_all[,3],decreasing=F),]
topwords[1:10,1] =  rownames(wordscores_all)[1:10]
topwords[1:10,3] =  round(wordscores_all[1:10,3],3)

# Socioeconomic
wordscores_alleco = wordscores_alleco[order(wordscores_alleco[,3],decreasing=T),]
topwords[11:20,4] =  rownames(wordscores_alleco)[1:10]
topwords[11:20,6] =  round(wordscores_alleco[1:10,3],3)
wordscores_alleco = wordscores_alleco[order(wordscores_alleco[,3],decreasing=F),]
topwords[11:20,1] =  rownames(wordscores_alleco)[1:10]
topwords[11:20,3] =  round(wordscores_alleco[1:10,3],3)

library(xtable)
xtable(topwords)


pdf('C:/Users/samsung/Dropbox/Ma Thesis/Paper/Final/wordscores.pdf',width=10,height=8)
plot(wordscores_alleco[,3],1:nrow(wordscores_alleco),ylab='Words',xlab='Scores',pch=16,cex=.6)
points(wordscores_all[,3],1:nrow(wordscores_all),pch=16,col='red',cex=.6)
legend('topleft',lwd=c(2,2),lty=c(1,1),col=c('red','black'),c('General','Socioeconomic'))
dev.off()

length(which(wordscores_alleco==1|wordscores_alleco==-1)) #912

###################
# Exclude high SE #
###################
nrow(data_alleco)
data_all = data_all[data_all$error < 1,] # Lost 212 cases
data_alleco = data_alleco[data_alleco$error < 1,] # Lost 87 cases



###################
# Position Plots  #
###################
posplot = function(data,term){
  d = data[data$term==term,]
  d = d[order(d[,5]),]
  r = range(d$position)
  N = nrow(d)
  plot(seq(1:N),seq(r[1],r[2],length.out=N)
       ,type='n',xlab='Members',ylab='Position',cex.lab=2.2,cex.axis=1.8)
  cols = c('purple','darkgreen','red','black','yellow2')
  a = 0
  i=2
  for(i in 1:5){
    if(length(which(d$party==i))==0) next
    da = d[d$party==i,]
    z = a+nrow(da)-1
    points(c(a:z),da$position,col=cols[i],cex=.8,pch=i)  
    m = mean(da$position,na.rm=TRUE)
    points(mean(c(a:z)),m,cex=7,pch='-',col=cols[i])
    a = z
  }
 legend('bottomright',pch=c(1:5),col=cols,c('Die Linke','Die Gruenen','SPD','Union','FDP'),cex=1.4)
}
  

pdf('C:/Users/samsung/Dropbox/Ma Thesis/Paper/Final/scores_all.pdf',width=15,height=6)
data = data_all
par(mfrow = c(1,3), mar=c(5, 4.6, 4, 2))
for(i in 15:17) posplot(data,i)
dev.off()
pdf('C:/Users/samsung/Dropbox/Ma Thesis/Paper/Final/scores_alleco.pdf',width=15,height=6)
par(mfrow = c(1,3), mar=c(5, 4.6, 4, 2))
data = data_alleco
for(i in 15:17) posplot(data,i)
dev.off()






#data = data_all   # N = 1548
data = data_alleco # N = 621

if( length(which(!is.element(data$party,c(1:5))))!=0)  data = data[-which(!is.element(data$party,c(1:5))),]
data = data[,-8]


#summary(lm(position~party+as.factor(term)+party:as.factor(term),data=data))
# 1. 16. and 17. Legislaturperiode significant move to the left (LINKE entered)
# parliament. 2. Significant interaction effects between term16/17 dummy and 
# party --> increasing differenciation between parties.
# Try the same thing again with different anchor texts if consistent 
# could be interesting

# Prepare metadataset

meta =read.csv('c:/users/samsung/Dropbox/Ma Thesis/Empirics/data/parl_metadata/bundeswahlleiter/parl_metadata.csv')
meta$AGE = 2013 - meta$AGE
meta$F_NAME = as.character(meta$F_NAME)
meta$NAME = as.character(meta$NAME)
meta$F_NAME = gsub('Dr.','',meta$F_NAME)
meta$F_NAME = gsub('Prof.','',meta$F_NAME)
meta$F_NAME = gsub('  +','',meta$F_NAME)
meta$F_NAME = gsub('^ ','',meta$F_NAME)
meta$F_NAME = gsub('\\t','',meta$F_NAME)
a = data.frame('party'=meta$PARTY,'num'=0)
a$num[which(a$party=='DIE LINKE' | a$party=='Die Linke.' | a$party=='PDS')] = 1
a$num[which(a$party=='GRÜNE')]                                              = 2
a$num[which(a$party=='SPD')]                                                = 3
a$num[which(a$party=='CDU' | a$party=='CSU')]                               = 4
a$num[which(a$party=='FDP')]                                                = 5
meta$PARTY = a$num

fnames = strsplit(meta$F_NAME,' ')
a = rep(NA,length(fnames))
for(i in 1:length(fnames)) a[i] = fnames[[i]][1]
meta$F_NAME = a
meta$LIST_ELECT_STATE[which(meta$LIST_ELECT_STATE==''| meta$LIST_ELECT_STATE==' ')] = NA
meta$SCND_LIST_STATE[which(meta$SCND_LIST_STATE==''| meta$SCND_LIST_STATE==' ')] = NA

#Conform names
namescomb = data[,6]
splitted = strsplit(namescomb,' ')
names = matrix(NA,nc=2,nr=length(splitted))

for(i in 1:nrow(names)){
  nc = splitted[[i]]
  names[i,1] = nc[length(nc)]
  names[i,2] = nc[1]
}

a = matrix(NA,nc=10,nr=nrow(data))
data = cbind(data[,-6],names,a)

colnames(data)[7:ncol(data)] = c('name','f_name',colnames(meta[3:ncol(meta)]))
data$name = as.character(data$name)
data$f_name = as.character(data$f_name)

data_new = data


# Match Datasets
nm = 0


for(t in 15:17){
  cat(paste('Term:',t,'\n'))
  data_t = data[data$term==t,]
  for(i in 1:nrow(data_t)){
    match = which(meta$NAME == data_t$name[i] & meta$TERM==t) 
    if(length(match)==1){
      match_in_main = which(data$name == data_t$name[i] & data$term==t)
      data_new[match_in_main,9:ncol(data)] = meta[match,3:ncol(meta)]
    }
    else{
      if(length(match)==0){
        cat(paste('No match:',data_t$f_name[i], data_t$name[i]),'\n')
        nm = nm +1
      }
      else{
       
        identified = 0
        for(index in match){
          if(meta$F_NAME[index] == data_t$f_name[i]){
            match_in_main = which(data$name == data_t$name[i] & data$f_name == data_t$f_name[i] & data$term==t)
            data_new[match_in_main,9:ncol(data)] = meta[index,3:ncol(meta)] 
            identified = identified+1
            if(identified>1){
              cat(paste('Multiple matches:',data_t$f_name[i], data_t$name[i]),'\n')
              cat('Not identified, same first name\n')
              nm = nm +1
              break
            }
          }
        }
        if(identified == 0){
          cat(paste('Multiple matches:',data_t$f_name[i], data_t$name[i]),'\n')
          cat('Not identified, first name not found\n')
          nm = nm +1
        }
      }
    }
  }
}
 


nm   
# 55 data_all
# 13 data_alleco

# Distance from the mean

data_new$dist = NA
means15 = rep(NA,5)
means16 = rep(NA,5)
means17 = rep(NA,5)
for(i in 1:5) means15[i] = mean(data_new$position[which(data_new$party==i & data_new$term==15)],na.rm=TRUE) 
for(i in 1:5) means16[i] = mean(data_new$position[which(data_new$party==i & data_new$term==16)],na.rm=TRUE)
for(i in 1:5) means17[i] = mean(data_new$position[which(data_new$party==i & data_new$term==17)],na.rm=TRUE)
for(i in 1:5) data_new$dist[which(data_new$party==i & data_new$term==15)] = abs(data_new$position[which(data_new$party==i & data_new$term==15)] - means15[i])
for(i in 1:5) data_new$dist[which(data_new$party==i & data_new$term==16)] = abs(data_new$position[which(data_new$party==i & data_new$term==16)] - means16[i])
for(i in 1:5) data_new$dist[which(data_new$party==i & data_new$term==17)] = abs(data_new$position[which(data_new$party==i & data_new$term==17)] - means17[i])


# Get data_newset of differences from MPs elected in both terms
data_new = data_new[is.element(data_new$PARTY,c(1:5)),]
data = data_new[order(data_new[,6]),]

deltadata = matrix(0,nc=16,nr=nrow(data)) 
deltadata = as.data.frame(deltadata)
i = 1
c = 1
while(i < nrow(data)){
  both = data[i,7]==data[i+1,7]
  if(both){
    deltadata[c,c(1,2,3)] = data[i,c(5,7,8)]
    deltadata[c,3] = abs(data[i,1]-data[i+1,1])
    deltadata[c,4] = data[i,4]-data[i+1,4]
    deltadata[c,5] = data[i+1,4]
    deltadata[c,6] = data[i+1,3]
    deltadata[c,7:16] = data[i+1,10:18]
    i = i+1
    c = c+1
  }
  else i = i+1
  rownames(deltadata) = NULL
}
deltadata = deltadata[1:c,]
nn = colnames(data_new)
colnames(deltadata) = c('party','name','delta_dist','swichDir','dirT2','T2',nn[10:18])
# codes for delta_mandate: -1 list->direct, 0 no change, 1 direct->list
nrow(deltadata[which(deltadata$swichDir!=0),])

nrow(deltadata)

# data_all 610 70
# data_alleco 182 13

deltadata = deltadata[-nrow(deltadata),]
deltadata$delta_dist = as.numeric(deltadata$delta_dist)

deltadata$double = 0
deltadata$double[which(!is.na(deltadata$SCND_DIST| !is.na(deltadata$SCND_LIST_STATE)))] = 1
deltadata$gov = 0
deltadata$gov[which(deltadata$party==3|deltadata$party==4 & deltadata$T2==16)] = 1
deltadata$gov[which(deltadata$party==5 & deltadata$T2==17)] = 1

deltadata$DIST_ELECT_PERC[is.na(deltadata$DIST_ELECT_PERC)] = 0
deltadata$LIST_ELECT_RANK[is.na(deltadata$LIST_ELECT_RANK)] = 0
deltadata$LIST_ELECT_RANK = -deltadata$LIST_ELECT_RANK

###################
# Regression Model#
###################

library(rjags)

# Limited Covariates

data = cbind(deltadata,0,0,0,0,0)
colnames(data)[19:23] = c('linke','gruen','spd','union','fdp')
for(i in 1:5) data[data$party==i,i+17] = 1


init = list('sd'=1
            #,'betas'=rep(1,4)
            ,'alpha'=c(1,1),'gamma'=c(1,1))
dat = list('y'=(data$delta_dist+0.001),
           'dirT2'=(data$dirT2+1),
           'switchDir'=abs(data$swichDir),
           #'linke'    =data$linke,
           #'gruen'    =data$gruen,
           #'spd'      =data$spd,
           #'union'    =data$union,
           #'b0'=rep(0,4),'B0'=diag(0.1,4),
           'a0'=c(0,0),'A0'=diag(0.1,2),
           'g0'=c(0,0),'G0'=diag(0.1,2),
           'N'=nrow(data)
)

par.tosave = c('sd'
               #,'betas'
               ,'alpha','gamma')
jmod = jags.model(file='gamma_mod_lim.bug',data=dat,inits=init,n.chains=2,n.adapt=5000)
mcmcresults_lim_eco = coda.samples(model=jmod,variable.names=par.tosave,n.iter=50000,thin=1)

summary(mcmcresults_lim_eco)


pdf('C:/Users/samsung/Dropbox/Ma Thesis/Paper/Final/diag_all_lim.pdf')
plot(mcmcresults_lim_eco)
dev.off()

# All covariates

data = cbind(deltadata,0,0,0,0,0)
colnames(data)[19:23] = c('linke','gruen','spd','union','fdp')
for(i in 1:5) data[data$party==i,i+17] = 1


init = list('sd'=1
            ,'betas'=c(1,1,1,1,1,1)
            ,'alpha'=c(1,1),'gamma'=c(1,1))
dat = list('y'=(data$delta_dist+0.001),
           'dirT2'=(data$dirT2+1),
           'switchDir'=abs(data$swichDir),
           'linke'    =data$linke,
           'gruen'    =data$gruen,
           'spd'      =data$spd,
           'union'    =data$union,
           'double'   =data$double,
           'gov'      =data$gov,
           'b0'=c(0,0,0,0,0,0),'B0'=diag(0.1,6),
           'a0'=c(0,0),'A0'=diag(0.1,2),
           'g0'=c(0,0),'G0'=diag(0.1,2),
           'N'=nrow(data)
)

par.tosave = c('sd'
               ,'betas'
               ,'alpha','gamma')
jmod = jags.model(file='gamma_mod.bug',data=dat,inits=init,n.chains=2,n.adapt=5000)
mcmcresults_eco = coda.samples(model=jmod,variable.names=par.tosave,n.iter=50000,thin=10)


summary(mcmcresults_eco)


pdf('C:/Users/samsung/Dropbox/Ma Thesis/Paper/Final/diag_all.pdf')
plot(mcmcresults_eco)
dev.off()




# Make tables

library(xtable)

# all topics
a = summary(mcmcresults_all)
l = summary(mcmcresults_lim_all)
res_tab = matrix(NA,nr=11,nc=4)
res_tab[,c(1:2)] = rbind(l$statistics[1:2,c(1:2)],NA,NA,NA,NA,NA,NA,l$statistics[3:5,c(1:2)])
res_tab[,c(3:4)] = a$statistics[,c(1:2)]
rownames(res_tab) = rownames(a$statistics)
res_tab_all = res_tab
xtable(res_tab_all)


# eco topics
a = summary(mcmcresults_eco)
l = summary(mcmcresults_lim_eco)
res_tab = matrix(NA,nr=11,nc=4)
res_tab[,c(1:2)] = rbind(l$statistics[1:2,c(1:2)],NA,NA,NA,NA,NA,NA,l$statistics[3:5,c(1:2)])
res_tab[,c(3:4)] = a$statistics[,c(1:2)]
rownames(res_tab) = rownames(a$statistics)
res_tab_lim = res_tab
rm(res_tab)
xtable(res_tab_lim)


save.image('results_topics20130611.RData')

