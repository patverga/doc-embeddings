library(ggplot2)

path="/home/pat/doc-embeddings/out/"

data <-  read.table(paste(path,'experiments.result', sep=''), sep=' ',header=F,stringsAsFactors=F)
colnames(data)<- c('experiment','MAP')
data$experiment=unlist(strsplit(data$experiment, ".result"))


#### rerank data #######
reranks=c('sum','sentence','centroid','sdm')
rerank.data=subset(data,experiment%in%reranks)

p1<-	ggplot(rerank.data, aes(x=experiment, y=MAP, fill=experiment))+theme_grey(20)+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())
print(p1)
########################


#### expansion data #####
expansions=c('embed-expansion','rm-embed-expansion','rm-expansion','wiki-expansion', 'sdm')
expansion.data=subset(data,experiment%in%expansions)

p2<-  ggplot(expansion.data, aes(x=experiment, y=MAP, fill=experiment))+theme_grey(20)+geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())
print(p2)
########################


ggsave(paste(path, 'rerank.png',sep=''), p1, width = 8, height = 5)
ggsave(paste(path, 'expansion.png',sep=''), p2, width = 8, height = 5)
