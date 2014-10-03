library(plyr)
library(ggplot2)
library(foreach)
library(grid)
library(scales)
input.path="/home/pv/Documents/totally-legal-trafficking/results/paper"
output.path="/home/pv/Documents/totally-legal-trafficking/tex/cikm/figure/"

# input.path="~/github/2014-code-totally-legal-trafficking/results/paper"
# output.path="~/github/2014-code-totally-legal-trafficking//tex/cikm/figure/"
data <-  read.table(paste(input.path, "class-label-table", sep="/"), sep=',',header=F,stringsAsFactors=F)
colnames(data)<- c('post_id','author_id')
class.counts <- data.frame(table(data$author_id))

# p1 <- ggplot(subset(class.counts, Freq<=200), aes(x=Freq)) + geom_bar(binwidth=10) + scale_x_discrete(limits=c(0,100), breaks=seq(0,200,1))
# print(p1)

####


data2<-(read.csv(paste0(input.path, "/class.counts.csv"),sep=","))
colnames(data2)<-c("count","author_id")
p1<-	ggplot(data2,aes(x=count))+theme_grey(20)+geom_histogram(fill="white",color="black")+scale_x_log10(limit=c(1,120),breaks=c(2,4,6,8,10,20,40,60,80,100,200))+labs(x="Posts per author",y="Frequency")+theme(axis.text.x=element_text(angle=-90,vjust=.5))
print(p1)
ggsave(paste(output.path, 'class-sizes.pdf',sep=''), p1, width = 8, height = 5)
