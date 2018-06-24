######################################################################
## Avaliação primeiro questionário
######################################################################
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(gridExtra)

q=read.table("data/other/questionario1.dat",h=T)
Q=data.frame(q=paste("Q",seq(1:10),sep=""),
             title=c("Categoria",
                     "Gênero",
                     "Idade",
                     "Transporte",
                     "Alimentação",
                     "Futebol: time preferido",
                     "Música: estilo preferido",
                     "Satisfação",
                     "Rotina",
                     "Melhoria"))
q=merge(q,Q)

plots = list()

colors<-brewer.pal(10,"Spectral")
for (i in 1:10) {
    plots[[i]]=ggplot(data=subset(q,q==paste("Q",i,sep="")))+
        geom_bar(aes(x=cat,weight=nrep),fill=colors[i])+
        labs(title=Q$title[i],x="",y="Número de respostas")+
        coord_flip()
    
}
grid.arrange(grobs=plots,nrows=2)

