######################################################################
## Avaliação primeiro questionário
######################################################################
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(GGally)
library(gridExtra)

questions=paste("Q",1:10,sep="")

q=read.table("data/other/questionario1.dat",h=T)
q$q=factor(q$q,levels=questions,ordered=T)
Q=data.frame(q=questions,
             title=c("Categoria",
                     "Gênero",
                     "Idade",
                     "Transporte",
                     "Alimentação",
                     "Futebol: time preferido",
                     "Música: estilo preferido",
                     "Satisfação: já pensou em trocar",
                     "Rotina",
                     "Melhoria"))
q=merge(q,Q)
q=q %>% merge(q %>% group_by(q) %>% summarize(N=sum(nrep))) %>% mutate(pct=100*nrep/N)

pdf(width=18,height=12)

colors<-brewer.pal(10,"Spectral")
plots = list()
for (i in 1:10) {
    plots[[i]]=ggplot(data=subset(q,q==paste("Q",i,sep="")))+
        geom_bar(aes(x=cat,weight=nrep),fill=colors[i])+
        geom_text(aes(x=cat,y=nrep+2,label=sprintf("%.1f%%",pct)),size=1.75,hjust=0,color="black")+
        labs(title=paste(Q$title[i]," (",q$N[1]," respostas)",sep=""),x="",y="Número de respostas")+
        coord_flip()
}
grid.arrange(grobs=plots,nrow=5,ncol=2)


dev.off()


