  library(plyr)
  
  # wrapping function
  wrap.it <- function(x, len) { 
    sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
  }
  
  commaToDots <- function(y) {
    return(as.numeric(gsub(",",".",as.character(y))))
  }
  
  ################# read csv avaliacao pelos discentes ####################
  doc_disc <- read.csv2("AvalDocPeloDisc 2017-1.csv",sep= ",", strip.white=TRUE)
  #tur_tam <- read.csv2("TamTurmas2017-1.csv",sep= ";", strip.white=TRUE)
  
  ## troca virgulas
  n <- lapply(doc_disc[4], function (x) sapply(x, commaToDots ))
  doc_disc_num <- cbind(doc_disc[1:3],n)
  rm(n)
  
  ## Organiza turmas
  turmaN <- 1
  q <- 1
  m <- c(1:nrow(doc_disc_num))
  
  while(q<=nrow(doc_disc_num)){
    
    m[q] <- sub("^", turmaN, doc_disc_num$Turma[q])
    
    if(q == nrow(doc_disc_num)){
      rm(q)
      rm(turmaN)
      break
    }
    else if ((as.character(doc_disc_num$Turma[q]) == as.character(doc_disc_num$Turma[q+1]))&&(as.character(doc_disc_num$Questão[q]) == as.character(doc_disc_num$Questão[q+1])))
      turmaN <- turmaN + 1
    else
      turmaN <- 1
    q <- q + 1
  }
  doc_disc_classfied<-cbind(doc_disc_num[1:2],m,doc_disc_num[3:4])
  
  ## Operações para mudar o shape
  doc_disc_wide <- reshape(doc_disc_classfied[1:5], v.names = c("Nota"), idvar = c("Atividade.de.Ensino", "Turma", "m"), timevar="Questão", direction="wide") 
  
  #TESTE_wide$Turma <- lapply(TESTE_wide$Turma, function(x) gsub("\\d+", "", x))
  doc_disc_wide <- cbind(doc_disc_wide[1:2], doc_disc_wide[4:14])
  doc_disc_wide <- transform(doc_disc_wide,Turma=unlist(Turma))
  
  colnames(doc_disc_wide)=c("disc","turma","q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11")
  
  ##merge tam turma (outro df)
  #colnames(tur_tam)=c("disc","turma","tam")
  #Remove espaços indesejados no inicio e final
  #disc <- lapply(tur_tam[1], function (x) gsub('^.|.$', '', x))
  #tur_tam <- cbind(disc,tur_tam[2:3])
  #merge_test = merge(doc_disc_wide,tur_tam, by=c("disc", "turma"), all.x=T, sort=FALSE)
  #tur_tam = tur_tam[
  #  order(tur_tam[,1]),
  #]
  
  write.csv2(doc_disc_wide, "AvalDiscente_2017-1.csv", row.names = FALSE)
  #write.table(TESTE_wide, file = "MyData.csv", row.names = FALSE, sep=";")
  #write.csv(TESTE_wide, file = "MyData.csv",row.names=FALSE, na="NA")
