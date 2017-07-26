library(languageR)
library(lme4)
library(lattice)
data("dative")

set.seed(123)

#sense <- paste(dative$Verb,dative$SemanticClass,sep="+")
exo<-data.frame(LT=dative$LengthOfTheme,
                LR=dative$LengthOfRecipient,
                AR=dative$AnimacyOfRec,
                DR=dative$DefinOfRec,
                DT=dative$DefinOfTheme,
                R=dative$RealizationOfRecipient,
                SC=dative$SemanticClass,
                CR=dative$AccessOfRec,
                CT=dative$AccessOfTheme,
                PR=dative$PronomOfRec,
                PT=dative$PronomOfTheme,
                VS= paste(dative$Verb,dative$SemanticClass,sep="+"),
                id=1:nrow(dative)
               )

train <- exo[sample(nrow(exo), (nrow(exo)*3)/4), ]
selected <- train$id
test <- exo[!exo$id %in% selected,]

mod<-glm(R~CR+CT+PR+PT+AR+DR+DT+LR+LT+VS,data=exo,family=binomial("logit"))
ndata<-data.frame(R=test$R,
                  CR=test$CR, 
                  CT=test$CT, 
                  PR=test$PR, 
                  PT=test$PT, 
                  AR=test$AR, 
                  DR=test$DR,
                  DT=test$DT,
                  VS=test$VS, 
                  LR=test$LR, 
                  LT=test$LT)
ndata$predict<-predict(mod,ndata,type="response",allow.new.levels = TRUE)

summary(mod)

Result<-ifelse(ndata$predict>0.5,"PP","NP")

details <- data.frame(ndata)
details$Result <- Result
detailsNP <- details[details$R == "NP",]
detailsPP <- details[details$R == "PP",]

overall <- sum(Result==ndata$R)/length(Result)

overall
nrow(detailsPP[detailsPP$Result=="PP",])/nrow(detailsPP)
nrow(detailsNP[detailsNP$Result=="NP",])/nrow(detailsNP)


#resultats détailés

res=matrix(nrow=length(unique(exo$VS)), ncol=6)
rownames(res) <- unique(exo$VS)
colnames(res) <- c( "nb", "pred", "div", "NB","suppr", "var")
for (i in unique(exo$VS)) {
  tmp<-ndata[ndata$VS==i, ]
  nb <- nrow(tmp)
  predResult<-ifelse(tmp$predict>0.5,"PP","NP")
  calc <- sum(predResult==tmp$R)/length(predResult)
  if (nb == 0) {
    predResult <- 0
    calc <- 1
  }
  div <- abs(overall-calc)
  #calcul de suppr
  tmpExo <- exo[exo$VS != i, ]
  tmpTrain <- tmpExo[sample(nrow(tmpExo), (nrow(tmpExo)*3)/4), ]
  tmpSelected <- tmpTrain$id
  tmpTest <- tmpExo[!tmpExo$id %in% tmpSelected,]
  tmpMod<-glm(R~CR+CT+PR+PT+AR+DR+DT+LR+LT+VS,data=tmpExo,family=binomial("logit"))
  tmpNData<-data.frame(R=tmpTest$R, CR=tmpTest$CR, CT=tmpTest$CT, PR=tmpTest$PR, PT=tmpTest$PT, AR=tmpTest$AR, DR=tmpTest$DR,DT=tmpTest$DT,VS=tmpTest$VS, LR=tmpTest$LR, LT=tmpTest$LT)
  tmpNData <- tmpNData[tmpNData$VS != i, ]
  
  tmpNData$predict<-predict(tmpMod,tmpNData,type="response",allow.new.levels = TRUE)
  predResult<-ifelse(tmpNData$predict>0.5,"PP","NP")
  nball <- nrow(exo) - nrow(tmpExo)
  suppr <- sum(predResult==tmpNData$R)/length(predResult)
  vari <- abs(overall-suppr)
  res[i, ]<-c(nb, calc, div, nball, suppr, vari)
}
overall
res
plot(x = res[, "suppr"], y = res[, "pred"], xlim=c(0, 1), ylim=c(0,1), xlab="modèle sans terme", ylab="prédiction du modèle", main="Précision et biais")