library(languageR)
library(lme4)
library(lattice)
library(rms)
data("dative")

set.seed(123)

mod <- data.frame(dative)
m <- matrix(nrow=length(mod[0,]), ncol=length(mod[0,]))
rownames(m) <- names(mod)
colnames(m) <- names(mod)

for (i in 1:nrow(m)) {
  for (j in i:nrow(m)) { 
    m[i,j] <- cor(as.numeric(factor(mod[,i])), as.numeric(factor(mod[,j]))) 
  }
}

m

plotted <- c(m)
plotted <- plotted[! is.na(plotted) ]
plotted <- plotted[plotted < 0.9999999]
plot(plotted,   xlab="", xaxt='n', ylab="Score", type="h", main="Corrélations deux à deux des variables")

exocor<-data.frame(SC=mod$SemanticClass,CR=mod$AccessOfRec,CT=mod$AccessOfTheme,
                   PT=mod$PronomOfTheme,PR=mod$PronomOfRec,
                   AR=mod$AnimacyOfRec,
                   DR=mod$DefinOfRec,DT=mod$DefinOfTheme,
                   L=ifelse(mod$LengthOfRecipient-mod$LengthOfTheme != 0, log(abs(mod$LengthOfRecipient-mod$LengthOfTheme)), 0),
                   R=mod$RealizationOfRecipient,
                   id=1:nrow(mod))

train <- exocor[sample(nrow(exocor), (nrow(exocor)*3)/4), ]
selected <- train$id
test <- exocor[!exocor$id %in% selected,]

modcor<-glm(R ~ SC+AR+PR+PT+DR+DT+CR+CT+L,data=train,family=binomial)
summary(modcor)
ndatacor<-data.frame(R=test$R,SC=test$SC,AR=test$AR,PR=test$PR,PT=test$PT,DR=test$DR,DT=test$DT,CR=test$CR, CT=test$CT,L=test$L)

preds<-predict(modcor,ndatacor,type="response", allow.new.levels=TRUE)
Result<-ifelse(preds>0.5,"PP","NP")
ndatacor$Result <- Result
allPP <- ndatacor[ndatacor$R=="PP",]
allNP <- ndatacor[ndatacor$R=="NP",]

sum(Result==ndatacor$R)/length(Result)
nrow(allPP[allPP$Result=="PP",])/nrow(allPP)
nrow(allNP[allNP$Result=="NP",])/nrow(allNP)

