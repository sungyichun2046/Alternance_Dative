# calculer l'accuracy de Model C et trouver ROC
library(languageR)
library(Matrix)
library(ROCR)
library(lme4)
library(lattice)
data("dative")


# split le corpus en corpus d'apprentissage et corpus de test
sense <- paste(dative$Verb,dative$SemanticClass,sep="+")
dativesense<-cbind(dative,sense)
exo<-data.frame(PT=dativesense$PronomOfTheme,PR=dativesense$PronomOfRec,LT=dativesense$LengthOfTheme,LR=dativesense$LengthOfRecipient,AR=dativesense$AnimacyOfRec,AT=dativesense$AnimacyOfTheme,DR=dativesense$DefinOfRec,DT=dativesense$DefinOfTheme,R=dativesense$RealizationOfRecipient,SC=dativesense$SemanticClass,CR=dativesense$AccessOfRec,CT=dativesense$AccessOfTheme,PT=dativesense$PronomOfTheme,Verb=dativesense$Verb,id=1:nrow(dativesense))
set.seed(123) 
train <- exo[sample(nrow(exo), (nrow(exo)*3)/4), ] 
selected <- train$id 
test <- exo[!exo$id %in% selected,]
mod<-glmer(R~LT+LR+AR+DR+DT+(1|Verb),data=train,family=binomial)
ndata<-data.frame(LT=test$LT,LR=test$LR,AR=test$AR,DR=test$DR,DT=test$DT,Verb=test$Verb)
ndata$predict<-predict(mod,ndata,type="response",allow.new.levels = TRUE)
ndata$Ref<-test$R
preds<-prediction(ndata$predict,ndata$Ref)
perfs<-performance(preds,"tpr","fpr")
plot(perfs,colorize=T)
auc<-performance(preds,"auc")
attr(auc,"y.values")
dotplot(ranef(mod,condVar=T))[["Verb"]]
sum(ndata$predict>0.5)
sum(ndata$predict<0.5)
length(ndata$Verb)
# trouver la distribution de PR
PRtable<-data.frame(PR=exo$PR)
table(unlist(PRtable))
barplot(table(unlist(PRtable)), main = "Pourcentage de pronoms de récipients dans le corpus oral")
#### comparer la version "spoken" et la version "written" #####
# version spoken
dativespoken <- dativesense[dativesense$Modality=="spoken",]
dativewritten <-dativesense[dativesense$Modality=="written",]
exospoken<-data.frame(PT=dativespoken$PronomOfTheme,PR=dativespoken$PronomOfRec,LT=dativespoken$LengthOfTheme,LR=dativespoken$LengthOfRecipient,AR=dativespoken$AnimacyOfRec,AT=dativespoken$AnimacyOfTheme,DR=dativespoken$DefinOfRec,DT=dativespoken$DefinOfTheme,R=dativespoken$RealizationOfRecipient,SC=dativespoken$SemanticClass,CR=dativespoken$AccessOfRec,CT=dativespoken$AccessOfTheme,PT=dativespoken$PronomOfTheme,Verb=dativespoken$Verb,id=1:nrow(dativespoken))
set.seed(123) 
trainspoken <- exospoken[sample(nrow(exospoken), (nrow(exospoken)*3)/4), ] 
selected <- trainspoken$id 
testspoken <- exospoken[!exospoken$id %in% selected,]
modspoken<-glmer(R~LT+LR+AR+DR+DT+(1|Verb),data=trainspoken,family=binomial)
ndataspoken<-data.frame(LT=testspoken$LT,LR=testspoken$LR,AR=testspoken$AR,DR=testspoken$DR,DT=testspoken$DT,Verb=testspoken$Verb)
ndataspoken$predict<-predict(modspoken,ndataspoken,type="response",allow.new.levels = TRUE)
ndataspoken$Ref<-testspoken$R
predsspoken<-prediction(ndataspoken$predict,ndataspoken$Ref)
perfsspoken<-performance(predsspoken,"tpr","fpr")
plot(perfsspoken,colorize=T)
aucspoken<-performance(predsspoken,"auc")
attr(aucspoken,"y.values")
dotplot(ranef(modspoken,condVar=T))[["Verb"]]
sum(ndataspoken$predict>0.5)
sum(ndataspoken$predict<0.5)
length(ndataspoken$Verb)
# trouver la distribution de PR
PRtable<-data.frame(PR=exospoken$PR)
table(unlist(PRtable))
barplot(table(unlist(PRtable)), main = "Pourcentage de pronoms de récipients dans le corpus oral")
# version "written"

exowritten<-data.frame(PT=dativewritten$PronomOfTheme,PR=dativewritten$PronomOfRec,LT=dativewritten$LengthOfTheme,LR=dativewritten$LengthOfRecipient,AR=dativewritten$AnimacyOfRec,AT=dativewritten$AnimacyOfTheme,DR=dativewritten$DefinOfRec,DT=dativewritten$DefinOfTheme,R=dativewritten$RealizationOfRecipient,SC=dativewritten$SemanticClass,CR=dativewritten$AccessOfRec,CT=dativewritten$AccessOfTheme,PT=dativewritten$PronomOfTheme,Verb=dativewritten$Verb,id=1:nrow(dativewritten))
set.seed(123) 
trainwritten <- exowritten[sample(nrow(exowritten), (nrow(exowritten)*3)/4), ] 
selected <- trainwritten$id 
testwritten <- exowritten[!exowritten$id %in% selected,]
modwritten<-glmer(R~LT+LR+AR+DR+DT+(1|Verb),data=trainwritten,family=binomial)
ndatawritten<-data.frame(LT=testwritten$LT,LR=testwritten$LR,AR=testwritten$AR,DR=testwritten$DR,DT=testwritten$DT,Verb=testwritten$Verb)
ndatawritten$predict<-predict(modwritten,ndatawritten,type="response",allow.new.levels = TRUE)
ndatawritten$Ref<-testwritten$R
predswritten<-prediction(ndatawritten$predict,ndatawritten$Ref)
perfswritten<-performance(predswritten,"tpr","fpr")
plot(perfswritten,colorize=T)
aucwritten<-performance(predswritten,"auc")
attr(aucwritten,"y.values")
dotplot(ranef(modwritten,condVar=T))[["Verb"]]
sum(ndatawritten$predict>0.5)
sum(ndatawritten$predict<0.5)
length(ndatawritten$Verb)
# trouver la distribution de PR
PRtable<-data.frame(PR=exowritten$PR)
table(unlist(PRtable))
barplot(table(unlist(PRtable)), main = "Pourcentage de pronoms de récipients dans le corpus oral")

