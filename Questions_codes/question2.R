library(languageR)
library(lme4)
library(lattice)
data("dative")

dativespoken <- dativesense[dative$Modality=="spoken",]
exospoken<-data.frame(SC=dativespoken$SemanticClass,CR=dativespoken$AccessOfRec,CT=dativespoken$AccessOfTheme,PT=dativespoken$PronomOfTheme,PR=dativespoken$PronomOfRec,S=dativespoken$Speaker, LT=dativespoken$LengthOfTheme,LR=dativespoken$LengthOfRecipient,AR=dativespoken$AnimacyOfRec,AT=dativespoken$AnimacyOfTheme,DR=dativespoken$DefinOfRec,DT=dativespoken$DefinOfTheme,R=dativespoken$RealizationOfRecipient,SC=dativespoken$SemanticClass,CR=dativespoken$AccessOfRec,CT=dativespoken$AccessOfTheme,PT=dativespoken$PronomOfTheme,id=1:nrow(dativespoken))
set.seed(123) 
trainspoken <- exospoken[sample(nrow(exospoken), (nrow(exospoken)*3)/4), ] 
selected <- trainspoken$id 
testspoken <- exospoken[!exospoken$id %in% selected,]

modspoken1<-glm(R~SC+LT+LR+AR+AT+PR+PT+DR+DT+S,data=trainspoken,family=binomial)
ndataspoken1<-data.frame(S=testspoken$S,R=testspoken$R,SC=testspoken$SC,LT=testspoken$LT,LR=testspoken$LR,AR=testspoken$AR,AT=testspoken$AT,PR=testspoken$PR,PT=testspoken$PT,DR=testspoken$DR,DT=testspoken$DT)
preds<-predict(modspoken1,ndataspoken1,type="response",allow.new.levels = TRUE)
Result<-ifelse(preds>0.5,"PP","NP")
sum(Result==ndataspoken1$R)/length(Result)
summary(modspoken1)





# ajouter une variable aléatore speaker
modspoken<-glmer(R~SC+LT+LR+AR+AT+PR+PT+DR+DT+(1|S),data=trainspoken,family=binomial)
ndataspoken<-data.frame(R=testspoken$R,SC=testspoken$SC,LT=testspoken$LT,LR=testspoken$LR,AR=testspoken$AR,AT=testspoken$AT,PR=testspoken$PR,PT=testspoken$PT,DR=testspoken$DR,DT=testspoken$DT,S=testspoken$S)
preds<-predict(modspoken,ndataspoken,type="response",allow.new.levels = TRUE)
Result<-ifelse(preds>0.5,"PP","NP")
sum(Result==ndataspoken$R)/length(Result)
summary(modspoken)
