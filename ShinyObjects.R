library(PRROC)

demos <- odata
levels(demos$MARRIAGE) <- c("Casado","Otro","Soltero")
levels(demos$EDUCATION) <- c("Otro","Licenciatura","Maestría/Doctorado","Otro","Bachillerato")
levels(fdata$MARRIAGE) <- c("Casado","Otro","Soltero")
levels(fdata$EDUCATION) <- c("Otro","Licenciatura","Maestría/Doctorado","Otro","Bachillerato")
fdata <- fdata[,-14]

finalmodel <- train(default~.,data=fdata,method="nnet",trControl=trainControl(method="cv", number=9, summaryFunction = prSummary, classProbs = TRUE, savePredictions = TRUE),preProcess=c("center","scale"),tuneGrid=expand.grid(size=c(14), decay=c(0.1)),metric="ROC",maxit=900)

results <- data.frame(real=as.numeric(fdata$default)-1,pred=finalmodel$finalModel$fitted.values)

rocdata <- as.data.frame(roc.curve(scores.class1 = filter(results,real==0)[,2],scores.class0 = filter(results,real==1)[,2],curve=T)$curve)
prdata <- as.data.frame(pr.curve(scores.class1 = filter(results,real==0)[,2],scores.class0 = filter(results,real==1)[,2],curve=T)$curve)

rocdata[,3] <- round(rocdata[,3],2)
rocdata <- group_by(rocdata,V3) %>% summarise(V1=head(V1,1),V2=head(V2,1))

prdata[,3] <- round(prdata[,3],2)
prdata <- group_by(prdata,V3) %>% summarise(V1=head(V1,1),V2=head(V2,1))
