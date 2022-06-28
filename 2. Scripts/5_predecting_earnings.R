########################################
# Age Earning profile
# Problem_Set_1 
# Grupo XX
########################################



## Preparación del espacio y librerías ---------------------------------------------------
rm(list = ls())


setwd("C:/Users/ocaco/OneDrive/15. Maestria Economia/9. Big Data/3. GitHub/Problem_-Set_1/3. Stores")

# Llamado librerías
require(pacman)

p_load(
  tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  stargazer,
  tidyverse,
  boot,
  mcmspatial,
  caret
)

##Cargue de la base de datos

# Se importa base de datos obtenida del scraping
dbCompleta <- readRDS("db5.rds")

###--- 1. Predicción fuera de muestra ---###
set.seed(10101)

df<-  dbCompleta %>% select(c("age", "age2", "female", "edu", "formal", "y_total_m", "logingtot","oficio"))
df <-df%>%mutate(holdout=as.logical(1:nrow(df)%in%
                                                      sample(nrow(df),nrow(df)*.3)))
test <- df[df$holdout==T,]
train <- df[df$holdout==F,]

MSE<-matrix(rep(0,21),nrow=7,ncol=3)
colnames(MSE)<- c("Modelo","MSE","MSE_k-fold")
#modelo 1
model1 <- lm(logingtot~1, data = train)
summary(model1)
coef(model1)
mean(train$y_total_m)
test$model1<-predict(model1,newdata = test)
MSE[1,1]<-1
MSE[1,2]<- with(test,mean((logingtot-model1)^2))


#modelo 2
model2 <- lm(logingtot~age+age2, data = train)
test$model2<-predict(model2,newdata = test)
MSE[2,1]<-2
MSE[2,2]<- with(test,mean((logingtot-model2)^2))

#modelo 3
model3 <- lm(logingtot~age+age2+female, data = train)
test$model3<-predict(model3,newdata = test)
MSE[3,1]<-3
MSE[3,2]<-with(test,mean((logingtot-model3)^2))

#modelo 4
model4 <- lm(logingtot~age+age2+female+edu, data = train)
test$model4<-predict(model4,newdata = test)
MSE[4,1]<-4
MSE[4,2]<-with(test,mean((logingtot-model4)^2))

#modelo 5
model5 <- lm(logingtot~age+age2+female+edu+formal, data = train)
test$model5<-predict(model5,newdata = test)
MSE[5,1]<-5
MSE[5,2]<-with(test,mean((logingtot-model5)^2))

#modelo 6
model6 <- lm(logingtot~age+age2+female+edu+formal+age:female, data = train)
test$model6<-predict(model6,newdata = test)
MSE[6,1]<-6
MSE[6,2]<-with(test,mean((logingtot-model6)^2))

#modelo 7
model7 <- lm(logingtot~age+age2+female+edu+formal+age:female+poly(oficio,8), data = train)
test$model7<-predict(model7,newdata = test)
MSE[7,1]<-7
MSE[7,2]<-with(test,mean((logingtot-model7)^2))

stargazer(model1, model2, model3, model4, model5, model6, model7, type = "text")


###--- 2. Validación cruzada ---###



reg1 <- train(logingtot~.,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "null"
              )
MSE[1,3]<- reg1$results$RMSE^2
reg2 <- train(logingtot~age+age2,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[2,3]<- reg2$results$RMSE^2

reg3 <- train(logingtot~age+age2+female,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[3,3]<- reg3$results$RMSE^2

reg4 <- train(logingtot~age+age2+female+edu,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[4,3]<- reg4$results$RMSE^2

reg5 <- train(logingtot~age+age2+female+edu+formal,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[5,3]<- reg5$results$RMSE^2

reg6 <- train(logingtot~age+age2+female+edu+formal+age:female,
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[6,3]<- reg6$results$RMSE^2


reg7 <- train(logingtot~age+age2+female+edu+formal+age:female+poly(oficio,8),
              data=df,
              trControl = trainControl(method = "cv", number = 5),
              method= "lm"
)
MSE[7,3]<- reg7$results$RMSE^2

stargazer(MSE, type = "text")

###--- 3. LOOCV ---###

n<- nrow(df)
loocv<-matrix(rep(0,n),nrow=n,ncol=3)
loocv[,1]<-df$logingtot
colnames(loocv)<- c("Observacion","Prediccion","MSE")
for (i in 1:n) {
  reg_i<-lm(logingtot~age+age2+female+edu+formal+age:female+poly(oficio,8),
            data=df[-i,])#entrena con los datos menos la i observación
  loocv[i,2]<-predict(reg_i,newdata=df[i,]) #predice con la i observación
  loocv[i,3]<-(loocv[i,1]-loocv[i,2])^2
  }
loocv <- as.data.frame(loocv)
MSE_loocv <- mean(loocv$MSE)
MSE_loocv
