rm(list = ls(all.names = TRUE))
gc()
#--------------------------------------*----------------------------------------
#------------------------------- Proyecto 2  -----------------------------------
#--------------------------------------*----------------------------------------

datos <- read.csv("outputs/fmed_seleccionadas.csv", header = TRUE)
str(datos) # 1787 observaciones
summary(datos)
#View(datos)

# Tratamos a la mayoria de las variables como categoricas/factor
datos$sexo <- factor(datos$sexo,
                     levels = c('M', 'F'),
                     labels = c("hombre", "mujer"))
datos$grado_estudios <- factor(datos$grado_estudios)
datos$ejer_act <- factor(datos$ejer_act)
datos$ejer_1 <- factor(datos$ejer_1)
datos$ejer_5 <- factor(datos$ejer_5)
datos$suenio_act <- factor(datos$suenio_act)
datos$suenio_1 <- factor(datos$suenio_1)
datos$suenio_5 <- factor(datos$suenio_5)
datos$obesidad <- factor(datos$obesidad)
str(datos)
summary(datos)



#--------------------------------------*----------------------------------------
#---------------------- Conteos Naive   ----------------------------------------
#--------------------------------------*----------------------------------------
nrow(datos[datos$obesidad == 1, ])
nrow(datos[datos$ejer_act == "B", ])
nrow(datos[datos$ejer_1 == "M", ])
nrow(datos[datos$ejer_5 == "M", ])
nrow(datos[datos$obesidad == 1 & datos$ejer_act == "B", ])
nrow(datos[datos$obesidad == 1 & datos$ejer_1 == "M", ])
nrow(datos[datos$obesidad == 1 & datos$ejer_5 == "M", ])
nrow(datos[datos$obesidad == 0, ])


#--------------------------------------*----------------------------------------
#---------------------- Regresion Logística  -----------------------------------
#--------------------------------------*----------------------------------------
# 2/3 para entrenar y 1/3 para pruebas
train <- datos[1:476,]
test <- datos[477:714,]

#--------------------------------------*----------------------------------------
#---------------------------- Primer Intento -----------------------------------
#----------------------- Modelo con interacciones ------------------------------
#--------------------------------------*----------------------------------------
# La variable referencia es A
fit <- glm(obesidad ~ ejer_act * ejer_1 * ejer_5 , family=binomial(link='logit'), data=train)
summary(fit)

anova(fit, test="Chisq")

fitted.results <- predict(fit, newdata = subset(test, select=c("ejer_act", "ejer_1", "ejer_5")), type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$obesidad)
print(paste('Accuracy', 1 - misClasificError)) # Accuracy 0.806179775280899



library(ROCR)
#p <- predict(fit, newdata=subset(test,select=c("ejer_act", "ejer_1", "ejer_5", "ejer_10")), type="response")
p <- predict(fit, test, type="response")
pr <- prediction(p, test$obesidad)
pr <- prediction(p, test$obesidad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="Curva ROC mod con interacciones")
abline(coef = c(0, 1),
       col = "red",
       lwd = 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc ### Area bajo la curva ROC 0.5791799
legend("bottomright", inset=.02, title="AUC", legend = c(auc), col = c(auc))

#--------------------------------------*----------------------------------------
#--------------------------- Segundo  Intento ----------------------------------
#----------------------- Modelo sin interacciones ------------------------------
#--------------------------------------*----------------------------------------


modelo <- glm(obesidad ~ ejer_act + ejer_1 + ejer_5 + suenio_act + suenio_1 + suenio_5, family=binomial(link='logit'), data=train)
modelo <- glm(obesidad ~ ejer_act + ejer_1 + ejer_5, family=binomial(link='logit'), data=train)
summary(modelo)

anova(modelo, test="Chisq")

#fitted.results <- predict(modelo, newdata = subset(test, select=c("ejer_act", "ejer_1", "ejer_5", "ejer_10")), type='response')
fitted.results <- predict(modelo, test, type="response")
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$obesidad)
print(paste('Accuracy', 1 - misClasificError)) # "Accuracy 0.806179775280899"



library(ROCR)
#p <- predict(fit, newdata=subset(test,select=c("ejer_act", "ejer_1", "ejer_5", "ejer_10")), type="response")
p <- predict(modelo, test, type="response")
pr <- prediction(p, test$obesidad)
pr <- prediction(p, test$obesidad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="Curva ROC mod sin interacciones")
abline(coef = c(0, 1),
       col = "red",
       lwd = 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc ### Area bajo la curva ROC 0.5823613 vs 0.5791799 del anterior
legend("bottomright", inset=.02, title="AUC", legend = c(auc), col = c(auc))


#--------------------------------------*----------------------------------------
#--------------------------- Tercer  Intento -----------------------------------
#----------------------- Modelo solo con habito actual -------------------------
#--------------------------------------*----------------------------------------


fit2 <- glm(obesidad ~ ejer_act + ejer_1 + ejer_5 + sexo + grado_estudios, family=binomial(link='logit'), data=train)
summary(fit2)

anova(fit2, test="Chisq")

#fitted.results <- predict(fit2, newdata = subset(test, select=c("ejer_act", "ejer_1", "ejer_5", "ejer_10")), type='response')
fitted.results <- predict(fit2, test, type="response")
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$obesidad)
print(paste('Accuracy', 1 - misClasificError)) # "Accuracy 0.808988764044944" vs "Accuracy 0.806179775280899" 



library(ROCR)
#p <- predict(fit, newdata=subset(test,select=c("ejer_act", "ejer_1", "ejer_5", "ejer_10")), type="response")
p <- predict(fit2, test, type="response")
pr <- prediction(p, test$obesidad)
pr <- prediction(p, test$obesidad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="Curva ROC mod sin interacciones más sexo y grado de estudios")
abline(coef = c(0, 1),
       col = "red",
       lwd = 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc ### Area bajo la curva ROC 0.6177347 vs 0.5545877 vs 0.5823613 vs 0.5791799 del anterior

legend("bottomright", inset=.02, title="AUC", legend = c(auc), col = c(auc))


#library(bestglm)
# Prepare data
#X.matrix <- as.data.frame(model.matrix(obesidad ~ ejer_act + ejer_1 + ejer_5 + ejer_10 + sexo + grado_estudios, datos))

## Perform
#res.best.logistic <-
#  bestglm(X.matrix,
#         family = binomial,          # binomial family for logistic
#          IC = "AIC",                 # Information criteria for
#          method = "exhaustive")

#--------------------------------------*----------------------------------------
#---------------------- Regresion Logística  -----------------------------------
#--------------------------------------*----------------------------------------

#datos <- factor(datos, levels = levels(reference))
# 2/3 para entrenar y 1/3 para pruebas
train <- datos[1:476,]
test <- datos[477:714,]

modelo <- glm(obesidad ~ ejer_act + ejer_1 + ejer_5 + suenio_act + suenio_1 + suenio_5, family=binomial(link='logit'), data=train)
modelo <- glm(obesidad ~ ejer_act + ejer_1 + ejer_5, family=binomial(link='logit'), data=train)

# Realizar la predicción
prediccion <- predict(modelo, newdata = subset(test, select = c("ejer_act", "ejer_1", "ejer_5", "suenio_act", "suenio_1", "suenio_5")), type="response")
prediccion <- predict(modelo, newdata = subset(test, select = c("ejer_act", "ejer_1", "ejer_5")), type="response")

# Mostrar métricas de predicción
library(caret)
confusionMatrix(prediccion, test$obesidad)

# Realizar cross validation
library(boot)
cv <- cv.glm(data = test, glmfit = modelo)
cv$delta

