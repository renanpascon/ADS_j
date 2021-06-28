library(readxl)
library(tidyverse)
library(stringr)
library(corrplot)
library(caret)
library(gam)
library(randomForest)

# Importando os dados
dicionario <-  read_xlsx("Bases_Final_ADS_Jun2021.xlsx",sheet = 1)
dados <- read_xlsx("Bases_Final_ADS_Jun2021.xlsx",sheet = 2)

# Juntar as colunas por palavras chaves 

comercioCol <- grep("Comércio", dicionario$descrição)
industriaCol <- grep("Indústria", dicionario$descrição)
residencialCol <- grep("Residencial", dicionario$descrição)
centro <- grep("Centro Oeste", dicionario$descrição)
norte <- grep("Norte", dicionario$descrição)
nordeste <- grep("Nordeste", dicionario$descrição)
sul <- grep("Sul", dicionario$descrição)
sudeste <- grep("Sudeste", dicionario$descrição)
consumo <- grep("Consumo", dicionario$descrição)
PMCA <- grep("Ampliada",dicionario$descrição)
PMCR <- grep("Restrita",dicionario$descrição)
Tmax <- grep("Máxima",dicionario$descrição)
Tmin <- grep("Minima",dicionario$descrição)
Pindustrial <- grep("Produção",dicionario$descrição)
# Consumo

Comercio <- dados[intersect(consumo,comercioCol)] %>% rowSums(na.rm = FALSE)
hist(Comercio,breaks = 10)
mean(Comercio,na.rm = TRUE)
sd(Comercio, na.rm = TRUE)

Industria <- dados[intersect(consumo,industriaCol)] %>% rowSums(na.rm = FALSE)
hist(Industria,breaks = 10)
mean(Industria,na.rm = TRUE)
sd(Industria, na.rm = TRUE)

Residencial <- dados[intersect(consumo,residencialCol)] %>% rowSums(na.rm = FALSE)
hist(Residencial,breaks = 10)
mean(Residencial,na.rm = TRUE)
sd(Residencial, na.rm = TRUE)

#  PLots
consumoBR <- cbind(Comercio,Industria,Residencial) %>% data.frame()
consumoBR$data <- as.Date(dados$data_tidy)
consumoBR <- pivot_longer(consumoBR,Comercio:Residencial,names_to = "categoria")
consumoBR %>%  ggplot() + geom_line(aes(x = data,y = value, color = categoria))

plot(dados$data_tidy, dados$renda_r)
plot(dados$data_tidy, dados$pop_ocup_br)
plot(dados$data_tidy, dados$massa_r)

# Unir as variaveis de região para Brasil
PMCAmedia <- dados[PMCA] %>% rowMeans(na.rm = FALSE)
PMCRmedia <- dados[PMCR] %>% rowMeans(na.rm = FALSE)
Tmaxmedia <- dados[Tmax] %>% rowMeans(na.rm = FALSE)
Tminmedia <- dados[Tmin] %>% rowMeans(na.rm = FALSE)
Pindustrialmedia <- dados[Pindustrial] %>% rowMeans(na.rm = FALSE)

plot(PMCAmedia)
plot(PMCRmedia)
plot(Tmaxmedia)
plot(Tminmedia)
plot(Pindustrialmedia)

# Correlaçõess sem renda 
dadosBR <- cbind(Comercio,Industria,Residencial,dados[c(17,18,19,20)],PMCAmedia,PMCRmedia,Tmaxmedia,Tminmedia,Pindustrialmedia)
dadosBR_consumo <- dadosBR %>% filter(!is.na(Comercio)) # Retirando dados de consumo que não temos
dadosBR_consumo <- dadosBR_consumo[-c(4,6)]   # sem as rendas
corBR <- cor(dadosBR_consumo)
corrplot(corBR, type="upper")

# Correlaçõess com renda 
dadosBR_consumo_renda <- dadosBR %>% filter(!is.na(Comercio), !is.na(massa_r)) # Retirando dados de consumo que não temos
corBR <- cor(dadosBR_consumo_renda)
corrplot(corBR, type="upper")


# Análise pela região Sudeste
dadosSE <- dados[c(1,sudeste,17:20)]  # Seleção de data , sudeste e variaveis de renda, pop e dias uteis
dadosSE <- dadosSE[-c(2,4)] # Deixando somente industria
dadosSE_consumo <- dadosBR %>% filter(!is.na(Comercio)) # Retirando dados de consumo que não temos

corSE <- cor(dadosSE_consumo[-1]) # correlação de consumo
corrplot(corSE, type="upper")

dadosBR_consumo_renda <- dadosSE %>% filter(!is.na(ind_se), !is.na(massa_r)) # Retirando dados de consumo e renda que não temos
corSE <- cor(dadosBR_consumo_renda[-1]) # correlação de consumo com a renda
corrplot(corSE, type="upper")

plot(dadosSE$ind_se)

##### Previsões 
dadosSE_previsao <- dadosSE %>% filter(!is.na(Comercio))  #dados com todos os dados de consumo de industria

# Função para normalizar
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dadosSE_previsao$ind_se_norm <- normalize(dadosSE_previsao$ind_se)
dadosSE_previsao$pim_se_norm <- normalize(dadosSE_previsao$pim_se)

test_index <- createDataPartition(dadosSE_previsao$ind_se_norm, times = 1, p = 0.5, list = FALSE)

train_set <- dadosSE_previsao %>% slice(-test_index)  #train_set
test_set <- dadosSE_previsao %>%  slice(test_index)    #test_set

# Regressão Logistica com maior correlação
fit <- lm(ind_se_norm ~ pim_se, data = train_set)
yPrevisao <- predict(fit,test_set)
sqrt(mean((yPrevisao - test_set$ind_se_norm)^2))

# Modelo Linear generalizado para todas as variaveis menos de renda

fit <- glm(ind_se_norm ~ pim_se_norm + pmc_a_se + temp_max_se  + temp_min_se + pmc_r_se + pop_ocup_br + du, data = train_set, family = "gaussian")
yPrevisao <- predict(fit,test_set)
sqrt(mean((yPrevisao - test_set$ind_se_norm)^2))


### Mesma previsão só que com a variavel de renda
dadosSE_previsao <- dadosSE %>% filter(!is.na(ind_se), !is.na(massa_r))

dadosSE_previsao$ind_se_norm <- normalize(dadosSE_previsao$ind_se)
dadosSE_previsao$pim_se_norm <- normalize(dadosSE_previsao$pim_se)

test_index <- createDataPartition(dadosSE_previsao$ind_se_norm, times = 1, p = 0.5, list = FALSE)

train_set <- dadosSE_previsao %>% slice(-test_index)  #train_set
test_set <- dadosSE_previsao %>%  slice(test_index)    #test_set

# Regressão Logistica com maiores correlações
fit <- lm(ind_se_norm ~ pim_se + massa_r, data = train_set)
yPrevisao <- predict(fit,test_set)
sqrt(mean((yPrevisao - test_set$ind_se_norm)^2))

# Modelo Linear generalizado para todas as variaveis menos de renda

fit <- glm(ind_se_norm ~ pim_se_norm + pmc_a_se + temp_max_se  + renda_r + massa_r + temp_min_se + pmc_r_se + pop_ocup_br + du, data = train_set, family = "gaussian")
yPrevisao <- predict(fit,test_set)
sqrt(mean((yPrevisao - test_set$ind_se_norm)^2))



##### treinando as variaveis com bootstrap melhor modelo que parece ser com todas as variaveis , com renda
dadosSE_previsao <- dadosSE %>% filter(!is.na(ind_se), !is.na(massa_r))
dadosSE_previsao <- dadosSE_previsao[-1]
dadosSE_previsao$ind_se_norm <- normalize(dadosSE_previsao$ind_se)
dadosSE_previsao <- dadosSE_previsao[-1]

test_index <- createDataPartition(dadosSE_previsao$ind_se_norm, times = 1, p = 0.3, list = FALSE)

train_set <- dadosSE_previsao %>% slice(-test_index)  #train_set
test_set <- dadosSE_previsao %>%  slice(test_index)    #test_set

# GLM
train_glm <- train(ind_se_norm ~., method = "glm", data = train_set)
Y_glm <- predict(train_glm,test_set, type = "raw")

# KNN
col_index <- (1:(ncol(train_set)-1))

control <- trainControl(method = "cv", number = 10, p = .8)
train_knn <- train(train_set[,col_index], train_set$ind_se_norm, method = "knn",
                   tuneGrid = data.frame(k = seq(1, 20, 1)), data = train_set,
                   trControl = control)
ggplot(train_knn, highlight = TRUE)
fit_knn <- knn3(train_set[,col_index], as.factor(train_set$ind_se_norm), k = 4)
Y_knn <- predict(fit_knn, test_set[,col_index], type = "class")
cm <- confusionMatrix(
  table(factor(Y_knn, levels=min(test_set):max(test_set)), 
        factor(test_set$ind_se_norm, levels=min(test_set):max(test_set))))

sqrt(mean((as.numeric(as.character(Y_knn)) - test_set$ind_se_norm)^2))

train_knn$bestTune

control <- trainControl(method = "cv", number = 10, p = .9)

train_knn <- train(ind_se_norm ~., method = "knn",
                   tuneGrid = data.frame(k = seq(2, 60, 2)),
                   data = train_set,
                   trControl = control
                   )
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

fit_knn <- knn3(., ind_se_norm,  k = 4)
ggplot(train_knn, highlight = TRUE)

Y_knn <- predict(train_knn, test_set, type = "raw")
sqrt(mean((Y_knn - test_set$ind_se_norm)^2))

train_knn <- train(ind_se_norm  ~ pim_se + massa_r + pmc_a_se, method = "knn",
                   tuneGrid = data.frame(k = seq(2, 60, 2)),
                   data = train_set,
                   trControl = control
)
ggplot(train_knn, highlight = TRUE)

grid <- expand.grid(span = seq(0.3, 0.8, len = 9), degree = 1)

train_loess <- train(ind_se_norm ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = train_set)
ggplot(train_loess, highlight = TRUE)
train_loess$bestTune

#####

control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <-  train(train_set[, col_index], train_set$ind_se_norm, 
                   method = "rf", 
                   ntree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

fit_rf <- randomForest(train_set[, col_index], train_set$ind_se_norm, 
                       minNode = train_rf$bestTune$mtry)
y_rf <- predict(fit_rf, test_set[ ,col_index])
cm <- confusionMatrix(factor(as.numeric(y_rf), levels =as.numeric(y_rf)), factor(test_set$ind_se_norm, levels =as.numeric(y_rf) ))
sqrt(mean((y_rf - test_set$ind_se_norm)^2))


cm$overall["Accuracy"]

