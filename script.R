#Impotar dados

library(pastecs)

# Estatística Descritiva

desc <- stat.desc(dados)
descri_price <- stat.desc(dados[,2])
descri_price <- format(descri_price, scientific= FALSE, digits=2, nsmall=2)

# Gráficos
# Colunas (premium)

cont <- table(dados$premium)
barplot(cont, main = "Gráfico de Barras", xlab = "Premium?", ylab = "Frequência")

# Pizza
pie(cont, labels = c("Não", "Sim"), main="Gráfico de Pizza")

# Dispersão
plot(x = dados$hd, y = dados$price, pch=19)

scatter.smooth(dados$hd, dados$price, pch=19)


# Distribuição de frequencia "price"

##Quantidade de classes
sturges <- 1+3.3*log10(nrow(dados))

##Tamanho
range <- max(dados$price) - min(dados$price)

##Amplitude
amp <- range / sturges

## Arredondar para miltiplo de 5
amp5 <- 5 * round(amp/5)

xmin <- 5*round(min(dados$price)/5)-5

xmax <- 5*round(max(dados$price)/5)

limites <- seq(xmin, xmax+amp5, by=amp5)

cortes_aux <- cut(dados$price, limites, right = FALSE, dig.lab = 4)

price_freq <- as.data.frame(cbind(table(cortes_aux)))

sum(price_freq)

for(i in 1:nrow(price_freq)){
  price_freq[i, 2] <- round(price_freq[i, 1]/sum(price_freq[,1]), digits = 4)
}

colnames(price_freq) <- c("f", "f_rel")



##### Normalidade

hist(dados$price, prob=TRUE)
curve(dnorm(x, mean = mean(dados$price), sd = sd(dados$price)), add = TRUE)


### Regressão Linear

# Simples HD x Price
scatter.smooth(x = dados$hd, y = dados$price, pch=19)
r <- cor(x=dados$hd, y=dados$price)
reg = lm(dados$price~dados$hd)
summary(reg)

reg_multi <- lm(dados$price ~ dados$speed + dados$hd + dados$ram + dados$screen)
summary(reg_multi)


## Predição 1° dado
ref <- dados$price[1]

pred_simples <- predict(reg, dados)[1]
pred_multi <- predict(reg_multi, dados)[1]

library(caret)
library(randomForest)
dados <- as.data.frame(dados)
random_index <- createDataPartition(dados[,2], p=0.7, list=FALSE)
dados_treino <- dados[random_index,]
dados_teste <- dados[-random_index,]

dados_treino$multi <- factor(dados_treino$multi)
dados_treino$cd <- factor(dados_treino$cd)

dados_teste$multi <- factor(dados_teste$multi)
dados_teste$cd <- factor(dados_teste$cd)

rf_model <- randomForest(factor(premium) ~ ., data=dados_treino, controls = cforest_unbiased(ntree=1000, mtr=2))
rf_predict <- predict(rf_model, dados_teste)
ref_tab_acuraria <- table(rf_predict, dados_teste$premium)
rf_acuraccy <- 1-((ref_tab_acuraria[2,1]+ref_tab_acuraria[1,2])/nrow(dados_teste))
