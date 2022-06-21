install.packages(ggplot2)
install.packages(naniar)
install.packages(corrplot)
install.packages(gridExtra)
install.packages(cowplot)
install.packages(reshape2)


dados = read.csv("carprice.csv")
pairs(dados, price + wheelbase)
pairs(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg, data = dados)
cor(dados)
dados_numeric = dados[,sapply(dados, is.numeric)]
pairs(dados_numeric)
cor(dados_numeric)
dim(dados_numeric)
is.na(dados_numeric)
colSums(is.na(dados_numeric))
gg_miss_var(train[,colSums(is.na(train)) > 0])
