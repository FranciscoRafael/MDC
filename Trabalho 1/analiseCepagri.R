#set workdir
setwd("C:/Users/Vanessa Carneiro/Documents/MDC/Trabalho 1/")

#install.packages("rlang")
#install.packages("ggplot2")

library(ggplot2)



consecutivo <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  
  return(result)
}




#leitura dos dados e delimitação das colunas com seus respectivos nomes. 
names <- c('horario', 'temp', 'vento', 'umid', 'sensacao')
cepagri <- read.csv('cepagri.csv', header = FALSE, fill = TRUE, 
                       sep = ';', col.names = names)

summary(cepagri)



#remocao dos dados NA para sensacao termica
nrow(cepagri)
cepagri <- cepagri[!is.na(cepagri$sensacao), ]
nrow(cepagri)


#outras colunas todas estão completas

class(cepagri$temp)
class(cepagri$horario)
class(cepagri$vento)
class(cepagri$umid)
class(cepagri$sensacao)

cepagri$temp <- as.numeric(cepagri$temp)
class(cepagri$temp)

#o ano inicial e final são 2014 e 2022 respectivamente
#ambos anos estão incompletos e vão ser retirados
cepagri[1, ]
cepagri[nrow(cepagri), ]


head(cepagri$horario)


cepagri$horario <- as.POSIXlt(cepagri$horario, format = '%d/%m/%Y-%H:%M')
#cepagri[, 1] <- as.POSIXct(cepagri[, 1], format = '%d/%m/%Y-%H:%M', tz = "America/Sao_Paulo")

head(cepagri$horario)
#summary(cepagri)

cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1
summary(cepagri)


#removendo 2014 e 2022
cepagri <- cepagri[!(cepagri$ano == 2014 | cepagri$ano == 2022),]

head(cepagri)
tail(cepagri)




#################################################################
#### remoacao de dados repetidos


################ temperaura ##############################


#2 dias: 
#any(consecutivo(cepagri$temp, 288)) #2 dias
#any(consecutivo(cepagri$temp, 720)) #5 dias
#any(consecutivo(cepagri$temp, 1440)) #10 dias
#any(consecutivo(cepagri$temp, 1584)) #11 dias


#selecionando a cada 24 horas
filtro_consecutivo_temp <- consecutivo(cepagri$temp, 144)

length(unique(as.Date(cepagri[filtro_consecutivo_temp , 1])))

head(cepagri[filtro_consecutivo_temp, ], 10)
tail(cepagri[filtro_consecutivo_temp, ], 10)

#########################################################



############### Vento ##################################
#any(consecutivo(cepagri$vento, 144)) #2 dias
#any(consecutivo(cepagri$vento, 720)) #5 dias
#any(consecutivo(cepagri$vento, 1440)) #10 dias
#any(consecutivo(cepagri$vento, 1584)) #11 dias
#any(consecutivo(cepagri$vento, 2880)) #20 dias
#any(consecutivo(cepagri$vento, 4320)) #30 dias
#any(consecutivo(cepagri$vento, 5760)) #40 dias
#any(consecutivo(cepagri$vento, 7056)) #49 dias # FALSE


#Selecionando a cada 24 horas
filtro_consecutivo_vento <- consecutivo(cepagri$vento, 144)

length(unique(as.Date(cepagri[filtro_consecutivo_vento , 1])))


summary(cepagri[filtro_consecutivo_vento, ])
head(cepagri[filtro_consecutivo_vento, ], 10)
tail(cepagri[filtro_consecutivo_vento, ], 10)



######################umid##################################
#any(consecutivo(cepagri$umid, 144)) #2 dias
#any(consecutivo(cepagri$umid, 720)) #5 dias
#any(consecutivo(cepagri$umid, 1440)) #10 dias
#any(consecutivo(cepagri$umid, 1584)) #11 dias


#Selecionando a cada 24 horas
filtro_consecutivo_umid <- consecutivo(cepagri$umid, 144)

length(unique(as.Date(cepagri[filtro_consecutivo_umid , 1])))

summary(cepagri)

head(cepagri[filtro_consecutivo_umid, ], 10)
tail(cepagri[filtro_consecutivo_umid, ], 10)



######################sensacao###############################

#any(consecutivo(cepagri$sensacao, 144)) #2 dias
#any(consecutivo(cepagri$sensacao, 720)) #5 dias
#any(consecutivo(cepagri$sensacao, 1440)) #10 dias
#any(consecutivo(cepagri$sensacao, 1584)) #11 dias

#Selecionando a cada 24 horas. 
filtro_consecutivo_sensa <- consecutivo(cepagri$sensacao, 144)

length(unique(as.Date(cepagri[filtro_consecutivo_sensa , 1])))

summary(cepagri)

##############################################################

####################horario###################################


cepagri$datanumeric <- as.numeric(cepagri$horario)

summary(cepagri)


#any(consecutivo(cepagri$datanumeric, 2)) 
#any(consecutivo(cepagri$datanumeric, 10)) 
#any(consecutivo(cepagri$datanumeric, 52)) 


filtro_consecutivo_horario <- consecutivo(cepagri$datanumeric, 2)

length(unique(as.Date(cepagri[filtro_consecutivo_horario , 1])))
unique(as.Date(cepagri[filtro_consecutivo_horario , 1]))



####################################
# removendo os dados com valores invalidos
# sensacao termica

summary(cepagri$sensacao)
length(cepagri[cepagri$sensacao < 0, 5])

head(cepagri[cepagri$sensacao == 99.9, ], 10)

cepagri[cepagri$sensa == 99.9, 5] <- NA

summary(cepagri$sensacao)


####################################
##analisando os dados de umidade relativa do ar

summary(cepagri$umid)

length(cepagri[cepagri$umid <= 10, 4])
length(cepagri[cepagri$umid == 100, 4])

#ideia e remover todos abaixo de 8%

head(cepagri[cepagri$umid < 10, ], 10)

head(cepagri[cepagri$umid < 8, ], 10)

head(cepagri[cepagri$umid < 5, ], 10)

cepagri[cepagri$umid < 5, 4] <- NA


summary(cepagri)


#####################################################


cepagri$datanumeric <- NULL
summary(cepagri)

#verificando as datas dos valores consecutivos para cada variavel
length(unique(as.Date(cepagri[filtro_consecutivo_temp , 1])))
length(unique(as.Date(cepagri[filtro_consecutivo_vento , 1])))
length(unique(as.Date(cepagri[filtro_consecutivo_umid , 1])))
length(unique(as.Date(cepagri[filtro_consecutivo_sensa , 1])))
length(unique(as.Date(cepagri[filtro_consecutivo_horario , 1])))


summary(unique(as.Date(cepagri[filtro_consecutivo_temp , 1])))
summary(unique(as.Date(cepagri[filtro_consecutivo_vento , 1])))
summary(unique(as.Date(cepagri[filtro_consecutivo_umid , 1])))
summary(unique(as.Date(cepagri[filtro_consecutivo_sensa , 1])))
summary(unique(as.Date(cepagri[filtro_consecutivo_horario , 1])))


##############################################################
## analise por ano consecutivo sem filtrar


### 2015
cepagri2015 <- cepagri[cepagri$ano == 2015, ]
### 2015 temp
tapply(cepagri2015$temp, cepagri2015$mes, function(x){mean(consecutivo(x, 30))})
### 2015 temp
tapply(cepagri2015$vento, cepagri2015$mes, function(x){mean(consecutivo(x, 30))})
### 2015 umid
tapply(cepagri2015$umid, cepagri2015$mes, function(x){mean(consecutivo(x, 30))})
### 2015 sensacao
tapply(cepagri2015$sensacao, cepagri2015$mes, function(x){mean(consecutivo(x, 30))})




### 2016
cepagri2016 <- cepagri[cepagri$ano == 2016, ]
### 2015 temp
tapply(cepagri2016$temp, cepagri2016$mes, function(x){mean(consecutivo(x, 30))})
### 2015 temp
tapply(cepagri2016$vento, cepagri2016$mes, function(x){mean(consecutivo(x, 30))})
### 2015 umid
tapply(cepagri2016$umid, cepagri2016$mes, function(x){mean(consecutivo(x, 30))})
### 2015 sensacao
tapply(cepagri2016$sensacao, cepagri2016$mes, function(x){mean(consecutivo(x, 30))})



### 2017
cepagri2017 <- cepagri[cepagri$ano == 2017, ]
### 2017 temp
tapply(cepagri2017$temp, cepagri2017$mes, function(x){mean(consecutivo(x, 30))})
### 2017 temp
tapply(cepagri2017$vento, cepagri2017$mes, function(x){mean(consecutivo(x, 30))})
### 2017 umid
tapply(cepagri2017$umid, cepagri2017$mes, function(x){mean(consecutivo(x, 30))})
### 2017 sensacao
tapply(cepagri2017$sensacao, cepagri2017$mes, function(x){mean(consecutivo(x, 30))})





### 2018
cepagri2018 <- cepagri[cepagri$ano == 2018, ]
### 2018 temp
tapply(cepagri2018$temp, cepagri2018$mes, function(x){mean(consecutivo(x, 30))})
### 2018 temp
tapply(cepagri2018$vento, cepagri2018$mes, function(x){mean(consecutivo(x, 30))})
### 2018 umid
tapply(cepagri2018$umid, cepagri2018$mes, function(x){mean(consecutivo(x, 30))})
### 2018 sensacao
tapply(cepagri2018$sensacao, cepagri2018$mes, function(x){mean(consecutivo(x, 30))})


### 2019
cepagri2019 <- cepagri[cepagri$ano == 2019, ]
### 2019 temp
tapply(cepagri2019$temp, cepagri2019$mes, function(x){mean(consecutivo(x, 30))})
### 2019 temp
tapply(cepagri2019$vento, cepagri2019$mes, function(x){mean(consecutivo(x, 30))})
### 2019 umid
tapply(cepagri2019$umid, cepagri2019$mes, function(x){mean(consecutivo(x, 30))})
### 2019 sensacao
tapply(cepagri2019$sensacao, cepagri2019$mes, function(x){mean(consecutivo(x, 30))})


### 2020
cepagri2020 <- cepagri[cepagri$ano == 2020, ]
### 2020 temp
tapply(cepagri2020$temp, cepagri2020$mes, function(x){mean(consecutivo(x, 30))})
### 2020 temp
tapply(cepagri2020$vento, cepagri2020$mes, function(x){mean(consecutivo(x, 30))})
### 2020 umid
tapply(cepagri2020$umid, cepagri2020$mes, function(x){mean(consecutivo(x, 30))})
### 2020 sensacao
tapply(cepagri2020$sensacao, cepagri2020$mes, function(x){mean(consecutivo(x, 30))})


### 2021
cepagri2021 <- cepagri[cepagri$ano == 2021, ]
### 2021 temp
tapply(cepagri2021$temp, cepagri2021$mes, function(x){mean(consecutivo(x, 30))})
### 2021 temp
tapply(cepagri2021$vento, cepagri2021$mes, function(x){mean(consecutivo(x, 30))})
### 2021 umid
tapply(cepagri2021$umid, cepagri2021$mes, function(x){mean(consecutivo(x, 30))})
### 2021 sensacao
tapply(cepagri2021$sensacao, cepagri2021$mes, function(x){mean(consecutivo(x, 30))})



#removendo todos os dados repetidos.

summary(cepagri)
cepagri[filtro_consecutivo_temp, 2] <- NA
cepagri[filtro_consecutivo_vento, 3] <- NA
cepagri[filtro_consecutivo_umid, 4] <- NA
cepagri[filtro_consecutivo_sensa, 5] <- NA
cepagri[filtro_consecutivo_horario, 1] <- NA

summary(cepagri$temp)
summary(cepagri$vento)
summary(cepagri$umid)
summary(cepagri$sensacao)
summary(cepagri$horario)


#removendo os NAs para fins de analise. 

nrow(cepagri)
cepagri_wtNA <- cepagri[!is.na(cepagri$horario), ]
cepagri_wtNA <- cepagri[!is.na(cepagri$vento), ]
cepagri_wtNA <- cepagri[!is.na(cepagri$umid), ]
cepagri_wtNA <- cepagri[!is.na(cepagri$sensacao), ]
cepagri_wtNA <- cepagri[!is.na(cepagri$temp), ]
nrow(cepagri_wtNA)

summary(cepagri_wtNA)

##df para cada ano
cepagri2015 <- cepagri_wtNA[cepagri_wtNA$ano == 2015, ]
cepagri2016 <- cepagri_wtNA[cepagri_wtNA$ano == 2016, ]
cepagri2017 <- cepagri_wtNA[cepagri_wtNA$ano == 2017, ]
cepagri2018 <- cepagri_wtNA[cepagri_wtNA$ano == 2018, ]
cepagri2019 <- cepagri_wtNA[cepagri_wtNA$ano == 2019, ]
cepagri2020 <- cepagri_wtNA[cepagri_wtNA$ano == 2020, ]
cepagri2021 <- cepagri_wtNA[cepagri_wtNA$ano == 2021, ]


summary(cepagri2015)
summary(cepagri2016)
summary(cepagri2017)
summary(cepagri2018)
summary(cepagri2019)
summary(cepagri2020)
summary(cepagri2021)



library(ggplot2)
cepagri2015$mes <- as.factor(cepagri2015$mes)
cepagri2016$mes <- as.factor(cepagri2016$mes)
cepagri2017$mes <- as.factor(cepagri2017$mes)
cepagri2018$mes <- as.factor(cepagri2018$mes)
cepagri2019$mes <- as.factor(cepagri2019$mes)
cepagri2020$mes <- as.factor(cepagri2020$mes)
cepagri2021$mes <- as.factor(cepagri2021$mes)



ggplot(cepagri2015, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2016, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2017, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2018, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2019, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2020, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)
ggplot(cepagri2021, aes(x = mes, y = temp, group = mes)) + geom_point(alpha = 0.01)

