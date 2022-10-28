#set workdir
setwd("C:/Users/franc/OneDrive/Documentos/MDC - Unicamp/Trabalho 1")

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
any(consecutivo(cepagri$temp, 288)) #2 dias
any(consecutivo(cepagri$temp, 720)) #5 dias
any(consecutivo(cepagri$temp, 1440)) #10 dias
any(consecutivo(cepagri$temp, 1584)) #11 dias

filtro_consecutivo_temp <- consecutivo(cepagri$temp, 144)

length(unique(as.Date(cepagri[filtro_consecutivo_temp , 1])))

summary(cepagri)

#########################################################




############### Vento ##################################
any(consecutivo(cepagri$vento, 144)) #2 dias
any(consecutivo(cepagri$vento, 720)) #5 dias
any(consecutivo(cepagri$vento, 1440)) #10 dias
any(consecutivo(cepagri$vento, 1584)) #11 dias
any(consecutivo(cepagri$vento, 2880)) #20 dias
any(consecutivo(cepagri$vento, 4320)) #30 dias
any(consecutivo(cepagri$vento, 5760)) #40 dias
any(consecutivo(cepagri$vento, 7056)) #49 dias # FALSE


#Selecionando a cada 2 horas. 
filtro_consecutivo_vento <- consecutivo(cepagri$vento, 72)

length(unique(as.Date(cepagri[filtro_consecutivo_vento , 1])))

summary(cepagri)


######################umid##################################
any(consecutivo(cepagri$umid, 144)) #2 dias
any(consecutivo(cepagri$umid, 720)) #5 dias
any(consecutivo(cepagri$umid, 1440)) #10 dias
any(consecutivo(cepagri$umid, 1584)) #11 dias


#Selecionando a cada 8 horas. 
filtro_consecutivo_umid <- consecutivo(cepagri$umid, 72)

length(unique(as.Date(cepagri[filtro_consecutivo_umid , 1])))

summary(cepagri)



######################sensacao###############################

any(consecutivo(cepagri$sensacao, 144)) #2 dias
any(consecutivo(cepagri$sensacao, 720)) #5 dias
any(consecutivo(cepagri$sensacao, 1440)) #10 dias
any(consecutivo(cepagri$sensacao, 1584)) #11 dias

#Selecionando a cada 3 horas. 
filtro_consecutivo_sensa <- consecutivo(cepagri$sensacao, 72)

length(unique(as.Date(cepagri[filtro_consecutivo_sensa , 1])))

summary(cepagri)


####################################
# removendo os dados com valores invalidos
# sensacao termica

summary(cepagri$sensacao)
length(cepagri[cepagri$sensa < 0, 5])

head(cepagri[cepagri$sensa == 99.9, ], 10)

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

