#install.packages("tstools")
library(tstools)
library(writexl)
#Procesamiento EMAE.

emae.file <- paste(tempfile(), ".csv", sep = "")
download.file("https://infra.datos.gob.ar/catalog/sspm/dataset/143/distribution/143.3/download/emae-valores-anuales-indice-base-2004-mensual.csv", emae.file, mode = "wb")
emae <- read.csv(emae.file)
emae <- emae[c(1,3)]
emae_num <- as.numeric(emae$emae_desestacionalizada)
emae_num <- ts(emae_num, start = c(2004, 01), end = c(2020, 07), frequency = 12)
emae_num <- window(emae_num, end = c(2019, 12))
#plot(emae_num)
emae <- emae_num[complete.cases(emae_num)] # delete NAs
emae <- 100* log(emae)

anio <-2009

d2009_1<- create_dummy_ts(end_basic = c(anio,06), dummy_start = c(anio,03), dummy_end = c(anio,06), sp = TRUE, start_basic = c(2004,1),  dummy_value = -1, frequency = 12)
d2009_2<- create_dummy_ts(end_basic = c(2019,12), dummy_start = c(anio,08), dummy_end = c(anio,8), sp = TRUE, start_basic = c(anio,7),  dummy_value = 1, frequency = 12)
d2009_2[1] = 1
d2009 <- ts(c(d2009_1,d2009_2),  start = start(d2009_2),frequency = frequency(121))
d2009 <- ts(d2009, start = c(2004, 01), end = c(2019, 12), frequency = 12)

anio <-2012

d2012_1<- create_dummy_ts(end_basic = c(anio,06), dummy_start = c(anio,03), dummy_end = c(anio,06), sp = TRUE, start_basic = c(2004,1),  dummy_value = -1, frequency = 12)
d2012_2<- create_dummy_ts(end_basic = c(2019,12), dummy_start = c(anio,08), dummy_end = c(anio,8), sp = TRUE, start_basic = c(anio,7),  dummy_value = 1, frequency = 12)
d2012_2[1] = 1
d2012 <- ts(c(d2012_1,d2012_2),  start = start(d2012_2),frequency = frequency(121))
d2012 <- ts(c(d2012_1,d2012_2),  start = start(d2012_2),frequency = frequency(121))
d2012 <- ts(d2012, start = c(2004, 01), end = c(2019, 12), frequency = 12)

anio <-2018

d2018_1<- create_dummy_ts(end_basic = c(anio,06), dummy_start = c(anio,03), dummy_end = c(anio,06), sp = TRUE, start_basic = c(2004,1),  dummy_value = -1, frequency = 12)
d2018_2<- create_dummy_ts(end_basic = c(2019,12), dummy_start = c(anio,08), dummy_end = c(anio,8), sp = TRUE, start_basic = c(anio,7),  dummy_value = 1, frequency = 12)
d2018_2[1] = 1
d2018 <- ts(c(d2018_1,d2018_2),  start = start(d2018_2),frequency = frequency(121))
d2018 <- ts(d2018, start = c(2004, 01), end = c(2019, 12), frequency = 12)

dlog.emae.reg <- lm(emae ~ d2009 + d2012 + d2018)
dlog.emae.adj <- dlog.emae.reg$residuals

dlog_residuos <- as.numeric(dlog.emae.adj)
dlog_residuos <- ts(dlog_residuos, start = c(2004, 01), end = c(2019, 12), frequency = 12)

#DATOS INFLACIoN
#Traemos IPC base 2020 generado en ejercitaciÃ³n 1.
source("PS1_Data.R")

#Construimos variables como pide la consigna.

pc = window(pc, start = c(2003, 12), end = c(2019, 12))
pi = 100 * diff(log(pc))

#SERIE BADLAR

badlar.file <- paste(tempfile(), ".xls", sep = "")
download.file("http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/pashis.xls", badlar.file, mode = "wb")

badlar <- read_excel(badlar.file, skip = 793, sheet = 1)
badlar <- as.matrix(badlar)
badlar <- badlar[, c(1, 2, 12)]
badlar <- as.numeric(badlar[, 3])
badlar <- badlar[complete.cases(badlar)]

#badlar <- badlar[which(badlar != 0, arr.ind = TRUE)]
badlar <- ts(badlar, start = c(1999, 1), frequency = 12)
badlar <- window(badlar, start = c(2004, 1), end = c(2019, 12))

i = 100 * log(1+ badlar/1200)

#Primeras diferencias.

#EMAE
emae_dif = diff(dlog_residuos)
#plot(emae_dif)

#Inflacion
pi_dif = diff(pi)
#plot(pi_dif)

#Tasa de interes nominal.
i_dif = diff(i)
#plot(i_dif)

#Tasa real mensual.
r = i- pi
#plot(r)

#Test de raiz unitaria de Dickey Fuller
#library(urca)
#Para las variables en diferencias
#i_dif_df <- ur.df(y = i_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***
#pi_dif_df <- ur.df(y = pi_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***
#emae_dif_df <- ur.df(y = emae_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***

#Para las variables sin estar en diferencias.
#r_df <- ur.df(y = r, type = "drift", lags = 2, selectlags = "Fixed") # ***
#i_df <- ur.df(y = i, type = "drift", lags = 2, selectlags = "Fixed") # ***
#emae_df <- ur.df(y = emae, type = "drift", lags = 2, selectlags = "Fixed") # ***
#pi_df <- ur.df(y = pi, type = "drift", lags = 2, selectlags = "Fixed") # ***


# Remove variables
remove(anio, badlar.file, dlog.emae.adj, d2009, d2009_1, d2009_2, d2012, d2012_1, d2012_2, d2018, d2018_1, d2018_2)


#Completar con los summary, que son las salidas.
#summary(r_df)

Y <- cbind(i_dif, pi_dif, emae_dif)


#Datos para Punto 4.
#Primeras diferencias del EMAE - su media.
dy = emae_dif - mean (emae_dif)

#Primeras diferencias de la tasa de interés nominal - su media.
di = i_dif - mean (i_dif)

#Tasa de interes real - media
ri = r - mean (r)
ri = window(ri, start = c(2004, 2))

#Standard deviations 
sd_dy <- sd(dy)/10
sd_di <- sd(di)/10
sd_ri <- sd(ri)/10

output <- data.frame(dy, ri, di)


write_xlsx(output, "Variables.xlsx")
