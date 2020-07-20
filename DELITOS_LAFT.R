#Análisis de estadística descriptiva a los delitos LAFT
dfd_laft <- read.table("C:/Users/deant/Desktop/df_delitos_laft.csv", header = TRUE, sep = ",", fill = TRUE, encoding = "UTF-8", na.strings = "NA")
View(dfd_laft)

#Frecuencias de la cantidad de delitos LAFT por departamento desde 1970 a febrero de 2020

fi_d <- table(dfd_laft$DEPARTAMENTO)
fri_d <- prop.table(fi_d)
Fi_d <- cumsum(fi_d)
FRAi_d <- cumsum(fri_d)
tabla_frecuencias_d <- cbind(fi_d, fri_d, Fi_d, FRAi_d)
data6 <- tabla_frecuencias_d[-1,]
View(data6)

#Gráfico de la frecuencia por departamentos

library(plotly)

frecuencies_dfd_departamentos <- data.frame("Categorie"=rownames(data6), data6)
data6 <- frecuencies_dfd_departamentos[,c('Categorie', 'fri_d')]

fig_dfd_dpto <- plot_ly(data6, labels = ~Categorie, values = ~fri_d, type = 'pie')
fig_dfd_dpto <- fig_dfd_dpto %>% layout(title = 'Delitos LAFT por departamento desde 1970 a febrero de 2020',
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_dfd_dpto

#Frecuencias de la cantidad de delitos LAFT por municipio desde 1970 a febrero de 2020

fi_m <- table(dfd_laft$MUNICIPIO)
fri_m <- prop.table(fi_m)
Fi_m=cumsum(fi_m)
FRAi_m=cumsum(fri_m)
tabla_frecuencias_m= cbind(fi_m, fri_m, Fi_m, FRAi_m)
data7 <- as.data.frame(tabla_frecuencias_m[-1,])
View(data7)

frecuencies_dfd_m<- data.frame("Categorie"=rownames(data7), data7)
data7 <- frecuencies_dfd_m[,c('Categorie', 'fri_m')]

fig_dfd_municipio <- plot_ly(data7, labels = ~Categorie, values = ~fri_m, type = 'pie')
fig_dfd_municipio <- fig_dfd_municipio %>% layout(title = 'Delitos por municipio desde 1970 a febrero de 2020',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_dfd_municipio

#dada la poca información que arroja se realiza de los 54 primeros municipios con participación superior a 0.1% del total
#se realiza sólo para la visualización y no para el tratamiento de los datos total.
data7 <- data.frame("Categorie"=rownames(data7), data7)
data7_principales <- filter(data7, fri_m > 0.003)
View(data7_principales)

data7_principales <- data7_principales[,c('Categorie', 'fri_m')]

fig_dfd_municipio_principales <- plot_ly(data7_principales, labels = ~Categorie, values = ~fri_m, type = 'pie')
fig_dfd_municipio_principales <- fig_dfd_municipio_principales %>% layout(title = 'Delitos por municipio desde 1970 a febrero de 2020',
                                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_dfd_municipio_principales

#ahora mirar los delitos más representativos

fi_dlf <- table(dfd_laft$DELITO)
fri_dlf <- prop.table(fi_dlf)
Fi_dlf=cumsum(fi_dlf)
FRAi_dlf=cumsum(fri_dlf)
tabla_frecuencias_dlf = cbind(fi_dlf, fri_dlf, Fi_dlf, FRAi_dlf)
data8 <- tabla_frecuencias_dlf
View(data8)

#Gráfico de la frecuencia por delito
#se seleccionan para la visualización los 34 delitos con mayor participación a nivel nacional desde 1970 a febrero de 2020
data8 <- data.frame("Categorie"=rownames(data8), data8)
data8 <- filter(data8, fri_dlf > 0.0038)
data8 <- data8[,c('Categorie', 'fri_dlf')]

fig_dfd_laft <- plot_ly(data8, labels = ~Categorie, values = ~fri_dlf, type = 'pie', textinfo = 'label+percent', showlegend = FALSE)
fig_dfd_laft <- fig_dfd_laft %>% layout(title = 'Delitos por municipio desde 1970 a febrero de 2020',
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_dfd_laft

#delitos fuente de lavado de activos más representativos en Colombia.
dotchart(data8$fri_dlf)

#Histograma de los años con la mayor cantidad de delitos asociados  LAFT.

fig_año <- plot_ly(x = ~dfd_laft$ANIO_DENUNCIA, type = "histogram")
fig_año <- fig_año %>% layout(barmode = "overlay")

fig_año

#Participación de los delitos LAFT respecto a los otros delitos

AÑOS <- c("2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
Delitos_LAFT <- c(34142, 37570, 44189, 45470, 42153, 40917, 40397, 40004, 40343, 37352, 34142)
Delitos_NO_LAFT <- c(72840, 395130, 397247, 398934, 374376, 366578, 362640, 358245, 343087, 324542, 298255)
data <- data.frame(AÑOS, Delitos_LAFT, Delitos_NO_LAFT)

fig_comparativo <- plot_ly(data, x = ~AÑOS, y = ~Delitos_LAFT, type = 'bar', name = 'Delitos LAFT')
fig_comparativo <- fig %>% add_trace(y = ~Delitos_NO_LAFT, name = 'Delitos NO LAFT')
fig <- fig_comparativo %>% layout(yaxis = list(title = 'Cantidad de delitos'), barmode = 'stack')

fig

#se continua con el análisis de las variables
#Se crea una tabla de contingencia

tab_cont1 <- ftable(dfd_laft[, c("ANIO_DENUNCIA", "DELITO")], col.vars = c(1,2))
View(tab_cont1)
tab_cont1


########prueba################################################################################################################

prueba_b <- as.data.frame(table(mtcars$am, mtcars$gear, dnn = c("am", "gear")))
View(prueba_b)
prueba_b


#grafico de la prueba:

gear <- prueba_b %>%
  filter(gear %in% c("3", "4", "5")) %>%
  select(gear)
gear

am_0 <- prueba_b %>%
  filter(am %in% c("0")) %>%
  select(Freq)
am_0
am_1 <- prueba_b %>%
  filter(am %in% c("1")) %>%
  select(Freq)
am_1

gear <- c("3", "4", "5")

data <- data.frame(gear, am_0, am_1)
data

fig <- plot_ly(data, x = ~gear, y = ~Freq, type = 'bar', name = 'Automáticos')
fig <- fig %>% add_trace(y = ~Freq.1, name = 'Manuales')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

fig

########prueba################################################################################################################

#Regresión: ¿cómo la variable departamento explicaría el tipo de delito en presencia de el valor de los giros postales recibidos?

View(dfd_laft)
mod <- lm(ANIO_DENUNCIA ~ DELITO + DEPARTAMENTO + giros$departamento, data=dfd_laft)
summary(mod)
anova(mod)


mod1 <- lm(TOTAL_VICTIMAS ~ DELITO, data=dfd_laft)
summary(mod1)
anova(mod1)


#Histórico de delitos

delitos_historico <- as.data.frame(table(dfd_laft$DELITO, dfd_laft$DEPARTAMENTO, dnn = c("HIST_DELITOS", "DEPARTAMENTO")))
View(delitos_historico)
delitos_historico[1:151,2] = "NA"
delitos_historico <- na.omit(delitos_historico)
delitos_historico <- cbind(fi_d, fri_d, Fi_d, FRAi_d)
View(delitos_historico)

View(dfd_laft)
memory.limit(TRUE)
mod1 <- lm(ANIO_DENUNCIA ~ DEPARTAMENTO + MUNICIPIO + DELITO + SEXO_VICTIMA + TOTAL_VICTIMAS, data=dfd_laft)

