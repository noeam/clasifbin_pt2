rm(list = ls(all.names = TRUE))
gc()

#------------------------------- Proyecto 2  -----------------------------------

df_2014 <- read.csv("outputs/2014_final.csv", header = TRUE)
str(df_2014)
summary(df_2014)
df_fmed <- read.csv("outputs/fmed_final.csv", header = TRUE)
str(df_fmed)
summary(df_fmed)
df <- read.csv("outputs/completo.csv", header = TRUE)
str(df)
summary(df)



names <- c('ejer_5', 'ejer_1', 'ejer_act', 'tienes_obesidad', 'sexo', 'obesidad')
df_2014[,names] <- lapply(df_2014[,names] , factor)
str(df_2014)
summary(df_2014)

namesmed <- c('ejer_5', 'ejer_1', 'ejer_act','suenio_act', 'suenio_1', 'suenio_5', 'tienes_obesidad', 'sexo', 'obesidad')
df_fmed[,namesmed] <- lapply(df_fmed[,namesmed] , factor)
str(df_fmed)
summary(df_fmed)

df[,namesmed] <- lapply(df[,namesmed] , factor)
str(df)
summary(df)

#-------------------------- GRAFICAS -------------------------------------------
library(tidyverse)
library(ggpubr)

# Por sexo FMed
(bsex <- ggplot(data = df_fmed, aes(x = sexo, fill=sexo)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Participantes por sexo FMed") + 
    ylab("Cantidad") +
    xlab("Sexo") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por sexo completo
(bsex <- ggplot(data = df, aes(x = sexo, fill=sexo)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Participantes por sexo FMed") + 
    ylab("Cantidad") +
    xlab("Sexo") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por Obesidad FMed
(bsex <- ggplot(data = df_fmed, aes(x = obesidad, fill=obesidad)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Obesidad Fmed") + 
    ylab("Cantidad") +
    xlab("Estado") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por Obesidad completo
(bsex <- ggplot(data = df, aes(x = obesidad, fill=obesidad)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Obesidad completo") + 
    ylab("Cantidad") +
    xlab("Estado") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))


# Por hab ejercicio actual completo
(bsex <- ggplot(data = df, aes(x = ejer_act, fill=ejer_act)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de ejercicio actualmente") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por hab ejercicio 1 anio completo
(bsex <- ggplot(data = df, aes(x = ejer_1, fill=ejer_1)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de ejercicio hace 1 año") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por hab ejercicio 5 anio completo
(bsex <- ggplot(data = df, aes(x = ejer_5, fill=ejer_5)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de ejercicio hace 5 años") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por hab suenio actual completo (fmed)
(bsex <- ggplot(data = df_fmed, aes(x = suenio_act, fill=obesidad)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de sueño actualmente") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por hab suenio 1 anio completo (fmed)
(bsex <- ggplot(data = df_fmed, aes(x = suenio_1, fill=obesidad)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de sueño hace 1 año") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))

# Por hab suenio 5 anio completo (fmed)
(bsex <- ggplot(data = df_fmed, aes(x = suenio_5, fill=obesidad)) +
    geom_bar(stat = "count", colour = "white") +
    ggtitle("Hábitos de sueño hace 5 años") + 
    ylab("Cantidad") +
    xlab("Tipo de hábito") + 
    stat_count(geom = "text", 
               aes(label = stat(count)),
               position=position_stack(0.5),
               colour="white"))




