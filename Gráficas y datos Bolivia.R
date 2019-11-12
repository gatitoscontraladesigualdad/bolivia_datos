########################################################
#Datos socioeconómicos de Bolivia 2005-2018 (circa)
#Elaborado por: Gatitos Contra la Desigualdad
#Fecha: 11/10/2019
########################################################
rm(list = ls())

### Instalar una paquetería que facilita apertura de paqueterías: pacman
#install.packages("pacman") 
library(pacman)

# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest, viridis)
library("wesanderson")
names(wes_palettes)
#install.packages('ghibli')
library("ghibli")
# display palettes w/ names
#par(mfrow=c(9,3))
#for(i in names(ghibli_palettes)) print(ghibli_palette(i))

### Setup ----
options(scipen=999) # Prevenir notación científica
#-----------------

###Carpeta de trabajo y carga de datos
setwd("~/Documents/Data Science/Repos/2019B/Gatitos Contra La Desigualdad/bolivia_datos")
#Fuentes: CEPAL: https://estadisticas.cepal.org/cepalstat/Portada.html?idioma=spanish
        #Banco Mundial: https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD

#Datos
indic_bol<- read.csv(file = "indicadores_bolivia.csv", sep=",") 

    
    #Función para determinar theme de las gráficas
    theme_map <- function(...) {
      theme_minimal() +
        theme(
          text = element_text(family = "Verdana",
                              color = "#939486"),
          # remove all axes
          axis.line = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          # add a subtle grid
          panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
          panel.grid.minor = element_blank(),
          # background colors
          plot.background = element_rect(fill = "#F5F5F3",
                                         color = NA),
          panel.background = element_rect(fill = "#F5F5F3",
                                          color = NA),
          legend.background = element_rect(fill = "#F5F5F3",
                                           color = NA),
          # borders and margins
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.border = element_blank(),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          # titles
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 9,hjust = 1,
                                     color = "#939486"),
          plot.title = element_text(size = 15, hjust = 0.5,
                                    color = "#4B4C47"),
          plot.subtitle = element_text(size = 10, hjust = 0.5,
                                       color = "#939486",
                                       margin = margin(b = -0.1,
                                                       t = -0.1,
                                                       l = 2,
                                                       unit = "cm"),
                                       debug = F),
          # captions
          plot.caption = element_text(size = 7,
                                      hjust = .5,
                                      margin = margin(t = 0.2,
                                                      b = 0,
                                                      unit = "cm"),
                                      color = "#939184"),
          ...
        )
    }
  
    
####
####PIB por persona
####
indic_bol %>%
  ggplot() +
    geom_line(mapping = aes(x = Año, y = gdp_pc_crec, color=País))+
    scale_colour_viridis_d(option = "viridis",alpha = 0.8, begin = 0.0, end = 0.9,direction = -1) +
  # Agregar títulos
  labs(x = "Años",
       y = NULL,
       title = "% crecimiento anual PIB por persona",
       subtitle = NULL,
       caption = "Fuente: Elaborado co nuestras garritas de 
       Gatitos contra la desigualdad, con datos de BM.") +
    theme_map()
    
#Guardar el mapa
ggsave("PIB por persona_países.png", width = 7)



####
####GDP
####
indic_bol %>%
  filter(País %in% c("Bolivia")) %>%
  ggplot() +
  geom_line(mapping = aes(x = Año, y = gdp), size = 2)+
  scale_fill_manual(values=c("#4B4C47" )) +
  # Agregar títulos
  labs(x = "Años",
       y = NULL,
       title = "PIB por persona en Bolivia",
       subtitle = "PPP (constantes a 2011 international $)",
       caption = "Fuente: Elaborado co nuestras garritas de 
       Gatitos contra la desigualdad, con datos de BM.") +
  theme_map()

#Guardar el mapa
ggsave("PIB por persona_Bolivia.png", width = 7)


#Función para determinar theme de las gráficas
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      axis.line = element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9,hjust = 1,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}



####
####Gini
####
indic_bol %>%
  ggplot() +
  #geom_line(mapping = aes(x = Año, y = gini, color=País))+
  geom_smooth(mapping = aes(x = Año, y = gini, color=País),se = FALSE)+ 
  scale_colour_viridis_d(option = "viridis",alpha = 0.8, begin = 0.0, end = 0.9,direction = -1) +
  # Agregar títulos
  labs(x = "Años",
       y = NULL,
       title = "Coeficiente de Gini",
       subtitle = "de la desigualdad de ingresos",
       caption = "Fuente: Elaborado co nuestras garritas de 
       Gatitos contra la desigualdad, con datos de CEPAL.") +
  scale_y_continuous(breaks = seq(0,0.6,0.02)) +
  scale_x_continuous(breaks = seq(2005,2018,2)) +
  theme_map()

#Guardar el mapa
ggsave("Gini.png", width = 7)


####
####Pobreza
####
indic_bol %>%
  ggplot() +
  #geom_line(mapping = aes(x = Año, y = gini, color=País))+
  geom_smooth(mapping = aes(x = Año, y = pob_ext_nac, color=País),se = FALSE)+ 
  scale_colour_viridis_d(option = "viridis",alpha = 0.8, begin = 0.0, end = 0.9,direction = -1) +
  # Agregar títulos
  labs(x = "Años",
       y = NULL,
       title = "Pobreza extrema",
       subtitle = "% de población",
       caption = "Fuente: Elaborado co nuestras garritas de 
       Gatitos contra la desigualdad, con datos de CEPAL.") +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_x_continuous(breaks = seq(2005,2018,2)) +
  theme_map()

#Guardar el mapa
ggsave("Pobreza.png", width = 7)




####
####Esperanza de vida
####
indic_bol %>%
  ggplot() +
  #geom_line(mapping = aes(x = Año, y = gini, color=País))+
  geom_smooth(mapping = aes(x = Año, y = esperanza_de_vida, color=País),se = FALSE)+ 
  scale_colour_viridis_d(option = "viridis",alpha = 0.8, begin = 0.0, end = 0.9,direction = -1) +
  # Agregar títulos
  labs(x = "Años",
       y = NULL,
       title = "Esperanza de vida",
       subtitle = "Años, al nacer",
       caption = "Fuente: Elaborado co nuestras garritas de 
       Gatitos contra la desigualdad, con datos de CEPAL.") +
  scale_y_continuous(breaks = seq(40,80,2)) +
  scale_x_continuous(breaks = seq(2005,2018,2)) +
  theme_map()

#Guardar el mapa
ggsave("Esperanza.png", width = 7)

