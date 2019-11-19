# Animaciones Colombia 
rm(list = ls())
setwd("C:/Users/W/Google Drive/Gapminder/Datos")
library(gapminder);library(gganimate);library(gghighlight);library(ggrepel)
library(ggplot2);library(dplyr);library(readr);library(tidyr)
library(ggthemes);library(scales)
theme_set(theme_bw())
#Importar datos 
data_sur <- readRDS("data_sur.rds")
dane <- readRDS("dane.rds")
escolarizacion <- readRDS("escolarizacion.rds")
movil <- readRDS("movil.rds")
pobreza <- readRDS("pobreza.rds")
text_ <- readRDS("text_.rds")

#Expectativa de vida 
lifeexp <- ggplot(data = data_sur, aes(x = pib, y = lifexp, color = Pais, label = Pais)) + 
  geom_point(aes(size = pop)) + 
  #Agregar letras a los dots
  geom_text(hjust = -.2, vjust = .2) + 
  scale_color_viridis_d(name = "") + 
  scale_x_continuous(labels = scales::dollar) + 
  theme_tufte(14, "Avenir") + 
  theme(legend.position = "none",
        plot.caption = element_text(size = 9, face = "italic")) + 
  guides(size = F, color = F) + 
  ggtitle("Expectativa de vida") +
  #gganimate
  labs(title = 'Año: {frame_time}',
       x = 'PIB per capita (USD 2010)',
       y = "Esperanza de Vida", 
       caption = "Tamaño de las burbujas proporcional a la poblacion\nChart por @kodysolarte") +
  transition_time(Año) + 
  ease_aes('linear')
animate(lifeexp, nframes = 100, fps = 8, width = 600, height = 400, end_pause = 30)
#---------------------------------------------------------------------------------------------------------------
#GDP Suramerica 
gdp_sur <- data_sur %>% 
  filter(Pais != "MEX", Pais != "PAN")

gdp <- ggplot(gdp_sur, aes(x = Pais, y = pib, fill = Pais, label = Pais)) + 
  geom_col() + 
  geom_text(vjust = -.1) + 
  scale_fill_viridis_d(name = "") +
  scale_y_continuous(labels = scales::dollar) + 
  theme_tufte(14, "Avenir") +
  #Dejar en blanco eje x
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  #Animate 
  labs(title = 'Año: {frame_time}', 
       x = '', 
       y = 'PIB PER CAPITA',
       caption = "Producto interno bruto por persona ajustado en dólares internacionales (precios fijos de 2011).\nFuente: Banco Mundial\nPor @kodysolarte") + 
  transition_time(Año) + 
  ease_aes('linear')
animate(gdp, fps=5, end_pause = 30) # usar anim_save(filename) para guardar
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Pobreza Banco Mundial 
ggplot(pobreza, aes(x = Año, y = pobreza, group = country, color = "#FDE725FF")) +
  geom_line(size = 1) +
  geom_point() +
  theme_tufte(ticks=FALSE) +
  theme(legend.position = "none") + #Remover labels 
  scale_x_continuous(breaks = c(2008,2010,2012,2014,2016)) +
  #Paquete scales remover decimales
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #Etiquetas
  labs(title = "Tasa de incidencia en la pobreza (Colombia).",
       x = "Año", 
       y = "",
       caption = "Porcentaje de la población que vive con menos de $ 1.90 por día a precios internacionales de 2011.\nFuente: Banco Mundial\nPor: @kodysolarte") +
  #Animacion
  transition_reveal(Año)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Pobreza Departamento vs Ingreso 
pobreza <- ggplot(data = dane, aes(x = ingreso, y = pobreza, color = departamento, label = departamento)) + 
  geom_point(aes(size = 6)) + 
  geom_text(data = text_,hjust = -.2, vjust = .2) + #Resaltar Chocó,Bogotá y Total Nacional
  scale_color_viridis_d(name = "") + 
  scale_x_continuous(labels = scales::dollar) + #Signo dolar al eje x
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) + #Ajustar porcentaje eje y
  theme_tufte(14, "Avenir") + 
  theme(legend.position = "none",
        plot.caption = element_text(size = 9, face = "italic")) + #Eliminar labels 
  guides(size = F, color = F) + 
  #gganimate
  labs(title = 'Año: {frame_time}',
       x = 'Ingreso per cápita de la unidad de gasto de la población',
       y = "Pobreza Monetaria (%)", 
       caption = "Pobreza monetaria. 23 Departamentos y Bogotá D.C 2002-2018\nFuente: Dane\nPor:@kodysolarte") +
  #Animación
  transition_time(año) + 
  shadow_mark(alpha = 0.3, size = 0.5) + #Sombras 
  ease_aes('linear')
#Resolucion 
animate(pobreza, height = 500, width = 500)
#-------------------------------------------------------------------------------------------------------------------------------
#Tasa de cobertura bruta en educación 
esc <- ggplot(escolarizacion, aes(x = genero, y = esc_m, fill = genero, label = genero)) + 
  geom_col() + 
  geom_text(vjust = -.1) + 
  scale_fill_viridis_d(name = "") +
  #Escala de porcentajes eje x 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100)) +
  theme_tufte(14, "Avenir") +
  #Dejar en blanco eje x
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) + 
  #Animate 
  labs(title = 'Tasa bruta de matricula (TBM) Año: {frame_time}', 
       x = '', 
       y = '',
       caption = "Corresponde al porcentaje del total de hombres/mujeres matriculados en educación secundaria.\nFuente: Banco Mundial\nPor @kodysolarte") + 
  transition_time(Año) + 
  ease_aes('linear')
animate(esc, fps=5, end_pause = 30)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
int <- ggplot(movil, aes(x = Año)) +
  geom_line(aes(y = cell, colour = "Telefonia Movil"), size = 0.5) +
  geom_line(aes(y = internet, colour = "Internet"), size = 0.5) +
  theme_tufte(ticks=FALSE) +
  scale_y_continuous(limits = c(0,130)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Personas que usan internet [%]",
                                         labels = scales::percent_format(accuracy = 1,
                                                                         scale = 1))) +
  scale_color_viridis_d() + 
  labs(y = "Suscripción a telefonía móvil",
       x = "Año", 
       caption = "Suscripciones a telefonia móvil (por cada 100 personas) y personas que acceden a Internet (% de la población)\nFuente: Banco Mundial ") +
  #Animacion
  transition_reveal(Año)

animate(int, height = 400, width = 700)


