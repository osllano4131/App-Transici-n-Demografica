library(tidyverse)

# EJERCICIO ---------------------------------------------------------------
?tribble
tribble(
  ~Grupo_Etario,~Hombre,~Mujer,~Total,
  '00 - 04',5426,5286,10712,
  '05 - 09',5520,5493,11013,
  '10 - 14',5139,5062,10201,
  '15 - 19',4766,4342,9108,
  '20 - 24',3709,3602,7311,
  '25 - 29',3269,3397,6666,
  '30 - 34',2783,2906,5689,
  '35 - 39',2462,2541,5003,
  '40 - 44',1956,1951,3907,
  '45 - 49',1715,1740,3455,
  '50 - 54',1392,1382,2774,
  '55 - 59',1125,1026,2151,
  '60 - 64',928,857,1785,
  '65 - 69',703,706,1409,
  '70 - 74',494,433,927,
  '75 - 79',325,276,601,
  '80 - 84',198,242,440,
  '85 y más',174,152,212
)%>%
  mutate(
    Masculinidad = (Hombre/Mujer)*100,
    tot = sum(Total),
    Hombre = (Hombre/tot)*100,
    Mujer = (Mujer/tot)*100
  ) %>%
  select(-tot) %>%
  pivot_longer(cols = c("Hombre", "Mujer"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") -> Datos

Datos %>%
  ggplot(aes(x = Grupo_Etario,
             y = `Poblacion por Sexo`,
             fill = Sexo)) +
  geom_bar(data = subset(Datos,Sexo == "Hombre") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.75, fill = "blue") +
  geom_label(data = subset(Datos,Sexo == "Hombre")%>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
            aes(label = round(`Poblacion por Sexo`*-1,2)),
            position=position_dodge(width=0.5), vjust=-0.15,fill="lightblue")+
  geom_bar(data = subset(Datos, Sexo == "Mujer"),
           stat = "identity", width = 0.75, fill = "pink") +
  geom_label(data = subset(Datos,Sexo == "Mujer"),
            aes(label =round(`Poblacion por Sexo`,2)),
            position=position_dodge(width=0.5), vjust=-0.15,fill="pink")+
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
        axis.text.x = element_text(family = "Arial"),
        axis.text.y = element_text(family = "Arial")
  ) +
  labs(title = "Pirámide Poblacional de Portoviejo,2015",
       x = "Años",
       y = "Hombres                        Mujeres",
       caption = "Fuente: ---") +
  scale_y_continuous(
    breaks = seq(-7,7, by = 1), 
    labels = paste0(c(seq(-7, 0, by = 1)*-1, seq(1, 7, by = 1)), "%"))


# Automatización ----------------------------------------------------------

piramide <- function(Datos,titulo = "Pirámide Poblacional",caption="Fuente ---"){
  require(tidyverse)
  maximo <- ceiling(max(Datos$`Poblacion por Sexo`))
  Datos %>%
    ggplot(aes(x = Grupo_Etario,
               y = `Poblacion por Sexo`,
               fill = Sexo)) +
    geom_bar(data = subset(Datos,Sexo == "Hombre") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
             stat = "identity", width = 0.75, fill = "blue") +
    geom_label(data = subset(Datos,Sexo == "Hombre")%>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
               aes(label = round(`Poblacion por Sexo`*-1,2)),
               position=position_dodge(width=0.5), vjust=-0.15,fill="lightblue")+
    geom_bar(data = subset(Datos, Sexo == "Mujer"),
             stat = "identity", width = 0.75, fill = "pink") +
    geom_label(data = subset(Datos,Sexo == "Mujer"),
               aes(label =round(`Poblacion por Sexo`,2)),
               position=position_dodge(width=0.5), vjust=-0.15,fill="pink")+
    coord_flip() +
    ggthemes::theme_tufte() +
    theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20),
          axis.text.x = element_text(family = "Arial"),
          axis.text.y = element_text(family = "Arial")
    ) +
    labs(title = titulo,
         x = "Años",
         y = "Hombres                        Mujeres",
         caption = caption) +
    scale_y_continuous(
      breaks = seq(maximo*-1,maximo, by = 1), 
      labels = paste0(c(seq(maximo*-1, 0, by = 1)*-1, seq(1,maximo, by = 1)), "%"))  
}

  
piramide(Datos)



# Masculinidad ------------------------------------------------------------

masculinidad <- unique(Datos$Masculinidad)
edad <- unique(Datos$Grupo_Etario)
plot(y = masculinidad,x= c(1:length(masculinidad)),
     ylim = c(min(masculinidad)*0.9,max(masculinidad)*1.1),
     type = "b",lty = 2,
     main =  "Masculinidad Por Grupo Etario Portoviejo",
     xlab = "Grupo Etario",ylab = "% Masculinidad",
     axes = F
)
abline(v = 1:length(masculinidad),h = 1:length(masculinidad),col="grey")
text(y = masculinidad,x= c(1:length(masculinidad)),
     labels = round(masculinidad,2),
     pos = c(3,3,1,3,4,1,rep(3,4),1,3,3,1,3,3,1,3))
points(x = c(1:length(masculinidad)),y = masculinidad,pch=19,col="red")
axis(1,at = c(1:length(masculinidad)),labels = edad)
axis(2)

plot_masculinidad <- function(Datos){
  masculinidad <- unique(Datos$Masculinidad)
  edad <- unique(Datos$Grupo_Etario)
  plot(y = masculinidad,x= c(1:length(masculinidad)),
       ylim = c(min(masculinidad)*0.9,max(masculinidad)*1.1),
       type = "b",lty = 2,
       main =  "Masculinidad Por Grupo Etario Portoviejo",
       xlab = "Grupo Etario",ylab = "% Masculinidad",
       xaxt = "n"
  )
  abline(v = 1:length(masculinidad),h = 1:length(masculinidad),col="grey")
  text(y = masculinidad,x= c(1:length(masculinidad)),
       labels = round(masculinidad,2),
       pos = c(3,3,1,3,4,1,rep(3,4),1,3,3,1,3,3,1,3))
  points(x = c(1:length(masculinidad)),y = masculinidad,pch=19,col="red")
  axis(1,at = c(1:length(masculinidad)),labels = edad)
}

plot_masculinidad(Datos)
