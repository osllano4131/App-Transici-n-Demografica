# Fecha: 01/02/2021

# Librerias y Opciones
library(tidyverse)

# Cargar datos
esmeraldas <- as_tibble(read.table(file = "c:/Users/OSCAR/Documents/GitHub/App-Esmeraldas/Data/Esmeraldas.txt",header = T,sep = " "))


# Manipulación de Datos // 70 <- "Mayor A 70 años"
esmeraldas %>%
  mutate(
    P01 = factor(P01,labels = c("Hombre","Mujer")),
    URP = factor(URP,labels = c("Urbana","Rural")),
    P03 = as.integer(P03),
    P03 = ifelse(P03>=70,70,P03)
  ) -> esmeraldas01

# Gráfico de la razón de sexos, por edad simple, Agreguegando todos los datos desde la edad 70 

esmeraldas01 %>%
  group_by(P03,P01) %>%
  count() %>%  
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Mujer,Hombre,Total)) %>% 
  ggplot(aes(x = P03,y=Masculinidad)) +
  geom_line(color = "chartreuse3") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.line.x.top = element_line(color = "white"),
    axis.line.y.right = element_line(color = "white") ) +
  labs(title = "Razón de Sexos\n Por Edad simple",
       x = "Años",
       y = "Masculinidad",
       caption = "Fuente: Censo Poblacional Ecuador:2010") +
  scale_y_continuous(
    breaks = seq(90,120, by = 5), 
    labels = paste0(seq(90,120, by = 5),"%")
  )+
  scale_x_continuous(
    breaks = seq(0,70, by = 5), 
    labels = seq(0,70, by = 5))


# Coloque dos curvas de razón de sexos, en un mismo gráfico, una del para el área
# urbana y otra del área rural. Comente la tendencia.

esmeraldas01 %>%
  filter(URP == "Urbana") %>%
  group_by(P03,P01) %>%
  count() %>%  
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Mujer,Hombre,Total)) -> Razon_Urbana

esmeraldas01 %>%
  filter(URP == "Rural") %>%
  group_by(P03,P01) %>%
  count() %>%  
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Mujer,Hombre,Total)) -> Razon_Rural


Razon_Urbana %>%
  ggplot(aes(x = P03,y=Masculinidad)) +
  geom_line(aes(color = "Urbana")) +
  geom_line(data = Razon_Rural,aes(color = "Rural"))+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.line.x.top = element_line(color = "white"),
    axis.line.y.right = element_line(color = "white") ) +
  labs(title = "Razón de Sexos\n Por Edad simple",
       x = "Años",
       y = "Masculinidad",
       caption = "Fuente: Censo Poblacional Ecuador:2010") +
  scale_y_continuous(
    breaks = seq(80,150, by = 5), 
    labels = paste0(seq(80,150, by = 5),"%")
  )+
  scale_x_continuous(
    breaks = seq(0,70, by = 5), 
    labels = seq(0,70, by = 5))

#Haga un gráfico de la pirámide etaria de la provincia, incluya la tabla cruzada que
#genera esa pirámide.

esmeraldas01 %>%
  mutate(Grupo_Etario = cut_width(P03,width = 4, boundary = 0,closed = "left")) %>%
  group_by(Grupo_Etario,P01) %>%
  count() %>%   
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Masculinidad)) %>%
  pivot_longer(cols = c("Hombre", "Mujer"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>%
  piramide()


# Haga un gráfico en el que se observen superpuestas dos pirámides etarias de la
# misma provincia: una urbana y otra rural. Incluya las tablas cruzadas que generan
# estas pirámides

esmeraldas01 %>%
  filter(URP == "Urbana") %>%
  mutate(Grupo_Etario = cut_width(P03,width = 4, boundary = 0,closed = "left")) %>%
  group_by(Grupo_Etario,P01) %>%
  count() %>%   
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Masculinidad)) %>%
  pivot_longer(cols = c("Hombre", "Mujer"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") -> Edad_Urbana

esmeraldas01 %>%
  filter(URP == "Rural") %>%
  mutate(Grupo_Etario = cut_width(P03,width = 4, boundary = 0,closed = "left")) %>%
  group_by(Grupo_Etario,P01) %>%
  count() %>%   
  spread(key=P01,value = n) %>%
  mutate(
    Hombre = ifelse(is.na(Hombre),0,Hombre),
    Mujer = ifelse(is.na(Mujer),0,Mujer)) %>%
  ungroup() %>%
  mutate(Masculinidad = (Hombre/Mujer)*100,
         Total = Hombre+Mujer,
         tot = sum(Total,na.rm = T),
         Hombre = (Hombre/tot)*100,
         Mujer = (Mujer/tot)*100) %>%
  select(-c(tot,Masculinidad)) %>%
  pivot_longer(cols = c("Hombre", "Mujer"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") -> Edad_Rural

Edad_Urbana[,c(1,3,4)] %>%
  left_join(Edad_Rural[,c(1,4)],by = "Grupo_Etario") %>%
  rename(
    Pob_Urbana=`Poblacion por Sexo.x`,
    Pob_Rural= `Poblacion por Sexo.y`) -> Edad_Zona

Edad_Urbana %>%
  ggplot(aes(x = Grupo_Etario,y= `Poblacion por Sexo`
             )) +
  geom_bar(data = subset(Edad_Rural,Sexo == "Hombre") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           aes(fill = "Hombre Rural"),
           stat = "identity", width = 0.75 ) +
  geom_bar(data = subset(Edad_Rural, Sexo == "Mujer"),
           aes(fill = "Mujer Rural"),
           stat = "identity", width = 0.75) +
  geom_bar(data = subset(Edad_Urbana,Sexo == "Hombre") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           aes(fill = "Hombre Urbano",y=`Poblacion por Sexo`),
           stat = "identity", width = 0.75, 
            ) +
  geom_bar(data = subset(Edad_Urbana, Sexo == "Mujer"),
           aes(fill = "Mujer Urbano"),
           stat = "identity", width = 0.75,
           #fill = "coral1"
           ) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(title = "Pirámide Poblacional Esmeraldas",
       x = "Años",
       y = "Hombres                        Mujeres",
       caption = "Fuente: Censo 2010") +
  scale_y_continuous(
    breaks = seq(-7,7, by = 1),
    labels = paste0(c(seq(-7, 0, by = 1)*-1, seq(1, 7, by = 1)), "%"))
