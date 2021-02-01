# Fecha: 01/02/2021

# Libreria y Opciones
library(tidyverse)
library(sparklyr)

# Cargar servidor 
sc <- spark_connect(master = "local",version = "2.4.3")

# Cargar Censo
censo <- spark_read_csv(sc,"../../Data/CPV2010M_CSV_Nacional/CPV2010M_Poblacion.csv")

# Seleccionar Provincia de Esmeraladas
censo %>%
  filter(I01 == 8) %>%
  select(I01,I02,URP,P01,P02,P03) -> Esmeraldas
  group_by(URP) %>%

# Cargar A Memoria
Esmeraldas <- collect(Esmeraldas)

# Escribir Json
Esmeraldas %>%
  jsonlite::write_json("C:/Users/OSCAR/Documents/GitHub/App-Esmeraldas/Esmeraldas.json")

# Cerrar Spark
spark_disconnect_all()
