library(reshape2)
library(tidyverse)
library(sf)

data1 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Domestic general government health expenditure (GGHE-D) as percentage of gross domestic product (GDP) (%.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

names(data1)[1] <- "PAIS"

data1 <- melt(data1, id.vars = "PAIS")
data1$PAIS <- gsub(data1$PAIS, pattern = '["]', replacement = '')
data1$variable <- gsub(data1$variable, pattern = '["]', replacement = '')
data1$value <- gsub(data1$value, pattern = '["]', replacement = '')
names(data1)[1] <- "PAIS_1"
names(data1)[2] <- "ANO_1"
names(data1)[3] <- "gasto_publico_porcentaje_PBI"



data2 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Domestic general government health expenditure (GGHE-D) per capita in US$.csv", 
                                                                                      ";", escape_double = FALSE, trim_ws = TRUE)

names(data2)[1] <- "PAIS"
data2 <- melt(data2, id.vars = "PAIS")
data2$PAIS <- gsub(data2$PAIS, pattern = '["]', replacement = '')
data2$variable <- gsub(data2$variable, pattern = '["]', replacement = '')
data2$value <- gsub(data2$value, pattern = '["]', replacement = '')
names(data2)[1] <- "PAIS_2"
names(data2)[2] <- "ANO_2"
names(data2)[3] <- "gasto_publico_porcentaje_percapita_dolares"


#data3 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Domestic general government health expenditure (GGHE-D) as percentage of general government expenditure (GGE) (%).csv", 
             #  ";", escape_double = FALSE, trim_ws = TRUE)

#names(data3)[1] <- "PAIS"

#data3 <- melt(data3, id.vars = "PAIS")
#data3$PAIS <- gsub(data3$PAIS, pattern = '["]', replacement = '')
#data3$variable <- gsub(data3$variable, pattern = '["]', replacement = '')
#data3$value <- gsub(data3$value, pattern = '["]', replacement = '')
#names(data3)[1] <- "PAIS"
#names(data3)[2] <- "ANO"
#names(data3)[3] <- "gasto_publico_porcentaje_gasto_general"


data4 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Domestic private health expenditure (PVT-D) per capita in US$.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)
names(data4)[1] <- "PAIS"
data4 <- melt(data4, id.vars = "PAIS")
data4$PAIS <- gsub(data4$PAIS, pattern = '["]', replacement = '')
data4$variable <- gsub(data4$variable, pattern = '["]', replacement = '')
data4$value <- gsub(data4$value, pattern = '["]', replacement = '')
names(data4)[1] <- "PAIS_4"
names(data4)[2] <- "ANO_4"
names(data4)[3] <- "gasto_privado_percapita_dolares"



data_1 <- cbind(data1, data2)
data_def <- cbind(data_1, data4)

data_def <- data_def %>% select(PAIS_1, ANO_2, gasto_publico_porcentaje_PBI, gasto_publico_porcentaje_percapita_dolares, gasto_privado_percapita_dolares)
names(data_def)[1] <- "PAIS"
names(data_def)[2] <- "ANO"

presupuesto_defensa <- read_csv("D:/Guada/Clases/Clases_R_Eze/Clase 1/presupuesto_defensa.csv")
presupuesto_defensa <- presupuesto_defensa %>% select(Country_name, Code, Continent, Sub_Continent)
names(presupuesto_defensa)[1] <- "PAIS"
data_final <- inner_join(data_def, presupuesto_defensa, by = "PAIS")

data_def$PAIS[data_def$PAIS == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
data_def$PAIS[data_def$PAIS == "Cuba"] <- "Cuba"
data_def$PAIS[data_def$PAIS == "Bolivia (Plurinational State of)"] <- "Bolivia"
data_def$PAIS[data_def$PAIS == "Haiti"] <- "Haiti"
data_def$PAIS[data_def$PAIS == "Dominican Republic"] <- "Dominican Republic"

data_final <- data_final %>% distinct(PAIS, ANO, gasto_publico_porcentaje_PBI,
                                                         gasto_publico_porcentaje_percapita_dolares, 
                                                         gasto_privado_percapita_dolares, Code, 
                                                         Continent, Sub_Continent) 

unique(data_LATAM$P)
data_LATAM <- data_final %>% filter(Sub_Continent == "South America" | Sub_Continent == "Central America" |
                                  PAIS == "Mexico" | Sub_Continent == "Caribbean")


### PERSONAL DE SALUD

#df_1 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Biomedical engineers density.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
#names(df_1)[1] <- "PAIS"
#df_1 <- melt(df_1, id.vars = "PAIS")
#df_1$value <- gsub(df_1$value, pattern = 'No data', replacement = NA)
#df_1$value <- gsub(df_1$value, pattern = '&lt', replacement = NA)
#names(df_1)[1] <- "PAIS"
#names(df_1)[2] <- "ANO"
#df_1$ANO <- as.numeric(as.character(df_1$ANO))
#names(df_1)[3] <- "densidad_ingenieros_biomedicos"


#df_2<- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Medical and Pathology Laboratory.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
#names(df_2)[1] <- "PAIS"
#df_2$ANO <- as.numeric(as.character(df_2$ANO))
#names(df_2)[1] <- "PAIS"
#names(df_2)[2] <- "ANO"

df_3 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Medical doctors (per 10 000 population).csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
names(df_3)[1] <- "PAIS"
names(df_3)[2] <- "ANO"
df_3$ANO <- as.numeric(as.character(df_3$ANO))

df_4 <- read_delim("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/Nursing personnel (per 10 000 population).csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
names(df_4)[1] <- "PAIS"
names(df_4)[2] <- "ANO"
df_4$ANO <- as.numeric(as.character(df_4$ANO))


df12 <- inner_join(df_3, df_4)
df12$PAIS[df12$PAIS == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
df12$PAIS[df12$PAIS == "Cuba"] <- "Cuba"
df12$PAIS[df12$PAIS == "Bolivia (Plurinational State of)"] <- "Bolivia"
df12$PAIS[df12$PAIS == "Haiti"] <- "Haiti"
df12$PAIS[df12$PAIS == "Dominican Republic"] <- "Dominican Republic"

df_final <- inner_join(df12, presupuesto_defensa, by = "PAIS")

df_final <- df_final %>% distinct(PAIS, ANO,`Medical doctors (per 10 000 population)`, `Nursing personnel (per 10 000 population)`,
                                  Code, Continent, Sub_Continent) 
unique(df_LATAM$PAIS)
df_LATAM <- df_final %>% filter(Sub_Continent == "South America" | Sub_Continent == "Central America" |
                                      PAIS == "Mexico" | Sub_Continent == "Caribbean")
names(df_LATAM)
names(df_LATAM)[3] <- "MEDICOS_CADA_DIEZ_MIL_HAB"
names(df_LATAM)[4] <- "ENFERMERAS_CADA_DIEZ_MIL_HAB"
#### csv finales
mapa_paises <- st_read("https://datahub.io/core/geo-countries/r/countries.geojson")
names(mapa_paises)[2] <- "Code"
df_LATAM <- df_LATAM %>% inner_join(mapa_paises, by = "Code")
names(df_LATAM)
df_LATAM <- df_LATAM %>% select(PAIS, ANO, MEDICOS_CADA_DIEZ_MIL_HAB, ENFERMERAS_CADA_DIEZ_MIL_HAB)
write.csv(df_LATAM, "D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/medicos_latam.csv")


data_LATAM <- data_LATAM %>% inner_join(mapa_paises, by = "Code")
data_LATAM <- data_LATAM %>% select(PAIS, ANO, gasto_publico_porcentaje_PBI, gasto_publico_porcentaje_percapita_dolares, gasto_privado_percapita_dolares)
write.csv(data_LATAM, "D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/presupuesto_salud_latinoamerica_.csv")



################ ARG
DISTRITO <- read_excel("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/DISTRITO_CASOS_POBL_ARG.xlsx")
CAMAS_POR_DISTRITO <- read_excel("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/CAMAS_POR_DISTRITO.xlsx")

CAMAS_POR_DISTRITO <- melt(data = CAMAS_POR_DISTRITO, id.vars = "DISTRITO")
DISTRITO <- inner_join(DISTRITO, CAMAS_POR_DISTRITO)
names(DISTRITO)[5] <- "TIPO_CAMA"
names(DISTRITO)[6] <- "CANTIDAD"
 
provincias_arg <- st_read("D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/provincia (2)/provincia.json")
unique(provincias_arg$nam)
names(provincias_arg)[6] <- "DISTRITO"
unique(DISTRITO$DISTRITO)     

DISTRITO_F <- left_join(DISTRITO, provincias_arg, by = "DISTRITO")
DISTRITO_F <- DISTRITO_F %>% select(DISTRITO, CASOS, MUERTOS, POBLACION, TIPO_CAMA, CANTIDAD
                                    # falta geometry
                                    )
write.csv(DISTRITO_F, "D:/Guada/Clases/Lab_Pol_Publ/Tidyverse/limpieza/CAMAS_CASOS_DISTRITO.csv")
