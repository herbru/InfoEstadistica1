library(tidyverse)
library(labelled)

individuos <- read_csv2("RawData/usu_individual_T322.txt",
                        col_types = cols(
                          CH10 = col_integer(),
                          CH06 = col_integer(),
                          CH03 = col_integer(),
                          CH12 = col_integer(),
                          CH14 = col_integer()
                        )
                       ) #Uso csv2 porque el separador por default es ";" y el separador de los decimales es ",".
hogares <-  read_csv2("RawData/usu_hogar_T322.txt")
individuos
hogares

#Etiquetado de variables.
var_label(hogares$II1)    <- "Cantidad de habitaciones de uso excusivo del hogar"
var_label(hogares$IX_TOT) <- "Cantidad de miembros del hogar"
var_label(hogares$IV6) <- "Tiene agua..."
var_label(hogares$IV3) <- "Los pisos interiores son principalmente de..."
var_label(hogares$IV8) <- "¿Tiene baño / letrina?"
var_label(hogares$IV10) <- "El baño tiene..."
var_label(individuos$CH10) <- "¿Asiste o asistió a algún establecimiento educativo? (colegio, escuela, universidad)"
var_label(individuos$CH06) <- "¿Cuántos años cumplidos tiene?"
var_label(individuos$CH03) <- "Relación de parentesco"
var_label(individuos$CH12) <- "¿Cuál es el nivel más alto que cursa o cursó?"
var_label(individuos$CH14) <- "Cuál fue el último año que aprobó"

#Etiquetado de valores de las variables.
hogares <- hogares %>%
  set_value_labels(
    IV6 = c(
      "por cañería dentro de la vivienda" = 1,
      "fuera de la vivienda pero dentro del terreno" = 2,
      " fuera del terreno" = 3),
    IV3 = c(
      "mosaico / baldosa / madera / cerámica / alfombra" = 1,
      "cemento / ladrillo fjo" = 2,
      "ladrillo suelto / tierra" = 3
    ),
    IV8 = c(
      "Sí" = 1,
      "No" = 2
    )
  )

individuos <- individuos %>%
  set_value_labels(
    ESTADO = c(
    "Entrevista no realizada" = 0,
    "Ocupado" = 1,
    "Desocupado" = 2,
    "Inactivo" = 3,
    "Menor de 10 años" = 4),
    CH03 = c(
      "Jefe" = 1,
      "Conyuge" = 2,
      "Hijo" = 3,
      "Yerno" = 4,
      "Nieto" = 5,
      "Madre / padre" = 6,
      " Suegro/a" = 7,
      " Hermano/a" = 8,
      " Otros familiares" = 9,
      " No familiares" = 10),
 )


#NBI 1 Hacinamiento -----------------------------------------------------

 #Le pongo un label a la variable.
count(hogares, IX_TOT) #Los valores de IX_TOT parecen razonables.

count(hogares, II1) #El valor 99 y el 0 es llamativo.
hogares <- hogares %>%
  mutate(II1 = ifelse(II1 == 99, NA, II1)) #Si la Cant. de habitaciones del hogar es 99 se lo considera Ns/Nc

hogares <- hogares %>%
  mutate (pers_x_habitacion = IX_TOT/II1, 
          NBI1 = ifelse(pers_x_habitacion <= 3, FALSE, TRUE), 
          NBI1 = ifelse(NRO_HOGAR %in% c(51,61), TRUE, NBI1))%>%
  select(-(pers_x_habitacion)) #Elimino la variable temporal.
  

count(hogares, NBI1)


# #NBI 2 Tipo de vivienda --------------------------------------------------

count(hogares, IV6, IV3)
hogares %>%
  ggplot(aes(x = as_factor(IV6), y = as_factor(IV3)))+
    geom_count()+
    labs(x = "Tiene agua por...", y = "Material de los pisos") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#Si IV3 = 4 lo seteo como NA, ya que 4 no es uno de los valores en le diseño de registro. 
#Ademas creo el NBI2 cuando el piso es de tierra o no tienen agua por cañeria dentro de la vivienda.
hogares <- hogares %>%
  mutate(IV3 = ifelse(IV3 == 4, NA, IV3), 
         NBI2 = ifelse(IV3 == 3 | IV6 != 1, TRUE, FALSE)) 

count(hogares, NBI2)

#NBI 3 Condiciones sanitarias. -------------------------------------------

count(hogares, IV8, IV10) #Veo las frecuencias relativas de ¿Tiene baño / letrina? y si el baño tiene arrastre de agua.

#Si IV10 = 0 lo seteo como Na ya que ese valor no forma parte del diseño de registro.
#Si no tiene baño o el baño no tiene arrastre de agua el NBI4 se considera instatisfecho.
hogares <- hogares %>%
  mutate(IV10 = ifelse(IV10 == 0, NA, IV10)) %>% 
  mutate(NBI3 = ifelse (IV8 ==2 | IV10 == 3, TRUE, FALSE)) 

count(hogares, NBI3)


# #NBI 4 Asistencia escolar ------------------------------------------------

#Veo la frecuancia absoluta de ¿Asiste o asistió a algún establecimiento educativo? (colegio, escuela, universidad)
count(individuos, CH10)
#Veo la frecuencia absoluta de ¿Cuántos años cumplidos tiene?
count(individuos, CH06) 

#Si el chico tiene entre 5 y 13 años incl. y no asiste o nunca asistio a la escuela el NBI4 individual es verdadero
#Agrupo los hogares.
#Si hay algun chico con NBI4 insatisfecha en el hogar, se considera NBI4 insatisfecha a todo el.
hogares <- individuos %>%
  mutate(NBI4_ind = ifelse(between(CH06,5,13) & CH10 %in% c(2, 3), TRUE, FALSE))%>%   
  group_by(CODUSU, NRO_HOGAR)%>% 
  summarise(NBI4 = ifelse(sum(NBI4_ind) != 0, TRUE, FALSE)) %>% 
  select(CODUSU, NRO_HOGAR, NBI4) %>%
  right_join(hogares, by = c("CODUSU", "NRO_HOGAR"))

summary(hogares$NBI4)


# #NBI 5 Subsitencia del hogar --------------------------------------------

#Recodificacion de variables para jefe ocupado.
#Si Nose relaizo la entrevista individual lo seteo como NA.
#Si ESTADO es ocupado seteo ocupado = TRUE.
#Si esta inactivo pero es jubilado o rentista lo tomo como ocupado.
individuos <- individuos %>%
  mutate(ESTADO = ifelse(ESTADO == 0, NA, ESTADO))%>% 
  mutate(OCUPADO = ifelse(ESTADO == 1, TRUE, FALSE)) %>% 
  mutate(OCUPADO = ifelse(ESTADO ==3 & (CAT_INAC == 1 | CAT_INAC == 2), TRUE, OCUPADO)) 

count(individuos, ESTADO)
count(individuos, CH10)
count(individuos, CH12)
count(individuos, CH14)

#Recodificacion de variables para nivel educativo del jefe
#Si CH10 es 0 o 9 lo seteo como NA.
#SiCH12 es 0 o 99 lo seteo como NA
#Si CH14 es 99 lo seteo como NA
individuos <- individuos %>%
  mutate(CH10 = ifelse(CH10 %in% c(0, 9), NA, CH10)) %>% 
  mutate(CH12 = ifelse(CH12 %in% c(0, 99), NA, CH12)) %>%
  mutate(CH14 = ifelse(CH14 == 99, NA, CH14)) 

#Si el jefe del hogar nunca asistio a asiste a la escuela pero no completo 3er grado entonces tiene un nivel educativo bajo
#Se agrupa por CODUSU y NRO_HOGAR
#Se crea una nueva columna para mas de 4 personas por ocupado.
#Si el nivel educativo del jefe es bajo y hay mas de 4 personas por ocupado el NBI5 es Insatisfecho.
#Se seleccionan solo las columnas que me sirven
#Agrego la columna NBI5 al df hogares.
hogares <- individuos %>%
  mutate(Niv_Educativo_Jefe_Bajo = ifelse(CH03 == 1 & (CH10 == 3 | (CH12<=2 & CH14<=2)), TRUE, FALSE))%>% 
  group_by(CODUSU, NRO_HOGAR) %>%
  mutate(Mas_de_4_Personas_x_Ocupado = ifelse(n()/sum(OCUPADO) >= 4, TRUE, FALSE)) %>% 
  summarise(NBI5 = ifelse(sum(Mas_de_4_Personas_x_Ocupado) != 0 & sum(Niv_Educativo_Jefe_Bajo) != 0, TRUE, FALSE))%>% 
  select(CODUSU, NRO_HOGAR, NBI5) %>%
  right_join(hogares, by = c("CODUSU", "NRO_HOGAR")) %>%  
  ungroup()

count(hogares, NBI5)


# Resumen NBI -------------------------------------------------------------

hogares <- hogares %>% 
  mutate(NBI_Vivienda = ifelse((NBI1 | NBI2 | NBI3), TRUE, FALSE), 
         NBI_Insercion = ifelse((NBI4 | NBI5), TRUE, FALSE),
         Tipo_NBI = ifelse(NBI_Vivienda, "Vivenda", ifelse(NBI_Insercion, "Insercion", "Ninguna")),
         Tipo_NBI = parse_factor(Tipo_NBI)
  ) 
# summary(hogares)
# 
# #Creo la variable cuantos NBI tiene el hogar.
# hogares <- hogares %>%
#   mutate(Cantidad_de_NBI = NBI1 + NBI2 + NBI3 +NBI4)
# 
# count(hogares, Cantidad_de_NBI)
# 
# #Tabla de contingencia de los NBI.
# tabla_combi_NBI <- hogares %>%
#   count(NBI1,NBI2,NBI3,NBI4,NBI5) %>% 
#   drop_na() %>%
#   arrange(desc(n))
# 
# View(tabla_combi_NBI)
# 
# #Ponderacion de la poblacion por la variable PONDERA.
# count(hogares, AGLOMERADO)
# 
# #Distribucion de la variable PONDERA
# hogares %>%
#   ggplot(aes(AGLOMERADO, PONDERA))+
#     geom_point(gmail.com)
# 
# summary(hogares, PONDERA)


# LP - LI -----------------------------------------------------------------

#Grafico el ITF de los hogares
ggplot(hogares, aes(x = ITF)) +
  geom_histogram(bins = 100)

#Elimino los ITF que son 0
hogares <- hogares %>% 
  mutate(ITF = ifelse(ITF == 0, NA, ITF))
  
# Especifico la ruta del archivo pdf
excel_adulto_equivalente <- "RawData/Pobreza_Data.csv"
excel_CBA <- "RawData/Series_CBA.csv"
excel_ICE <- "RawData/Series_ICE.csv"
excel_CBT <- "RawData/Series_CBT.csv"

adultos_equivalentes <- read_csv2(excel_adulto_equivalente)

CBA <- read_csv2(excel_CBA) %>% 
  select(Promedio, COD_REGION)

ICE <- read_csv2(excel_ICE) %>% 
  select(Promedio, COD_REGION)

CBT <- read_csv2(excel_CBT) %>% 
  select(Promedio, COD_REGION)

#Me aseguro que no hay otros valores que 1 o 2 en la variable CH04
#Reemplazo los años cumplidos = -1, a 0.
individuos <- individuos %T>% 
  distinct(CH04) %>% 
  mutate(CH06 = ifelse(CH06 == -1, 0, CH06))

#Funcion que recibe edad, sexo y df con equivalencias de adulto. 
obtener_equivalencia_adulto <- function(edad, sexo, df_equiv){
  df_equiv %>% 
    filter(Desde <= edad & edad < Hasta) %>% 
    pull(ifelse(sexo == 1, Varones, Mujeres))
}

#Calculo la cantidad de adultos equivalentes para cada hogar
individuos <- individuos %>%
  mutate(Equivalencia_Adulto = map2_dbl(CH06, CH04, ~ obtener_equivalencia_adulto(.x, .y, adultos_equivalentes)))

hogares <- individuos %>%
  group_by(NRO_HOGAR, CODUSU) %>%
  summarise(Equivalencia_Adultos = sum(Equivalencia_Adulto)) %>%
  right_join(hogares, by = c("CODUSU", "NRO_HOGAR"))

ggplot(hogares, aes(x = Equivalencia_Adultos)) +
  geom_histogram(binwidth = 0.25)

hogares <- hogares %>%
  left_join(CBA, by = c("REGION" = "COD_REGION")) %>% 
  left_join(CBT, by = c("REGION" = "COD_REGION")) %>% 
  rename(CBA = Promedio.x, CBT = Promedio.y)
  
hogares <- hogares %>%   
  mutate(LI = CBA * Equivalencia_Adultos,
         LP = CBT * Equivalencia_Adultos
         ) %>%
  mutate(Pobre     = ifelse(ITF < LP, TRUE, FALSE),
         Indigente = ifelse(ITF < LI, TRUE, FALSE)
         ) %>%
  mutate(
      Tipo_Pobreza = ifelse(Indigente, "Indigente", ifelse(Pobre, "Pobre", "No Pobre")),
      Tipo_Pobreza = parse_factor(Tipo_Pobreza)
    )

#Pondero a los hogares por ingreso
hogares %>%
  ungroup() %>% 
  count(Tipo_Pobreza, wt = PONDIH)

#Genero tabla de contingecia sobre el tipo de pobreza y el tipo de NBI
table(hogares$Pobreza, hogares$Tipo_NBI)

   
  
