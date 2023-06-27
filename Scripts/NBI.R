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
head(individuos)
head(hogares)

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
  mutate(II1 = ifelse(II1 == 99, NA, II1)) #Si la Cant. de habitaciones del hogar es 99 se lo considera NA. Hay 12 NA

hogares <- hogares %>%
  mutate(
    pers_x_habitacion = IX_TOT/II1, 
    NBI1 = ifelse(pers_x_habitacion <= 3, FALSE, TRUE), 
    NBI1 = ifelse(NRO_HOGAR %in% c(51,61), TRUE, NBI1)
  ) %>%
  select(-(pers_x_habitacion)) #Elimino la variable temporal.


hogares %>% count(NBI1, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2)


# #NBI 2 Tipo de vivienda --------------------------------------------------

count(hogares, IV6, IV3)
hogares %>%
  ggplot(aes(x = as_factor(IV6), y = as_factor(IV3)))+
    geom_count()+
    labs(x = "Tiene agua por...", y = "Material de los pisos") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#Si IV3 = 4 lo seteo como NA, ya que 4 no es uno de los valores en el diseño de registro. 
#Ademas creo el NBI2 cuando el piso es de tierra o no tienen agua por cañeria dentro de la vivienda.
hogares <- hogares %>%
  mutate(
    IV3 = ifelse(IV3 == 4, NA, IV3), 
    NBI2 = ifelse(IV3 == 3 | IV6 != 1, TRUE, FALSE)
  ) 

hogares %>% count(NBI2, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2) #25 NA

#NBI 3 Condiciones sanitarias. -------------------------------------------

count(hogares, IV8, IV10) #Veo las frecuencias relativas de ¿Tiene baño / letrina? y si el baño tiene arrastre de agua.

#Si IV10 = 0 lo seteo como Na ya que ese valor no forma parte del diseño de registro.
#Si no tiene baño o el baño no tiene arrastre de agua el NBI4 se considera instatisfecho.
hogares <- hogares %>%
  mutate(IV10 = ifelse(IV10 == 0, NA, IV10)) %>% 
  mutate(NBI3 = ifelse (IV8 ==2 | IV10 == 3, TRUE, FALSE)) 

hogares %>% count(NBI3, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2) #0 NA


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
  right_join(hogares, by = c("CODUSU", "NRO_HOGAR")) %>% 
  ungroup()

hogares %>% count(NBI4, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2) #0 NA

# #NBI 5 Subsitencia del hogar --------------------------------------------

#Recodificacion de variables para jefe ocupado.
#Si Nose relaizo la entrevista individual lo seteo como NA.
#Si ESTADO es ocupado seteo ocupado = TRUE.
#Si esta inactivo pero es jubilado o rentista lo tomo como ocupado.
individuos <- individuos %>%
  mutate(ESTADO = ifelse(ESTADO == 0, NA, ESTADO))%>% 
  mutate(OCUPADO = ifelse(ESTADO == 1, TRUE, FALSE)) %>% 
  mutate(OCUPADO = ifelse(ESTADO == 3 & (CAT_INAC == 1 | CAT_INAC == 2), TRUE, OCUPADO)) 

count(individuos, ESTADO)
count(individuos, CH10)
count(individuos, CH12)
count(individuos, CH14)

#Recodificacion de variables para nivel educativo del jefe
#Si CH10 es 0 o 9 lo seteo como NA.
#Si CH12 es 0 o 99 lo seteo como NA
#Si CH14 es 99 lo seteo como NA
individuos <- individuos %>%
  mutate(CH10 = ifelse(CH10 %in% c(0, 9), NA, CH10)) %>% 
  mutate(CH12 = ifelse(CH12 %in% c(0, 99), NA, CH12)) %>%
  mutate(CH14 = ifelse(CH14 == 99, NA, CH14)) 

#Si el jefe del hogar nunca asistio o asistio a la escuela pero no completo 3er grado entonces tiene un nivel educativo bajo
#Se agrupa por CODUSU y NRO_HOGAR
#Se crea una nueva columna para mas de 4 personas por ocupado.
#Si el nivel educativo del jefe es bajo y hay mas de 4 personas por ocupado el NBI5 es Insatisfecho.
#Se seleccionan solo las columnas que me sirven
#Agrego la columna NBI5 al df hogares.
hogares <- individuos %>%
  mutate(Niv_Educativo_Jefe_Bajo = ifelse(CH03 == 1 & (CH10 == 3 | (CH12 <= 2 & CH14 <= 2)), TRUE, FALSE))%>% 
  group_by(CODUSU, NRO_HOGAR) %>%
  mutate(Mas_de_4_Personas_x_Ocupado = ifelse(n()/sum(OCUPADO) >= 4, TRUE, FALSE)) %>% 
  summarise(NBI5 = ifelse(sum(Mas_de_4_Personas_x_Ocupado) != 0 & sum(Niv_Educativo_Jefe_Bajo) != 0, TRUE, FALSE))%>% 
  select(CODUSU, NRO_HOGAR, NBI5) %>%
  right_join(hogares, by = c("CODUSU", "NRO_HOGAR")) %>%  
  ungroup()

hogares %>% count(NBI5, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2)

# Resumen NBI -------------------------------------------------------------

hogares <- hogares %>% 
  mutate(NBI_Vivienda = ifelse((NBI1 | NBI2 | NBI3), TRUE, FALSE), 
         NBI_Insercion = ifelse((NBI4 | NBI5), TRUE, FALSE),
         Tipo_NBI = ifelse(NBI_Vivienda, "Vivenda", ifelse(NBI_Insercion, "Insercion", "Ninguna")),
         Tipo_NBI = parse_factor(Tipo_NBI)
  ) 

#Creo la variable cuantos NBI tiene el hogar.
hogares <- hogares %>%
  mutate(Cantidad_de_NBI = NBI1 + NBI2 + NBI3 + NBI4 + NBI5)

hogares %>% count(Cantidad_de_NBI, wt = PONDIH) %>% 
  mutate(Freq_Relativa = n/sum(n) * 100) %>% 
  round(2)

#Tabla de contingencia de los NBI.
tabla_combi_NBI <- hogares %>%
  count(NBI1,NBI2,NBI3,NBI4,NBI5) %>%
  drop_na() %>%
  arrange(desc(n))

tabla_combi_NBI

#Tabla de frecuencias absolutas de hogares que tienen al menos 1 NBI agrupados por aglomerado
hogares %>% 
  mutate(Tiene_NBI = ifelse(NBI1 + NBI2+ NBI3 + NBI4 + NBI5 == 0, 0, 1)) %>% 
  count(Tiene_NBI, AGLOMERADO, wt = PONDERA) %>% 
  filter(Tiene_NBI == 1)

# LP - LI -----------------------------------------------------------------

#Grafico el ITF de los hogares
ggplot(hogares, aes(x = ITF)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::comma,limits = c(0, 1000000))

#Grafico el ITF de los hogares
ggplot(hogares, aes(x = ITF/IX_TOT)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::comma,limits = c(0, 1000000))
  

#Elimino los ITF que son 0
hogares <- hogares %>% 
  mutate(ITF = ifelse(ITF == 0, NA, ITF)) %>%
  filter(!is.na(ITF))
  
# Especifico la ruta del archivo pdf
excel_adulto_equivalente <- "RawData/Pobreza_Data.csv"
excel_CBA <- "RawData/Series_CBA.csv"
excel_ICE <- "RawData/Series_ICE.csv"
excel_CBT <- "RawData/Series_CBT.csv"

adultos_equivalentes <- read_csv2(excel_adulto_equivalente)  %>% 
  pivot_longer(cols = c("Mujeres", "Varones"), names_to = "Sexo") %>% 
  mutate(Sexo = ifelse(Sexo == "Varones", 1, 2))

CBA <- read_csv2(excel_CBA) %>% 
  select(Promedio, COD_REGION)

ICE <- read_csv2(excel_ICE) %>% 
  select(Promedio, COD_REGION)

CBT <- read_csv2(excel_CBT) %>% 
  select(Promedio, COD_REGION)

#Me aseguro que no hay otros valores que 1 o 2 en la variable CH04
#Reemplazo los años cumplidos = -1, a 0.
individuos <- individuos %>% 
  mutate(CH06 = ifelse(CH06 == -1, 0, CH06))

#Calculo la cantidad de adultos equivalentes para cada hogar
individuos <- individuos %>%
  left_join(adultos_equivalentes, join_by(between(CH06, Desde, Hasta, bounds = "[)"), CH04 == Sexo)) %>% 
  rename(Equivalencia_Adulto = value)
  
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
  mutate(
    LI = CBA * Equivalencia_Adultos,
    LP = CBT * Equivalencia_Adultos
  ) %>%
  mutate(
    Pobre     = ifelse(ITF < LP, TRUE, FALSE),
    Indigente = ifelse(ITF < LI, TRUE, FALSE)
  ) %>%
  mutate(
    Tipo_Pobreza = ifelse(Indigente, "Indigente", ifelse(Pobre, "Pobre", "No Pobre")),
    Tipo_Pobreza = parse_factor(Tipo_Pobreza)
  )

#Tabla de frecuencia relativa de pobreza por individuo
hogares %>%
  ungroup() %>% 
  count(Tipo_Pobreza, wt = PONDIH * IX_TOT) %>%
  mutate(freq_relativa = (n/sum(n))*100)

#Tabla de frecuencia relativa de pobreza por hogar
hogares %>%
  ungroup() %>% 
  count(Tipo_Pobreza, wt = PONDIH) %>%
  mutate(freq_relativa = (n/sum(n))*100)
  
#Genero tabla de contingecia sobre el tipo de pobreza y el tipo de NBI
tabla_contingencia <- table(hogares$Tipo_Pobreza, hogares$Tipo_NBI)
tabla_contingencia <- tabla_contingencia[,-3]
prop_cont_tabla <- round(prop.table(tabla_contingencia) * 100, 2)

prop_cont_tabla

# NSE ---------------------------------------------------------------------
Cuadro_1 <- read_csv2("RawData/Cuadro_1_NSE.csv")
Cuadro_2 <- read_csv2("RawData/Cuadro_2_NSE.csv")
Nombres_NSE <- read_csv2("RawData/Nombres_NSE.csv")

Cuadro_1 <- Cuadro_1 %>%
  mutate(CMed = ifelse(CMed == "CM si", as.logical(TRUE), as.logical(FALSE))) %>%
  pivot_longer(cols = contains("/"), names_to = c("AP", "NE"), names_sep = "/", values_to = "Valor") %>% 
  rename(
    Primer_Nivel         = `1er Nivel`, 
    Segundo_Nivel        = `2do Nivel`, 
    Tercer_Nivel         = `3er Nivel`,
    Nivel_Educativo_Jefe = NE,
    Cobertura_Medica     = CMed) %>% 
  mutate(
    Nivel_Educativo_Jefe = case_when(
      Nivel_Educativo_Jefe == "NE1" ~ as.double(1),
      Nivel_Educativo_Jefe == "NE2" ~ as.double(2),
      Nivel_Educativo_Jefe == "NE3" ~ as.double(3),
      Nivel_Educativo_Jefe == "NE4" ~ as.double(4)
    ),
    Segundo_Nivel = ifelse(Primer_Nivel == "Empleador", NA, Segundo_Nivel),
    Estado = "Ocupado"
  )

Cuadro_2 <- Cuadro_2 %>% 
  pivot_longer(cols = contains("/"), names_to = c("AP", "NE"), names_sep = "/", values_to = "Valor") %>% 
  rename(
    Primer_Nivel         = `1er Nivel`, 
    Segundo_Nivel        = `2do Nivel`, 
    Tercer_Nivel         = `3er Nivel`,
    Nivel_Educativo_Jefe = NE,
    Cobertura_Medica     = CMed) %>% 
  mutate(
    Nivel_Educativo_Jefe = case_when(
      Nivel_Educativo_Jefe == "NE1" ~ as.double(1),
      Nivel_Educativo_Jefe == "NE2" ~ as.double(2),
      Nivel_Educativo_Jefe == "NE3" ~ as.double(3),
      Nivel_Educativo_Jefe == "NE4" ~ as.double(4)
    )
  )

Cuadros <- bind_rows(Cuadro_1, Cuadro_2)
  
# Calculo la cantidad de aportantes para cada hogar
individuos <- individuos %>%
  mutate(Aporta = ifelse(P47T > 0, TRUE, FALSE)) %>% 
  group_by(CODUSU, NRO_HOGAR)

View(individuos %>% count(ESTADO, CAT_OCUP, CAT_INAC))
#Los inactivos y menores de 10 años tienen CAT_OCUP = 0

#Calculo las otras variables para el jefe del hogar
#Los estudiantes y las amas de casa los tomo como jubilados
#33NA porque tienen CAT_OCUP = 0 que no esta entre las opciones
jefes_hogares <- individuos %>%
  filter(P47T == max(P47T)) %>%
  mutate(
    Cantidad_Aportantes = sum(Aporta, na.rm = FALSE),
    Estado = case_when(
      ESTADO   == 1 ~ "Ocupado",
      ESTADO   == 2 ~ "Desocupado",
      CAT_INAC == 1 ~ "Jubilado",
      ESTADO == 3 &  CAT_INAC %in% c(3, 4)  ~ "Jubilado",
    ),
    Primer_Nivel  = case_when(
      CAT_OCUP == 3 ~ "Empleado",
      CAT_OCUP == 2 ~ "Cuenta Propia",
      CAT_OCUP == 1 ~ "Empleador"
    ),
    Segundo_Nivel = case_when(
      substr(as.character(PP04D_COD), 5, 5) == 1 & substr(as.character(PP04D_COD), 3, 3) == 1 ~ "Profesional",
      substr(as.character(PP04D_COD), 5, 5) == 2 & substr(as.character(PP04D_COD), 3, 3) == 1 ~ "Tecnico",
      substr(as.character(PP04D_COD), 5, 5) == 3 & substr(as.character(PP04D_COD), 3, 3) == 1 ~ "Operativo",
      substr(as.character(PP04D_COD), 5, 5) == 4 & substr(as.character(PP04D_COD), 3, 3) == 1 ~ "No calificada",
      substr(as.character(PP04D_COD), 3, 3) == 0 & Primer_Nivel == "Empleado" ~ "Directivo",
      substr(as.character(PP04D_COD), 3, 3) == 2 & Primer_Nivel == "Empleado" ~ "Jefe",
      substr(as.character(PP04D_COD), 3, 3) == 3 & Primer_Nivel == "Empleado" ~ "Trabajador",
    ),
    Tercer_Nivel = case_when(
      Primer_Nivel == "Cuenta Propia" & PP3E_TOT >= 35 ~ "Ocupado",
      Primer_Nivel == "Cuenta Propia" & PP3E_TOT < 35 ~ "Subocupado",
      Primer_Nivel == "Empleado" & Segundo_Nivel %in% c("Directivo", "Jefe") & between(PP04C, 1, 5) ~ "Hasta 5 pers.",
      Primer_Nivel == "Empleado" & Segundo_Nivel %in% c("Directivo", "Jefe") & between(PP04C, 6, 8) ~ "6-40 pers.",
      Primer_Nivel == "Empleado" & Segundo_Nivel %in% c("Directivo", "Jefe") & between(PP04C, 9, 10) ~ "41-200.",
      Primer_Nivel == "Empleado" & Segundo_Nivel %in% c("Directivo", "Jefe") & between(PP04C, 11, 12) ~ ">200.",
      Primer_Nivel == "Empleado" & Segundo_Nivel == "Trabajador" & substr(as.character(PP04D_COD), 5, 5) == "1" & substr(as.character(PP04D_COD), 3, 3) %in% c("1", "3") ~ "Profesional",
      Primer_Nivel == "Empleado" & Segundo_Nivel == "Trabajador" & substr(as.character(PP04D_COD), 5, 5) == "2" & substr(as.character(PP04D_COD), 3, 3) %in% c("1", "3") ~ "Tecnico",
      Primer_Nivel == "Empleado" & Segundo_Nivel == "Trabajador" & substr(as.character(PP04D_COD), 5, 5) == "3" & substr(as.character(PP04D_COD), 3, 3) %in% c("1", "3") ~ "Operativo",
      Primer_Nivel == "Empleado" & Segundo_Nivel == "Trabajador" & substr(as.character(PP04D_COD), 5, 5) == "4" & substr(as.character(PP04D_COD), 3, 3) %in% c("1", "3") ~ "No calificada",
      Primer_Nivel == "Empleador" & is.na(Segundo_Nivel) & between(PP04C, 1, 5) ~ "Hasta 5 pers.",
      Primer_Nivel == "Empleador" & is.na(Segundo_Nivel) & between(PP04C, 6, 8) ~ "6-40 pers.",
      Primer_Nivel == "Empleador" & is.na(Segundo_Nivel) & between(PP04C, 9, 10) ~ "41-200 pers.",
      Primer_Nivel == "Empleador" & is.na(Segundo_Nivel) & between(PP04C, 11, 12) ~ "> 200 pers."
    ),
    Cobertura_Medica = ifelse(CH08 == 4, FALSE, ifelse(CH08 == 9, NA, TRUE)),
    Cobertura_Medica = ifelse(Estado %in% c("Desocupado", "Empleador", "Jubilado", "Rentista") | Segundo_Nivel == "Directivo" | Primer_Nivel == "Empleador", NA, Cobertura_Medica),
    Nivel_Educativo_Jefe = case_when(
      NIVEL_ED %in% c(1,7) ~ 1,
      NIVEL_ED %in% c(2,3) ~ 2,
      NIVEL_ED %in% c(4,5) ~ 3,
      NIVEL_ED == 6 ~ 4
    )
  )

 hogares <- hogares %>%
  ungroup() %>% 
  left_join(jefes_hogares, by = c("CODUSU", "NRO_HOGAR")) %>%
  mutate(
    Proporcion_Aportantes = Cantidad_Aportantes / IX_TOT,
    AP = ifelse(Proporcion_Aportantes <= 0.4, "AP1", ifelse(Proporcion_Aportantes < 0.7, "AP2", "AP3"))
  ) %>%
  left_join(Cuadros, by = c("Primer_Nivel", "Segundo_Nivel", "Tercer_Nivel", "Cobertura_Medica", "AP", "Nivel_Educativo_Jefe", "Estado"))

hogares %>% 
  filter(is.na(Valor)) %>% 
  count(Primer_Nivel, Segundo_Nivel, Tercer_Nivel, Estado) %>% 
  arrange(desc(n))

hogares %>% 
  count(Valor)

hogares %>%
  filter(Primer_Nivel == "Empleado") %>% 
  count(Segundo_Nivel, Tercer_Nivel)

View(Cuadros %>% filter(Primer_Nivel == "Empleado", Segundo_Nivel == "Trabajdor", Tercer_Nivel== "Tecnico"))

hogares %>% filter(is.na(Valor)) %>% count(Primer_Nivel, Segundo_Nivel, Tercer_Nivel) %>% arrange(desc(n))
