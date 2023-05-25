library(tidyverse)

hogares <-  read_csv2("RawData/usu_hogar_T322.txt")

# Análisis exploratorio de la base ----------------------------------------
hogares <- hogares %>% 
  select(V4, IV3, IV3_ESP, IV5) %>%
  rename(Techo = V4, Piso = IV3, PisoEsp = IV3_ESP, Cieloraso = IV5)

hogares %>% pull(Techo) %>% unique()
hogares %>% pull(Piso) %>% unique()
hogares %>% pull(Cieloraso) %>% unique()

hogares %>% ggplot(aes(x = Techo, y = stat(prop)))+
  geom_bar()

hogares %>% ggplot(aes(x = Piso, y = stat(prop)))+
  geom_bar()

hogares %>% ggplot(aes(x = Cieloraso, y = stat(prop)))+
  geom_bar()

# Creacion de variables auxiliares -------------------------------------------

hogares <- hogares %>% 
  mutate(
    CatPiso  = Piso,
    CatTecho = case_when(
      Cieloraso == 1                             ~ 1,
      Techo %in% c(1, 2, 3, 9)  & Cieloraso == 2 ~ 1,
      Techo %in% c(4, 5)        & Cieloraso == 2 ~ 2,
      Techo %in% c(6, 7)        & Cieloraso == 2 ~ 3
    )
  )

hogares <- hogares %>% 
  mutate(
    CALMAT = case_when(
      CatPiso == 1 & CatTecho == 1                                           ~ 1,
      (CatPiso == 2 & CatTecho == 1) | (CatPiso %in% c(1,2) & CatTecho == 1) ~ 2,
      CatPiso %in% c(1,2) & CatTecho == 2                                    ~ 3,
      CatPiso == 3 | CatPiso %in% c(1,2) & CatTecho == 3                     ~ 4
    )
  )
# Análisis de resultados --------------------------------------------------

hogares %>% ggplot(aes(x = CatTecho, y = stat(prop)))+
  geom_bar()

hogares %>% ggplot(aes(x = CatPiso, y = stat(prop)))+
  geom_bar()

hogares %>% ggplot(aes(x = CALMAT, y = stat(prop)))+
  geom_bar()

count(hogares, CatPiso) %>% 
  mutate(prop = n/sum(n)*100)

count(hogares, CALMAT) %>% 
  mutate(prop = n/sum(n)*100)
