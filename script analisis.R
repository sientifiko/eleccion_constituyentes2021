library(tidyverse)
library(rvest)
library(RSelenium)
library(casen)
library(survey)
library(readxl)
library(convey)
library(srvyr)

theme_set(  theme_classic() )

options(scipen = 999)

# ============================= LIMPIEZA DE DATOS ========================================
# FUNCION PARA OBTENER INICIALES DE UN NOMBRE
iniciales <- function(x){
  lapply(x, function(.x){
    .x <- str_split(.x, pattern = " ")[[1]]
    str_sub(.x, 1L, 1L) %>% 
      paste0(collapse = "")
  }) %>% 
    as.character()
}


# FUNCIÓN PARA FORMATEAR COMUNAS
formatear_comuna <- function(x){
  
  codigos_subdere$codigo_comuna <- as.numeric(paste0(codigos_subdere$codigo_comuna))
  
  if(!is.numeric(x)){
    x <- as.numeric(paste0(x))
  }
  
  x <- data.frame(comuna = x) %>%
    inner_join(codigos_subdere, by= c("comuna"="codigo_comuna")) %>%
    select(nombre_comuna)
  
  iconv(x[,1],
        from = 'UTF-8', 
        to = 'ASCII//TRANSLIT') %>%
    toupper() -> x.format
  
  x.format <- case_when(
    x.format == "AISEN" ~ "AYSEN",
    T ~ x.format
  )
  
  return(x.format)
}



# NORMALIZANDO DATOS SIN PUEBLOS INDIGENAS
paste0("DISTRITOS/", list.files("DISTRITOS/")) -> archivos
lista <- list()

for(i in 1:length(archivos)){
  
  print(paste("Iteracion:", i))
  temp <- read_xlsx(archivos[i], sheet = 4) %>%
    select(1, 2, 6, 7, 12, `Dirección del local`:Inscritos) %>%
    filter(!is.na(Región)) %>%
    gather("candidatos", "votos", 7:(ncol(.)-3))
  
  lista[[i]] <- temp
}

rm(temp, i, archivos)

distritos <- do.call("rbind", lista) %>% as.data.frame()


# NORMALIZANDO RESTO DE PUEBLOS INDIGENAS

# aymaras
aimaras <- read_excel("INDIGENAS/Aimara.xlsx", sheet = 4) %>%
  select(1, 2, 6, 7, 12, `Dirección del local`:EXCESO) %>%
  filter(!is.na(Región)) %>%
  gather("candidatos", "votos", 7:(ncol(.)-6))

# mapuches
mapuche <- read_xlsx("INDIGENAS/Mapuche.xlsx", sheet = 4) %>%
  select(1, 2, 6, 7, 12, `Dirección del local`:Inscritos) %>%
  filter(!is.na(Región)) %>%
  gather("candidatos", "votos", 7:(ncol(.)-3))


# rapanui y otros (sheet 4 a 11)
lista <- list()

for (i in 4:11) {
  
  temp <- read_excel("INDIGENAS/Todos-los-PI-excepto-Mapuche-y-Aimara.xlsx", sheet = i)  %>%
    select(1, 2, 6, 7, 12, `Dirección del local`:Inscritos) %>%
    filter(!is.na(Región)) %>%
    gather("candidatos", "votos", 7:(ncol(.)-3))
  
  lista[[i-3]] <- temp
  
}

rm(temp, i)

otros <- do.call("rbind", lista) %>% as.data.frame()


# procesando listas (BORRAR LOS CAMPOS DE LISTAS, Y NORMALIZAR USANDO EXCEL DE CANDIDATURAS)
listas <- distritos %>%
  group_by(candidatos) %>%
  summarise(n = sum(votos, na.rm = T)) %>%
  filter(n == 0) %>%
  select(candidatos) %>%
  pull()
   

distritos <- distritos %>%
  filter(!(candidatos %in% listas))


# importando hoja con listas inscritas
pactos <- read_xlsx("candidaturas_convencionales_gobernadores_alcaldes_y_concejales_2021.xlsx", sheet = 1) %>%
  filter(eleccion == "CONVENCIONALES CONSTITUYENTES" | 
         eleccion == "CONVENCIONALES CONSTITUYENTES - PUEBLOS INDIGENAS") %>%
  select(1:5, 12)

p_indigenas <- pactos %>% filter(eleccion == "CONVENCIONALES CONSTITUYENTES - PUEBLOS INDIGENAS")
p_noindigenas <- pactos %>% filter(eleccion == "CONVENCIONALES CONSTITUYENTES")



# NORMALOIZANDO CANDIDATURAS NO INDIGENAS
p_noindigenas$iniciales <- iniciales(p_noindigenas$`candidato(a)`)

distritos$iniciales <- iniciales(distritos$candidatos)

p_noindigenas$territorio %>%
  str_extract("\\d+") %>%
  as.numeric() -> p_noindigenas$n_distrito

p_noindigenas$id <- paste0(p_noindigenas$regionid, p_noindigenas$iniciales, p_noindigenas$n_distrito)
noindigena_listaid <- p_noindigenas[, c(9, 6)]

noindigena_listaid %>%
  group_by(id) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  select(id) %>%
  pull() 

distritos$id <- paste0(distritos$`Número Región`, distritos$iniciales, distritos$`Número Distrito...7`)

distritos2 <- distritos %>%
  left_join(noindigena_listaid, by = "id")

distritos2$pacto[is.na(distritos2$pacto)] <- "IND FUERA DE PACTO"

# archivo es demasiado pesado para cargarlo
# write_rds(distritos2, "NI_candidatos_pactos.rds")


# NORMALIZANDO CANDIDATURAS INDIGENAS (PENDIENTE)

mapuche$candidatos <- gsub("\\s*\\([^\\)]+\\)","", mapuche$candidatos)
mapuche$iniciales <- iniciales(mapuche$candidatos)
mapuche$id <- paste0()

colnames(mapuche)
colnames(aimaras)
colnames(otros)

sum(otros$votos, na.rm = T)

p_indigenas$iniciales <- iniciales(p_indigenas$`candidato(a)`)
p_indigenas$id <- paste0(p_indigenas$iniciales, p_indigenas$regionid)



# NORMALIZANDO DISTRITOS

delpueblo <- c("A PULSO, POR EL BUEN VIVIR (D27)", 
               "ASAMBLEA CONSTITUYENTE ATACAMA")

distritos2$pacto2 <- case_when(
  str_detect(distritos2$pacto , 
             regex("DEL PUEBLO")) ~ "Del pueblo",
  distritos2$pacto %in% delpueblo ~ "Del pueblo",
  str_detect(distritos2$pacto,
             regex("NUEVA CONSTITUCION")) ~ "Nueva Constitucion",
  str_detect(distritos2$pacto,
             regex("NO NEUTRALES")) ~ "Nueva Constitucion",
  distritos2$pacto == "VAMOS POR CHILE" ~ "Vamos por Chile",
  distritos2$pacto == "LISTA DEL APRUEBO" ~ "Lista del apruebo",
  distritos2$pacto == "APRUEBO DIGNIDAD" ~ "Apruebo Dignidad",
  T ~ "Otros"
)



distritos2 %>% 
  group_by(pacto2) %>%
  summarize(n = n())


resumen_cdidat <- distritos2 %>%
  group_by(candidatos, `Número Distrito...7`,  Comuna, pacto2) %>%
  summarize(total_votos = sum(votos, na.rm = T))


total_pacto <- distritos2 %>%
  group_by(pacto2) %>%
  summarize(totalvotos = sum(votos, na.rm = T))


comunas_w <- resumen_cdidat %>%
  group_by(pacto2,`Número Distrito...7`, Comuna) %>%
  summarize(n_candidatos = n(),
            votos = sum(total_votos)) 

comunas_w <- comunas_w %>%
  left_join(total_pacto, by = "pacto2")

comunas_w$votos_w <- comunas_w$votos * (comunas_w$votos/comunas_w$totalvotos)

comunas_w$pactoid <- paste0(comunas_w$pacto2, comunas_w$`Número Distrito...7`)

distrito_w <- distritos2 %>%
  group_by(pacto2,`Número Distrito...7`) %>%
  summarize(candidatos = length(unique(candidatos))) %>%
  mutate(p_candidatos= candidatos/sum(candidatos)) %>%
  as.data.frame()

distrito_w$pactoid <- paste0(distrito_w$pacto2, distrito_w$`Número Distrito...7`)
distrito_w <- distrito_w %>% select(5, 4)

comunas_w <- comunas_w %>%
  left_join(distrito_w, by = "pactoid")


comunas_w$votos_w2 <- comunas_w$votos_w * comunas_w$p_candidatos

total_w2 <- comunas_w %>%
  group_by(pacto2) %>%
  summarise(total_w = sum(votos_w2))

comunas_w <- comunas_w %>%
  left_join(total_w2, by = "pacto2")


comunas_w$indice_electoral <- comunas_w$votos_w2 / comunas_w$total_w


rm(total_pacto, distrito_w, total_w2, lista, delpueblo, distritos, resumen_cdidat)


write.csv(comunas_w, "votacion_comunas.csv", row.names = F)



# =================  DESDE ACÁ ANÁLISIS =================

casen <- read_rds("2017.rds")


casen$comuna <- as.numeric(casen$comuna)
casen$pobreza_multi_4d <- as.factor(casen$pobreza_multi_4d)

diseno <- casen %>% 
  as_survey_design( strata = varstrat,
                    id =  varunit,
                    weights = expc)

prep <- convey_prep(diseno)

gini_comunas <- svyby(~ytotcorh, 
             by=~comuna, 
             design = prep,
             FUN= svygini,
             deff = FALSE)

gini_comunas$comuna <- formatear_comuna(gini_comunas$comuna)

colnames(gini_comunas)[2] <- "gini"

mediana_ingreso <- diseno %>%
  group_by(comuna) %>%
  summarise(median = survey_median(ytotcorh, na.rm = T))

mediana_ingreso$comuna <- formatear_comuna(as.character(mediana_ingreso$comuna))


pobreza <- diseno %>%
   group_by(comuna, pobreza_multi_4d) %>%
   summarize(p = survey_mean()) %>%
   select(comuna, pobreza_multi_4d, p) %>%
   spread(pobreza_multi_4d, p)

pobreza$comuna <- formatear_comuna(pobreza$comuna)

colnames(pobreza)[3] <- "p_pobreza"


df <- comunas_w %>%
  inner_join(mediana_ingreso, by = c("Comuna"="comuna"))  %>%
  inner_join(gini_comunas, by = c("Comuna"="comuna")) %>%
  inner_join(pobreza, by = c("Comuna"="comuna"))


ggplot(df, aes(median, indice_electoral, color = pacto2)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  # scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto2) +
  labs(x="Mediana comunal del ingreso total del hogar 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y nivel de ingresos",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")

ggplot(df, aes(gini, indice_electoral, color = pacto2)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  # scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto2) +
  labs(x="Gini del ingreso total del hogar 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y desigualdad",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")


ggplot(df, aes(p_pobreza, indice_electoral, color = pacto2)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  # scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto2) +
  labs(x="% pobreza comunal 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y pobreza",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")


pactos <- unique(df$pacto2)

lista.temp <- list()

intr <- c("<20%", "20% - 40%", "40% - 60%", "60% - 80%", ">80%")

intervalos <- factor(intr,
                     levels = c("<20%", "20% - 40%", "40% - 60%", "60% - 80%", ">80%"))


for (i in 1:length(pactos)) {
  
  x <- c()
  
  df %>%
    filter(p_pobreza >= 0,
           p_pobreza < .2,
           pacto2 == pactos[i]) %>%
    ungroup() %>%
    select(indice_electoral) %>%
    unlist() %>%
    mean()  -> x[1]
  
  df %>%
    filter(p_pobreza >= .2,
           p_pobreza < .4,
           pacto2 == pactos[i]) %>%
    ungroup() %>%
    select(indice_electoral) %>%
    unlist() %>%
    mean() -> x[2]
  
  df %>%
    filter(p_pobreza >= .4,
           p_pobreza < .6,
           pacto2 == pactos[i]) %>%
    ungroup() %>%
    select(indice_electoral) %>%
    unlist() %>%
    mean() -> x[3]
  
  
  df %>%
    filter(p_pobreza >= .6,
           p_pobreza < .8,
           pacto2 == pactos[i]) %>%
    ungroup() %>%
    select(indice_electoral) %>%
    unlist() %>%
    mean() -> x[4]
  
  df %>%
    filter(p_pobreza >= .8,
           pacto2 == pactos[i]) %>%
    ungroup() %>%
    select(indice_electoral) %>%
    unlist() %>%
    mean() -> x[5]
  
  lista.temp[[i]] <- data.frame(pacto = pactos[i],
                                interval = intervalos,
                                medias = x)
}

voto_esperado <- do.call("rbind", lista.temp)

rm(i, x, lista.temp)

ggplot(voto_esperado, aes(interval, medias, fill = pacto)) +
  guides(fill = "none") +
  geom_col(position = "dodge") +
  facet_wrap(.~pacto) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1 , hjust = 1)) +
  labs(x="Intérvalos de porcentaje de pobreza comunal 2017",
       y= "Ìndice de desempeño electoral promedio",
       title = "Votos esperados por nivel de pobreza comunal")



# =========== DESEMPEÑO ELECTORAL SECTORES RURALES ============


svytable(~zona+comuna, svy_casen) %>%
  as.data.frame() -> ruralidad

ruralidad$zona <- case_when(
  ruralidad$zona == 1 ~ "Urbano",
  T ~ "Rural"
) 

ruralidad2 <- ruralidad %>%
  spread(zona, Freq)

ruralidad2$comuna <- formatear_comuna(ruralidad2$comuna)

ruralidad2$p_rural <- ruralidad2$Rural/(
  ruralidad2$Rural + ruralidad2$Urbano
)


voto_rural <- comuna_pond %>%
  inner_join(ruralidad2, by = "comuna")


ggplot(voto_rural,
       aes(p_rural, voto_w2, color = pacto)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  # scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto) +
  labs(x="% ruralidad comunal 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y ruralidad",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")










