library(tidyverse)
library(rvest)
library(RSelenium)
library(casen)
library(survey)

theme_set(  theme_classic() )

options(scipen = 999)

# funciones pa limpiar
limpieza <- function(objtabla){
  objtabla <- objtabla[-which(is.na(objtabla[,1])), ]
  
  objtabla <- objtabla[c(1:(nrow(objtabla)-5)),]
  
  objtabla$Electo <- str_detect(objtabla[, 6], "[*]")
  
  objtabla["NA."] <- NULL
  
  objtabla$pacto <- str_sub(objtabla[,1], 1, 1) %>% 
    as.numeric() 
  
  objtabla$pacto <- if_else(is.na(objtabla$pacto), T, NA)
  
  objtabla[which(objtabla[,6]),6] <- objtabla[which(objtabla[,6]),1]
  
  
  objtabla <- objtabla %>% fill(pacto, .direction = "down") 
  
  objtabla <- objtabla[-which(objtabla[, 2] == ""),]
}

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

url <- "https://www.servelelecciones.cl/"


# configuraciones iniciales
driver <- rsDriver(browser = c("firefox"))

remote_driver <- driver[["client"]]


# abrir el phantomJS
remote_driver$open()

# ir al sitio
remote_driver$navigate(url)

# xpath primera comuna
# //*[@id="selComunas"]/option[2]

# xpath ultima comuna
# //*[@id="selComunas"]/option[346]


lista <- list()

for (i in 2:346) {
  
  # construir el path
  path <- paste0('//*[@id="selComunas"]/option[', i, ']')
  
  
  # ir a la comuna buscada
  remote_driver$findElement(using = "id", 
                value = "selComunas")$findChildElement(using = "xpath", 
                path)$clickElement()
  
  # enfocarse en la comuna elegida pa extraerla
  tempRD <- remote_driver$findElement(using = "id", 
                                      value = "selComunas")
  
  # extraer la comuna
  tempRD$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_node(xpath = path) %>%
    html_text() -> comuna
  
  # enfocarse en la tabla del estado actual
  tabla <- remote_driver$findElement("id", "divVotacion")
  
  # sacarle las cosas a esa tabla
  objtabla <- tabla$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="basic-table"]/table') %>%
    html_table(fill = T) %>%
    as.data.frame() 
  
  # limpiarla
  temptable <- limpieza(objtabla)
  
  # imprimir comuna en la que se va
  print(paste("Comuna: ", comuna))
  
  # pasarle la comuna
  temptable$comuna <- comuna
  
  # acumular en la lista
  lista[[i-1]] <- temptable
  
  # descansar pa que no nos boten
  Sys.sleep(3)
}

rm(i, objtabla, tempRD, tabla, temptable, path, comuna)

remote_driver$close()
remote_driver$closeServer()
remote_driver$closeall()

rm(remote_driver, driver)

consolidado <- do.call("rbind", lista)

colnames(consolidado) <- c("candidato", "partido", "votos", "porcentaje", "electo",
                           "lista", "comuna")

rownames(consolidado) <- 1:nrow(consolidado)

consolidado$comuna <- iconv(consolidado$comuna,
                            from = 'UTF-8', 
                            to = 'ASCII//TRANSLIT')

consolidado$porcentaje <- consolidado$porcentaje %>%
  str_replace("%", "") %>%
  str_replace(",", ".") %>%
  as.numeric()

consolidado$porcentaje <- consolidado$porcentaje/100


# AGREGANDO MÁS DATOS
consolidado$lista %>%
  sub('.*\\.', '', .) %>%
  str_trim() %>%
  toupper() -> consolidado$lista

delpueblo <- c("A PULSO, POR EL BUEN VIVIR (D27)", 
               "ASAMBLEA CONSTITUYENTE ATACAMA")

consolidado$pacto <- case_when(
  str_detect(consolidado$lista, 
             regex("DEL PUEBLO")) ~ "Del pueblo",
  consolidado$lista %in% delpueblo ~ "Del pueblo",
  str_detect(consolidado$lista,
             regex("NUEVA CONSTITUCION")) ~ "Nueva Constitucion",
  str_detect(consolidado$lista,
             regex("NO NEUTRALES")) ~ "Nueva Constitucion",
  consolidado$lista == "VAMOS POR CHILE" ~ "Vamos por Chile",
  consolidado$lista == "LISTA DEL APRUEBO" ~ "Lista del apruebo",
  consolidado$lista == "APRUEBO DIGNIDAD" ~ "Apruebo Dignidad",
  T ~ "Otros"
)

write.csv(consolidado, "votacion_comuna.csv", row.names = F)

# consolidado <- read.csv("votacion_comuna.csv")

casen <- read_rds("2017.rds")

diseno1 <- configuracion_disenio(casen, "ytotcorh", "comuna", "expc")

mediana_ingreso <- mediana_agrupada(diseno1) %>%
  na.omit() 

mediana_ingreso$comuna <- formatear_comuna(mediana_ingreso$comuna_codigo)



comuna_pond <- consolidado %>%
  group_by(pacto, comuna) %>%
  summarize(votacion = sum(votos),
            n_candidatos = n()) %>%
  mutate(p_votacion = votacion/sum(votacion)) %>%
  mutate(voto_w = votacion * p_votacion,
         p_candidatos = n_candidatos/sum(n_candidatos)) %>%
  mutate(voto_w2 = voto_w * p_candidatos)


inc_voto_pond <- comuna_pond %>%
  inner_join(mediana_ingreso, by = "comuna")


ggplot(inc_voto_pond, aes(mediana_ytotcorh, voto_w2, color = pacto)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  # scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto) +
  labs(x="Mediana comunal del ingreso total del hogar 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y nivel de ingresos",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")


svy_casen <- svydesign(data =  casen,
                       ids = ~varunit,
                       strata = ~varstrat,
                       weights = ~expc,
                       nest = T)

svytable(~pobreza_multi_4d+comuna, svy_casen) %>%
  as.data.frame() -> pobreza_comuna

pobreza_comuna$pobreza_multi_4d <- case_when(
  pobreza_comuna$pobreza_multi_4d == 1 ~ "pobres",
  T ~ "no_pobres"
) 

pobreza_comuna2 <- pobreza_comuna %>%
  spread(pobreza_multi_4d, Freq)

pobreza_comuna2$comuna <- formatear_comuna(pobreza_comuna2$comuna)

pobreza_comuna2$p_pobreza <- pobreza_comuna2$pobres/(pobreza_comuna2$no_pobres + pobreza_comuna2$pobres)


voto_pobreza <- comuna_pond %>%
  inner_join(pobreza_comuna2, by = "comuna")


ggplot(voto_pobreza, aes(p_pobreza, voto_w2, color = pacto)) +
  theme_classic() +
  guides(color = "none") +
  geom_jitter() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  # scale_x_continuous(trans = "log10") +
  facet_wrap(.~pacto) +
  labs(x="% pobreza comunal 2017", 
       y="Índice de desempeño electoral",
       title = "Votación y pobreza",
       subtitle = "Voto comunal ponderado por cantidad de votos y candidatos a nivel nacional",
       caption = "Elaborado por @sientifiko1")


pactos <- unique(voto_pobreza$pacto)

lista.temp <- list()

intr <- c("<20%", "20% - 40%", "40% - 60%", "60% - 80%", ">80%")

intervalos <- factor(intr,
                     levels = c("<20%", "20% - 40%", "40% - 60%", "60% - 80%", ">80%"))


for (i in 1:length(pactos)) {
  
  x <- c()
  
  voto_pobreza %>%
    filter(p_pobreza >= 0,
           p_pobreza < .2,
           pacto == pactos[i]) %>%
    ungroup() %>%
    select(voto_w2) %>%
    unlist() %>%
    mean()  -> x[1]
  
  voto_pobreza %>%
    filter(p_pobreza >= .2,
           p_pobreza < .4,
           pacto == pactos[i]) %>%
    ungroup() %>%
    select(voto_w2) %>%
    unlist() %>%
    mean() -> x[2]
  
  voto_pobreza %>%
    filter(p_pobreza >= .4,
           p_pobreza < .6,
           pacto == pactos[i]) %>%
    ungroup() %>%
    select(voto_w2) %>%
    unlist() %>%
    mean() -> x[3]
  
  
  voto_pobreza %>%
    filter(p_pobreza >= .6,
           p_pobreza < .8,
           pacto == pactos[i]) %>%
    ungroup() %>%
    select(voto_w2) %>%
    unlist() %>%
    mean() -> x[4]
  
  voto_pobreza %>%
    filter(p_pobreza >= .8,
           pacto == pactos[i]) %>%
    ungroup() %>%
    select(voto_w2) %>%
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










