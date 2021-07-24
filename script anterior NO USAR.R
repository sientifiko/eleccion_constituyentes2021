library(tidyverse)
library(rvest)
library(RSelenium)

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