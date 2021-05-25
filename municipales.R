library(tidyverse)

theme_set(  theme_classic() )

options(scipen = 999)


constituyentes <- read.csv2("Estimacion_Ideologia_Convencionales_Constituyentes_Mayo2021_v2.csv") %>%
  na.omit()

constituyentes$Lista <- toupper(constituyentes$Lista)

constituyentes$lista <- case_when(
  str_detect(constituyentes$Lista, 
             regex("DEL PUEBLO")) ~ "Del pueblo",
  constituyentes$Lista %in% delpueblo ~ "Del pueblo",
  str_detect(constituyentes$Lista,
             regex("NUEVA CONSTITUCION")) ~ "Nueva Constitucion",
  str_detect(constituyentes$Lista,
             regex("NO NEUTRALES")) ~ "Nueva Constitucion",
  constituyentes$Lista == "VAMOS POR CHILE" ~ "Vamos por Chile",
  constituyentes$Lista == "LISTA DEL APRUEBO" ~ "Lista del apruebo",
  constituyentes$Lista == "APRUEBO DIGNIDAD" ~ "Apruebo Dignidad",
  T ~ "Otros"
)


ggplot(constituyentes, aes(Media.ideológica.estimada)) +
  geom_density() +
  geom_vline(xintercept = 0) +
  annotate("text", x= -1.950191, y = .3, label = "J. Bassa") +
  annotate("text", x= 2.177641320, y = .3, label = "M. Cubillos") +
  annotate("text", x= 0.064501203, y = .1, label = "A. Squella") +
  labs(x="Media ideológica estimada", 
       title = "Ideología estimada de miembros de la Convención",
       subtitle = paste("N: ", nrow(constituyentes))) 



ggplot(constituyentes, aes(Media.ideológica.estimada)) +
  geom_density() +
  geom_vline(xintercept = 0) +
  facet_wrap(.~lista, ncol = 2) +
  labs(x="Media ideológica estimada", 
       title = "Ideología estimada de miembros de la Convención por lista",
       subtitle = paste("N: ", nrow(constituyentes))) 



constituyentes %>%
  filter(lista == "Vamos por Chile") %>%
  select(Media.ideológica.estimada) %>%
  unlist() %>%
  min() -> min.derecha

min.derecha

constituyentes %>%
  group_by(lista) %>%
  summarize(p.derecha = mean(Media.ideológica.estimada >= min.derecha))


constituyentes %>%
  group_by(lista) %>%
  summarize(n = n())


alcaldias <- read_excel("var alcaldias.xlsx", sheet = 1)

alcald.s <- alcaldias %>%
  na.omit() %>%
  gather("anno", "electos", c(9, 6))

alcald.s$anno <- str_extract(alcald.s$anno, "\\d+")


alcald.s$orientacion <- as.factor(alcald.s$orientacion)

alcald.s$orientacion <- factor(alcald.s$orientacion,
                               levels(alcald.s$orientacion)[c(3, 1, 2)])


ggplot(alcald.s, aes(str_wrap(partido, 15), electos, fill = as.factor(anno) )) +
  geom_col(position = "dodge") +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip() +
  geom_text(aes(label = electos), 
            position = position_dodge(width = 1)) +
  facet_wrap(.~orientacion, scales = "free_y") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x= "", y="", 
       title = "Cantidad de alcaldes electos",
       fill = "Año")



guides(fill = guide_legend(reverse = TRUE))


exito <- alcaldias %>%
  group_by(partido) %>%
  summarize(candidatos_t = sum(candidatos),
            electos = sum(electo2021)) %>%
  mutate(exito = electos/candidatos_t)

ggplot(exito %>% filter(candidatos_t >= 10), aes(reorder(partido, exito), 
                  exito, 
                  fill = partido)) +
  guides(fill = "none") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y ="", title = "Tasa de éxito alcaldes por partido")
















