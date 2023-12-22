
library(tidyverse)
library(readxl)
library(ggplot2)
library(forcats)
library(viridis)
library(ggrepel)
library(scales)
library(openxlsx)
library(RColorBrewer)

#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Importo la base en que unifiqué RA para INGRESANTES y MATRÍCULA ####

ingresantes_y_matricula_RA_total_grado <- read_excel("99 - Ingresantes y Matricula RA 2015-2023 - Egresados RA 2015-2022.xlsx", sheet = "Ingr y Matr RA 2015-2023")

## Le corro los procesamientos necesarios para poder trabajarla ##

# Saco TEOLOGÍA y todos los que designé como xxx que no nos van a interesar para el informe
# Elimino el año 2015 porque no traía datos de artística

ingresantes_y_matricula_RA_total_grado <- ingresantes_y_matricula_RA_total_grado %>%
  filter(`N - Titulo Facu Agrupado` != "xxx") %>% 
  filter(Año > 2015) %>% 
  mutate(`N - Titulo Facu Nivel` = case_when( `N - Titulo Facu Nivel` == "Nivel Secundario y/o superior" ~ "Nivel Secundario y o superior",
                                              TRUE  ~ `N - Titulo Facu Nivel`))

# Ordeno la variable "N - Titulo Facu Agrupado"

ingresantes_y_matricula_RA_total_grado <- ingresantes_y_matricula_RA_total_grado %>% 
  mutate(`N - Titulo Facu Agrupado` = fct_relevel(`N - Titulo Facu Agrupado`,
                                                  "Educación Inicial", "Educación Primaria", "Lengua y Literatura",	"Matemática",	
                                                  "Química",	"Física",	"Biología",	"Ciencia Política",	"Ciencias de la Administración",	
                                                  "Ciencias Jurídicas",	"Ciencias de la Educación",	"Economía",	"Filosofía",	"Geografía",	
                                                  "Historia",	"Psicología",	"Informática", "TIC","Educación Física",	"Música",	"Danza",	
                                                  "Artes visuales",	"Teatro",	"Expresión corporal",	"Educación Especial",	
                                                  "Educación tecnológica",	 "Inglés",	"Italiano",	"Portugues",	"Alemán",	"Francés"))

# Ordeno la variable "N - Titulo Facu Nivel" 

ingresantes_y_matricula_RA_total_grado <- ingresantes_y_matricula_RA_total_grado %>% 
  mutate(`N - Titulo Facu Nivel` = fct_relevel(`N - Titulo Facu Nivel`,
                                               "Nivel inicial", "Nivel primaria","Nivel Secundario y o superior",
                                               "Multinivel"))

# Ordeno la variable "Gestión" 

ingresantes_y_matricula_RA_total_grado <- ingresantes_y_matricula_RA_total_grado %>% 
  mutate(`Gestión` = fct_relevel(`Gestión`,"Estatal",
                                 "Privada"))

# Designo colores para variable "Gestion"

color_gestion <- c("Estatal"="lightblue", "Privada"="steelblue")

# Designo colores para variable N - Titulo Facu Nivel

color_nivel <- c("Nivel inicial"="darksalmon", "Nivel primaria"="chocolate3", 
                 "Nivel Secundario y o superior"="red", "Multinivel"="darkred")



#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Trabajo con MATRÍCULA estatal y privada en base a la unificación de RA ####

# Genero tabla dinámica con la MATRÍCULA de GESTIÓN ESTATAL y PRIVADA 2016 - 2023 por Año

ev_matricula_RA_total_grado_por_gestion <- ingresantes_y_matricula_RA_total_grado %>%
  group_by(Gestión, Año) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>%
  view()

#----------------------------------------------------------------------------------------------------------.

## Grafico Matrícula 1 ## 

TD_matricula_1 <- ev_matricula_RA_total_grado_por_gestion %>%
  filter(Año == 2023) %>% 
  mutate(prop_matri_prof = percent(matri_prof/sum(matri_prof), accuracy = 0.1)) %>% 
  pivot_wider(#id_cols = matri_prof,
    names_from="Gestión",
    values_from=c(matri_prof, prop_matri_prof)) %>% 
  rename("Matrícula Estatal" = matri_prof_Estatal) %>% 
  rename("Matrícula Privada" = matri_prof_Privada) %>% 
  rename("Proporción Matrícula Estatal" = prop_matri_prof_Estatal) %>% 
  rename("Proporción Matrícula Privada" = prop_matri_prof_Privada) %>% 
  select("Matrícula Estatal","Proporción Matrícula Estatal", "Matrícula Privada", "Proporción Matrícula Privada") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_matricula_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_matricula_1 <- 
  ggplot(filter(ev_matricula_RA_total_grado_por_gestion, Año == 2023),
         aes(x=2, y=matri_prof, fill=Gestión))+
  geom_col()+
  geom_text(aes(label=percent(matri_prof/sum(matri_prof))),
            position=position_stack(vjust=0.5),color="black",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = color_gestion) +
  xlim(0.5,2.5)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Proporción de la matrícula por gestión",
       subtitle = "Para año 2023",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_matricula_1.pdf", grafico_matricula_1, width = 8, height = 6)

## Grafico Matrícula 2 ## 

ev_matricula_RA_total_grado_por_gestion_2 <- ingresantes_y_matricula_RA_total_grado %>%
  group_by(Gestión, Año, `N - Titulo Facu Agrupado`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>%
  view()

TD_matricula_2 <- ev_matricula_RA_total_grado_por_gestion_2 %>%
  filter(Año == 2023) %>% 
  pivot_wider(#id_cols = `N - Titulo Facu Agrupado`,
    names_from="Gestión",
    values_from=c(matri_prof)) %>%
  arrange(`N - Titulo Facu Agrupado`) %>% 
  mutate("Proporción Matrícula Estatal" = percent(Estatal / (Estatal + Privada),accuracy = 0.1)) %>% 
  mutate("Proporción Matrícula Privada" = percent(Privada / (Estatal + Privada),accuracy = 0.1)) %>% 
  select(-Año) %>% 
  rename("Carrera Agrupada" = `N - Titulo Facu Agrupado`) %>% 
  rename("Matricula Estatal" = Estatal) %>% 
  rename("Matricula Privada" = Privada) %>% 
  view() %>%  
  write.xlsx("Resultados/TD_matricula_2.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_matricula_2 <- 
  ggplot(filter(ev_matricula_RA_total_grado_por_gestion_2, Año == 2023),
         aes(x=matri_prof, y=reorder(`N - Titulo Facu Agrupado`, desc(`N - Titulo Facu Agrupado`)),
             fill=Gestión, sort.val = ""))+
  geom_col()+
  scale_fill_manual(values = color_gestion)+
  #scale_x_continuous()+
  #scale_x_log10()+
  #guides(x="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Matrícula por carrera según gestión",
       subtitle = "Para año 2023",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_matricula_2.pdf", grafico_matricula_2, width = 8, height = 6)

#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Concentración y dispersión de la oferta ####

## Gráfico concentración y dispersion de la oferta 1 ##

ev_matricula_RA_total_grado_por_gestion_3 <- ingresantes_y_matricula_RA_total_grado %>%
  filter(Año == 2023) %>%
  group_by(Gestión, `N - Titulo Facu Agrupado`, `N - Establecimiento Facu`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

ev_matricula_RA_total_grado_por_gestion_3.1 <-  ev_matricula_RA_total_grado_por_gestion_3%>%
  group_by(Gestión, `N - Titulo Facu Agrupado`) %>% 
  summarise(matri_prof = sum(matri_prof , na.rm = TRUE)) 

ev_matricula_RA_total_grado_por_gestion_3.2 <-  ev_matricula_RA_total_grado_por_gestion_3%>%
  group_by(Gestión, `N - Titulo Facu Agrupado`) %>% 
  summarise(cant_estab = n()) 

ev_matricula_RA_total_grado_por_gestion_3.3 <- ev_matricula_RA_total_grado_por_gestion_3.1 %>% 
  left_join(ev_matricula_RA_total_grado_por_gestion_3.2, by = c("Gestión" = "Gestión", 
                                                                "N - Titulo Facu Agrupado" = "N - Titulo Facu Agrupado")) %>% 
  mutate(prop_matri_estab = round(matri_prof/cant_estab,1)) 

TD_concetracion_1 <- ev_matricula_RA_total_grado_por_gestion_3.3 %>%
  pivot_wider(#id_cols = matri_prof,
    names_from="Gestión",
    values_from=c(matri_prof, cant_estab, prop_matri_estab)) %>%
  arrange(`N - Titulo Facu Agrupado`) %>% 
  rename("Carrera Agrupada" = `N - Titulo Facu Agrupado`) %>% 
  rename("Matrícula Estatal" = matri_prof_Estatal) %>% 
  rename("Matrícula Privada" = matri_prof_Privada) %>% 
  rename("Cantidad de Establecimientos Estatal" = cant_estab_Estatal) %>% 
  rename("Cantidad de Establecimientos Privada" = cant_estab_Privada) %>%
  rename("Promedio de matrícula por establecimiento y carrera Estatal" = prop_matri_estab_Estatal) %>%
  rename("Promedio de matrícula por establecimiento y carrera Privada" = prop_matri_estab_Privada) %>%
  select("Carrera Agrupada","Matrícula Estatal","Cantidad de Establecimientos Estatal",
         "Promedio de matrícula por establecimiento y carrera Estatal", "Matrícula Privada",
         "Cantidad de Establecimientos Privada", "Promedio de matrícula por establecimiento y carrera Privada") %>% 
  view() %>%  
  write.xlsx("Resultados/TD_concentracion_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

ev_matricula_RA_total_grado_por_gestion_3.4 <- ev_matricula_RA_total_grado_por_gestion_3.3 %>% 
  mutate(prop_matri_estab = ifelse(Gestión == "Privada",prop_matri_estab*-1,as.numeric(paste0(prop_matri_estab))))

grafico_concentracion_1 <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_3.4,
         aes(y=prop_matri_estab, x=reorder(`N - Titulo Facu Agrupado`,desc(`N - Titulo Facu Agrupado`)), color=Gestión))+
  geom_segment(aes(xend=`N - Titulo Facu Agrupado`, yend=0), size=1) +
  geom_point(size=1.2, color="grey") +
  coord_flip() +
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Estatal"),
            aes(label=prop_matri_estab),
            hjust=-0.5,vjust=0.3,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Privada"),
            aes(label=prop_matri_estab*-1),
            hjust=1.5,vjust=0.3,color="black",size=2)+
  #scale_y_log10()+
  scale_color_manual(values = color_gestion)+
  ylim(-2000,2000)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Promedio de matrícula por establecimiento y carrera",
       subtitle = "Según gestión para año 2023",
       x = "",
       y = "",
       caption = "Nota: los valores provistos resultan de tomar la matrícula total de la carrera y dividirla por la cantidad
de establecimientos que la dictan
Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_1.pdf", grafico_concentracion_1, width = 8, height = 6)


grafico_concentracion_1.Y <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_3.4,
         aes(y=prop_matri_estab, x=reorder(`N - Titulo Facu Agrupado`,desc(`N - Titulo Facu Agrupado`)), color=Gestión))+
  geom_point()+
  coord_flip() +
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Estatal"),
            aes(label=prop_matri_estab),
            hjust=-0.5,vjust=0.3,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Privada"),
            aes(label=prop_matri_estab*-1),
            hjust=1.5,vjust=0.3,color="black",size=2)+
  #scale_y_log10()+
  scale_color_manual(values = color_gestion)+
  ylim(-2000,2000)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Promedio de matrícula por establecimiento y carrera",
       subtitle = "Según gestión para año 2023",
       x = "",
       y = "",
       caption = "Nota: los valores provistos resultan de tomar la matrícula total de la carrera y dividirla por la cantidad
  de establecimientos que la dictan
  Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_1.Y.pdf", grafico_concentracion_1.Y, width = 8, height = 6)


grafico_concentracion_1.Z <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_3.4,
         aes(y=prop_matri_estab, x=reorder(`N - Titulo Facu Agrupado`,desc(`N - Titulo Facu Agrupado`)), color=Gestión))+
    geom_point()+
    geom_segment(aes(x="Educación Inicial", y=0, xend="Francés", yend=0), colour = "darkgrey", size = 0.5, linetype=3)+
    coord_flip() +
    geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Estatal"),
              aes(label=prop_matri_estab),
              hjust=-0.5,vjust=0.3,color="black",size=2)+
    geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.4 %>% filter(Gestión == "Privada"),
              aes(label=prop_matri_estab*-1),
              hjust=1.5,vjust=0.3,color="black",size=2)+
    #scale_y_log10()+
    scale_color_manual(values = color_gestion)+
    ylim(-2000,2000)+
    theme_minimal()+
    theme(axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0)
    )+
    labs(title = "Promedio de matrícula por establecimiento y carrera",
         subtitle = "Según gestión para año 2023",
         x = "",
         y = "",
         caption = "Nota: los valores provistos resultan de tomar la matrícula total de la carrera y dividirla por la cantidad
  de establecimientos que la dictan
  Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_1.Z.pdf", grafico_concentracion_1.Z, width = 8, height = 6)


#----------------------------------------------------------------------------------------------------------.

## Gráfico concentración y dispersion de la oferta 1.1 (max, min y median estatal) ##

ev_matricula_RA_total_grado_por_gestion_3.5 <-  ev_matricula_RA_total_grado_por_gestion_3%>%
  group_by(Gestión, `N - Titulo Facu Agrupado`) %>% 
  summarise(mediana_matri_prof = median(matri_prof, na.rm = TRUE),
            max_matri_prof = max(matri_prof , na.rm = TRUE),
            min_matri_prof = min(matri_prof , na.rm = TRUE))%>% 
  ungroup() 

ev_matricula_RA_total_grado_por_gestion_3.5 <- ev_matricula_RA_total_grado_por_gestion_3.5 %>% 
  mutate(min_matri_prof_2 = case_when(min_matri_prof == max_matri_prof ~ 0,
                                      TRUE ~ min_matri_prof)) %>% 
  mutate(max_matri_prof_2 = case_when(min_matri_prof_2 == 0 ~ 0,
                                      TRUE ~ max_matri_prof)) 

ev_matricula_RA_total_grado_por_gestion_3.6 <- ev_matricula_RA_total_grado_por_gestion_3.5 %>% 
  select(Gestión, `N - Titulo Facu Agrupado`, mediana_matri_prof) 

ev_matricula_RA_total_grado_por_gestion_3.7 <- ev_matricula_RA_total_grado_por_gestion_3.5 %>% 
  pivot_longer(cols = c("max_matri_prof_2", "min_matri_prof_2"),
               names_to= "max_min",
               values_to= "cantidad") %>% 
  select(-mediana_matri_prof,-min_matri_prof) %>% 
  filter(cantidad != 0) 

TD_concetracion_1_mediana_estatal <- ev_matricula_RA_total_grado_por_gestion_3.5 %>% 
  filter(Gestión == "Estatal") %>% 
  rename("Carrera Agrupada" = `N - Titulo Facu Agrupado`) %>% 
  rename("Mediana Estatal" = mediana_matri_prof) %>% 
  rename("Máxima Estatal" = max_matri_prof) %>% 
  rename("Mínima Estatal" = min_matri_prof_2) %>% 
  select("Carrera Agrupada", "Mínima Estatal", "Mediana Estatal", "Máxima Estatal") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_concetracion_1_mediana_estatal.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_concentracion_1_mediana_estatal <- 
  ggplot()+
  geom_point(data=filter(ev_matricula_RA_total_grado_por_gestion_3.6, Gestión == "Estatal"),
             aes(
               x=mediana_matri_prof,
               y=reorder(`N - Titulo Facu Agrupado`,desc(`N - Titulo Facu Agrupado`)),
             ),
             size=1, color="red")+
  geom_line(data=filter(ev_matricula_RA_total_grado_por_gestion_3.7, Gestión == "Estatal"),
            aes(x=cantidad, 
                y=`N - Titulo Facu Agrupado`, 
                color=Gestión
            ))+
  geom_point(data=filter(ev_matricula_RA_total_grado_por_gestion_3.7, Gestión == "Estatal"),
             aes(x=cantidad, 
                 y=`N - Titulo Facu Agrupado`, 
                 color=Gestión),
             size=1, color="grey")+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.7 %>% filter(Gestión == "Estatal" & max_min == "max_matri_prof_2"),
            aes(x= cantidad,
                y=`N - Titulo Facu Agrupado`,
                label=cantidad),
            hjust=-0.5,vjust=0,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.7 %>% filter(Gestión == "Estatal" & max_min == "min_matri_prof_2"),
            aes(x= cantidad,
                y=`N - Titulo Facu Agrupado`,
                label=cantidad),
            hjust=1.5,vjust=0,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.6 %>% filter(Gestión == "Estatal"),
            aes(x=mediana_matri_prof,
                y=`N - Titulo Facu Agrupado`,
                label=mediana_matri_prof),
            hjust=0.5,vjust=-0.5,color="black",size=2)+
  #scale_x_log10()+
  scale_color_manual(values = color_gestion)+
  guides(color="none")+
  #xlim(0,2500)+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Matrícula mínima, mediana y máxima por carrera",
       subtitle = "Según gestión estatal para año 2023",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_1_mediana_estatal.pdf", grafico_concentracion_1_mediana_estatal, width = 8, height = 6)

#----------------------------------------------------------------------------------------------------------.

## Gráfico concentración y dispersion de la oferta 1.1 (max, min y median privada) ##

TD_concetracion_1_mediana_privada <- ev_matricula_RA_total_grado_por_gestion_3.5 %>% 
  filter(Gestión == "Privada") %>% 
  rename("Carrera Agrupada" = `N - Titulo Facu Agrupado`) %>% 
  rename("Mediana Privada" = mediana_matri_prof) %>% 
  rename("Máxima Privada" = max_matri_prof) %>% 
  rename("Mínima Privada" = min_matri_prof_2) %>% 
  select("Carrera Agrupada", "Mínima Privada", "Mediana Privada", "Máxima Privada") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_concetracion_1_mediana_privada.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_concentracion_1_mediana_privada <- 
  ggplot()+
  geom_point(data=filter(ev_matricula_RA_total_grado_por_gestion_3.6, Gestión == "Privada"),
             aes(
               x=mediana_matri_prof,
               y=reorder(`N - Titulo Facu Agrupado`,desc(`N - Titulo Facu Agrupado`)),
             ),
             size=1, color="red")+
  geom_line(data=filter(ev_matricula_RA_total_grado_por_gestion_3.7, Gestión == "Privada"),
            aes(x=cantidad, 
                y=`N - Titulo Facu Agrupado`, 
                color=Gestión
            ))+
  geom_point(data=filter(ev_matricula_RA_total_grado_por_gestion_3.7, Gestión == "Privada"),
             aes(x=cantidad, 
                 y=`N - Titulo Facu Agrupado`, 
                 color=Gestión),
             size=1, color="grey")+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.7 %>% filter(Gestión == "Privada" & max_min == "max_matri_prof_2"),
            aes(x= cantidad,
                y=`N - Titulo Facu Agrupado`,
                label=cantidad),
            hjust=-0.5,vjust=0,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.7 %>% filter(Gestión == "Privada" & max_min == "min_matri_prof_2"),
            aes(x= cantidad,
                y=`N - Titulo Facu Agrupado`,
                label=cantidad),
            hjust=1.5,vjust=0,color="black",size=2)+
  geom_text(data=ev_matricula_RA_total_grado_por_gestion_3.6 %>% filter(Gestión == "Privada"),
            aes(x=mediana_matri_prof,
                y=`N - Titulo Facu Agrupado`,
                label=mediana_matri_prof),
            hjust=0.5,vjust=-0.5,color="black",size=2)+
  #scale_x_log10()+
  scale_color_manual(values = color_gestion)+
  guides(color="none")+
  #xlim(0,2500)+
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Matrícula mínima, mediana y máxima por carrera",
       subtitle = "Según gestión privada para año 2023",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_1_mediana_privada.pdf", grafico_concentracion_1_mediana_privada, width = 8, height = 6)

#----------------------------------------------------------------------------------------------------------.

## Gráfico concentración y dispersion de la oferta 2 ##

ev_matricula_RA_total_grado_por_gestion_4 <- ingresantes_y_matricula_RA_total_grado %>%
  filter(Año == 2023) %>%
  group_by(Gestión,`N - Establecimiento Facu`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>%
  view()

## Gráfico concentración y dispersion de la oferta 2 (histograma sin rangos) ##

grafico_concentracion_2 <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_4,
         aes(x=matri_prof, fill=Gestión)) +
  geom_histogram(position="dodge", bins=25)+
  scale_fill_manual(values = color_gestion)+
  guides(y="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Dispersión de los establecimientos según tamaño de matrícula total",
       subtitle = "Para año 2023",
       x = "Matrícula",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_2.pdf", grafico_concentracion_2, width = 8, height = 6)

ev_matricula_RA_total_grado_por_gestion_5 <- ingresantes_y_matricula_RA_total_grado %>%
  filter(Año == 2023) %>%
  group_by(Gestión, `N - Establecimiento Facu`, `N - Titulo Facu Agrupado`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión, `N - Establecimiento Facu`) 

ev_matricula_RA_total_grado_por_gestion_5.1 <-  ev_matricula_RA_total_grado_por_gestion_3%>%
  group_by(Gestión, `N - Establecimiento Facu`) %>% 
  summarise(matri_prof = sum(matri_prof , na.rm = TRUE)) 

ev_matricula_RA_total_grado_por_gestion_5.2 <-  ev_matricula_RA_total_grado_por_gestion_3%>%
  group_by(Gestión, `N - Establecimiento Facu`) %>% 
  summarise(cant_propuesta = n()) 

ev_matricula_RA_total_grado_por_gestion_5.3 <- ev_matricula_RA_total_grado_por_gestion_5.1 %>% 
  left_join(ev_matricula_RA_total_grado_por_gestion_5.2, by = c("Gestión" = "Gestión", 
                                                                "N - Establecimiento Facu" = "N - Establecimiento Facu")) %>% 
  mutate(prop_matri_propuesta = round(matri_prof/cant_propuesta,1)) 

TD_concetracion_2 <- ev_matricula_RA_total_grado_por_gestion_5.3 %>%
  rename("Establecimiento" = `N - Establecimiento Facu`) %>% 
  rename("Matrícula" = matri_prof) %>% 
  rename("Cantidad de propuestas" = cant_propuesta) %>% 
  rename("Promedio de matrícula por carrera y establecimiento" = prop_matri_propuesta) %>%
  view() %>%  
  write.xlsx("Resultados/TD_concentracion_2.xlsx",rowNames = FALSE, fileEncoding = "latin1")




#----------------------------------------------------------------------------------------------------------.

## Gráfico concentración y dispersion de la oferta 2.1.A (densidad con rangos) ##

grafico_concentracion_2.1.A <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_5.3,
         aes(x=prop_matri_propuesta, fill=Gestión))+
  geom_density(alpha = 0.5)+
  scale_fill_manual(values = color_gestion)+
  guides(y="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Dispersión de los establecimientos según promedio de matrícula por cantidad de carreras",
       subtitle = "Para año 2023",
       x = "Matrícula",
       y = "",
       caption = "Nota: los valores provistos resultan de tomar la matrícula total de cada establecimiento y dividirla por la cantidad
de carreras que se dictan en dicho establecimiento
Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_2.1.A.pdf", grafico_concentracion_2.1.A, width = 8, height = 6)

## Gráfico concentración y dispersion de la oferta 2.1.B (histograma) ##

grafico_concentracion_2.1.B <- 
  ggplot(ev_matricula_RA_total_grado_por_gestion_5.3,
         aes(x=prop_matri_propuesta, fill=Gestión))+
  geom_histogram(position="dodge", bins=25)+
  scale_fill_manual(values = color_gestion)+
  guides(y="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Dispersión de los establecimientos según promedio de matrícula por cantidad de carreras",
       subtitle = "Para año 2023",
       x = "Matrícula",
       y = "",
       caption = "Nota: los valores provistos resultan de tomar la matrícula total de cada establecimiento y dividirla por la cantidad
de carreras que se dictan en dicho establecimiento
Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_concentracion_2.1.B.pdf", grafico_concentracion_2.1.B, width = 8, height = 6)

ev_matricula_RA_total_grado_por_gestion_5.4 <- ev_matricula_RA_total_grado_por_gestion_5.3 %>% 
  mutate(rangos = case_when(prop_matri_propuesta <= 200 ~ "Hasta 200 inclusive",
                            prop_matri_propuesta > 200 & prop_matri_propuesta <= 400 ~ "Entre 201 y 400 inclusive",
                            prop_matri_propuesta > 400 & prop_matri_propuesta <= 600 ~ "Entre 401 y 600 inclusive",
                            prop_matri_propuesta > 600 & prop_matri_propuesta <= 800 ~ "Entre 601 y 800 inclusive",
                            prop_matri_propuesta > 800 & prop_matri_propuesta <= 1000 ~ "Entre 801 y 1000 inclusive",
                            prop_matri_propuesta > 1000 & prop_matri_propuesta <= 1200 ~ "Entre 1001 y 1200 inclusive",
                            prop_matri_propuesta > 1200 & prop_matri_propuesta <= 1400 ~ "Entre 1201 y 1400 inclusive",
                            prop_matri_propuesta > 1400 & prop_matri_propuesta <= 1600 ~ "Entre 1401 y 1600 inclusive",
                            prop_matri_propuesta > 1600 & prop_matri_propuesta <= 1800 ~ "Entre 1601 y 1800 inclusive",
                            prop_matri_propuesta > 1800 & prop_matri_propuesta <= 2000 ~ "Entre 1801 y 2000 inclusive",
                            prop_matri_propuesta > 2000 & prop_matri_propuesta <= 2200 ~ "Entre 2001 y 2200 inclusive",
                            prop_matri_propuesta > 2200 & prop_matri_propuesta <= 2400 ~ "Entre 2201 y 2400 inclusive",
                            TRUE ~ "ERROR")) %>% 
  rename("Establecimiento" = `N - Establecimiento Facu`) %>% 
  rename("Matrícula" = matri_prof) %>% 
  rename("Cantidad de propuestas" = cant_propuesta) %>% 
  rename("Promedio de matrícula por propuesta" = prop_matri_propuesta) %>% 
  view()

ev_matricula_RA_total_grado_por_gestion_5.4 <- ev_matricula_RA_total_grado_por_gestion_5.4 %>% 
  mutate(rangos = fct_relevel(rangos,
                              "Hasta 200 inclusive",
                              "Entre 201 y 400 inclusive",
                              "Entre 401 y 600 inclusive",
                              "Entre 601 y 800 inclusive",
                              "Entre 801 y 1000 inclusive",
                              "Entre 1001 y 1200 inclusive",
                              "Entre 1201 y 1400 inclusive",
                              "Entre 1401 y 1600 inclusive",
                              "Entre 1601 y 1800 inclusive",
                              "Entre 1801 y 2000 inclusive",
                              "Entre 2001 y 2200 inclusive",
                              "Entre 2201 y 2400 inclusive"))

ev_matricula_RA_total_grado_por_gestion_5.5 <- ev_matricula_RA_total_grado_por_gestion_5.4 %>% 
  group_by(Gestión, rangos) %>% 
  summarise(valores_rangos = n()) %>%
  rename("Rangos de promedio de matrícula por carrera y establecimiento" = rangos) %>% 
  view()

TD_concentracion_2.1 <- ev_matricula_RA_total_grado_por_gestion_5.5 %>%
  pivot_wider(#id_cols = matri_prof,
    names_from="Gestión",
    values_from=c(valores_rangos)) %>%
  view() %>% 
  write.xlsx("Resultados/TD_concentracion_2.1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Evolución de la matrícula ####

## Gráfico Evolución de la Matrícula 1  ##

TD_evolucion_matricula_1 <- ev_matricula_RA_total_grado_por_gestion %>% 
  pivot_wider(Año,
              names_from="Gestión",
              values_from=c(matri_prof)) %>%
  rename("Matrícula Estatal por Año" = Estatal) %>% 
  rename("Matrícula Privada por Año" = Privada) %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_matricula_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_matricula_1 <-   
  ggplot(ev_matricula_RA_total_grado_por_gestion,
         aes(x=Año, y=matri_prof, color=Gestión))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=matri_prof, label = matri_prof))+
  scale_colour_manual(values = color_gestion)+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Evolución de la matrícula según gestión",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Matrícula Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_matricula_1.pdf", grafico_evolucion_matricula_1, width = 8, height = 6)

## Grafico Evolución de la matrícula 2  ##

ev_matricula_RA_total_grado_por_gestion_6 <- ingresantes_y_matricula_RA_total_grado %>% 
  filter(Gestión == "Estatal") %>% 
  group_by(Año, `N - Titulo Facu Nivel`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Año) %>%
  view()

TD_evolucion_matricula_2 <- ev_matricula_RA_total_grado_por_gestion_6 %>% 
  pivot_wider("Año",
              names_from=`N - Titulo Facu Nivel`,
              values_from=c(matri_prof)) %>%
  rename("Matrícula nivel inicial estatal" = "Nivel inicial") %>% 
  rename("Matrícula nivel primaria estatal" = "Nivel primaria") %>%
  rename("Matrícula nivel secundario y/o superior estatal" = "Nivel Secundario y o superior") %>% 
  rename("Matrícula multinivel estatal" = "Multinivel") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_matricula_2.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_matricula_2 <-   
  ggplot(ev_matricula_RA_total_grado_por_gestion_6,
         aes(x=Año, y=matri_prof, color=`N - Titulo Facu Nivel`))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=matri_prof, label = matri_prof))+
  scale_colour_manual(values = color_nivel)+
  guides(color = guide_legend(title = "Nivel"))+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position="bottom"
  )+
  labs(title = "Evolución de la matrícula estatal según nivel",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Matrícula Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_matricula_2.pdf", grafico_evolucion_matricula_2, width = 8, height = 6)

## Grafico Evolución de la matrícula 2.1  ##

for (nivel_loop in c("Multinivel","Nivel Secundario y o superior")) {
  
  ev_matricula_RA_total_grado_por_gestion_6.1 <- ingresantes_y_matricula_RA_total_grado %>% 
    filter(Gestión == "Estatal") %>% 
    filter(`N - Titulo Facu Nivel` == nivel_loop) %>% 
    group_by(Año, `N - Titulo Facu Agrupado`) %>%
    summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Año) 
  
  TD_evolucion_matricula_2.1 <- ev_matricula_RA_total_grado_por_gestion_6.1 %>% 
    pivot_wider("Año",
                names_from=`N - Titulo Facu Agrupado`,
                values_from=c(matri_prof)) %>%
    view() %>% 
    write.xlsx(paste("Resultados/TD_evolucion_matricula_2.1_",nivel_loop,".xlsx"),rowNames = FALSE, fileEncoding = "latin1")
  
  grafico_evolucion_matricula_2.1 <-   
    ggplot(ev_matricula_RA_total_grado_por_gestion_6.1,
           aes(x=Año, y=matri_prof, color=`N - Titulo Facu Agrupado`))+
    geom_line()+
    geom_point()+
    geom_text_repel(
      aes(x=Año, y=matri_prof, label = matri_prof))+
    #scale_colour_manual(values = color_nivel)+
    guides(color = guide_legend(title = "Carrera Agrupada"))+
    scale_x_discrete(limits = c(2016:2023)) +
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position="bottom"
    )+
    labs(title = paste("Evolución de la matrícula estatal para",tolower(nivel_loop)),
         subtitle = "Para años 2016-2023",
         x = "Año",
         y = "Matrícula Profesorados",
         caption = "Fuente: elaboración propia en base a Relevamiento Anual")
  
  ggsave(filename = paste("Resultados/grafico_evolucion_matricula_2.1_",nivel_loop,".pdf"), grafico_evolucion_matricula_2.1, width = 10, height = 6)
  
}          

## Grafico Evolución de la matrícula 3  ##

ev_matricula_RA_total_grado_por_gestion_7 <- ingresantes_y_matricula_RA_total_grado %>% 
  filter(Gestión == "Privada") %>% 
  group_by(Año, `N - Titulo Facu Nivel`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Año) %>%
  view()

TD_evolucion_matricula_3 <- ev_matricula_RA_total_grado_por_gestion_7 %>% 
  pivot_wider("Año",
              names_from=`N - Titulo Facu Nivel`,
              values_from=c(matri_prof)) %>%
  rename("Matrícula nivel inicial privada" = "Nivel inicial") %>% 
  rename("Matrícula nivel primaria privada" = "Nivel primaria") %>%
  rename("Matrícula nivel secundario y/o superior privada" = "Nivel Secundario y o superior") %>% 
  rename("Matrícula multinivel privada" = "Multinivel") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_matricula_3.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_matricula_3 <-   
  ggplot(ev_matricula_RA_total_grado_por_gestion_7,
         aes(x=Año, y=matri_prof, color=`N - Titulo Facu Nivel`))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=matri_prof, label = matri_prof))+
  scale_colour_manual(values = color_nivel)+
  guides(color = guide_legend(title = "Nivel"))+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position="bottom"
  )+
  labs(title = "Evolución de la matrícula privada según nivel",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Matrícula Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_matricula_3.pdf", grafico_evolucion_matricula_3, width = 8, height = 6)

## Grafico Evolución de la matrícula 3.1  ##

for (nivel_loop in c("Multinivel","Nivel Secundario y o superior")) {
  
  ev_matricula_RA_total_grado_por_gestion_7.1 <- ingresantes_y_matricula_RA_total_grado %>% 
    filter(Gestión == "Privada") %>% 
    filter(`N - Titulo Facu Nivel` == nivel_loop) %>% 
    group_by(Año, `N - Titulo Facu Agrupado`) %>%
    summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Año) 
  
  TD_evolucion_matricula_3.1 <- ev_matricula_RA_total_grado_por_gestion_7.1 %>% 
    pivot_wider("Año",
                names_from=`N - Titulo Facu Agrupado`,
                values_from=c(matri_prof)) %>%
    view() %>% 
    write.xlsx(paste("Resultados/TD_evolucion_matricula_3.1_",nivel_loop,".xlsx"),rowNames = FALSE, fileEncoding = "latin1")
  
  grafico_evolucion_matricula_3.1 <-   
    ggplot(ev_matricula_RA_total_grado_por_gestion_7.1,
           aes(x=Año, y=matri_prof, color=`N - Titulo Facu Agrupado`))+
    geom_line()+
    geom_point()+
    geom_text_repel(
      aes(x=Año, y=matri_prof, label = matri_prof))+
    #scale_colour_manual(values = color_nivel)+
    guides(color = guide_legend(title = "Carrera Agrupada"))+
    scale_x_discrete(limits = c(2016:2023)) +
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position="bottom"
    )+
    labs(title = paste("Evolución de la matrícula privada para",tolower(nivel_loop)),
         subtitle = "Para años 2016-2023",
         x = "Año",
         y = "Matrícula Profesorados",
         caption = "Fuente: elaboración propia en base a Relevamiento Anual")
  
  ggsave(filename = paste("Resultados/grafico_evolucion_matricula_3.1_",nivel_loop,".pdf"), grafico_evolucion_matricula_3.1, width = 8, height = 6)
  
}  


#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Trabajo con INGRESANTES estatal y privada en base a la unificación de RA ####

#### Evolución de los ingresantes ####

## Grafico Evolución de los ingresantes 1  ##

ev_ingresantes_RA_total_grado_por_gestion <- ingresantes_y_matricula_RA_total_grado %>%
  group_by(Gestión, Año) %>%
  summarise(ingr_prof = sum(Ingresantes, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>%
  view() 

TD_evolucion_ingresantes_1 <- ev_ingresantes_RA_total_grado_por_gestion %>% 
  pivot_wider(Año,
              names_from="Gestión",
              values_from=c(ingr_prof)) %>%
  rename("Ingresantes Estatal por Año" = Estatal) %>% 
  rename("Ingresantes Privada por Año" = Privada) %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_ingresantes_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_ingresantes_1 <-   
  ggplot(ev_ingresantes_RA_total_grado_por_gestion,
         aes(x=Año, y=ingr_prof, color=Gestión))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=ingr_prof, label = ingr_prof))+
  scale_colour_manual(values = color_gestion)+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Evolución de los ingresantes según gestión",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Ingresantes Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_ingresantes_1.pdf", grafico_evolucion_ingresantes_1, width = 8, height = 6)

## Grafico Evolución de los ingresantes 2  ##

ev_ingresantes_RA_total_grado_por_gestion_2 <- ingresantes_y_matricula_RA_total_grado %>% 
  filter(Gestión == "Estatal") %>% 
  group_by(Año, `N - Titulo Facu Nivel`) %>%
  summarise(ingr_prof = sum(Ingresantes , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Año) %>%
  view()

TD_evolucion_ingresantes_2 <- ev_ingresantes_RA_total_grado_por_gestion_2 %>% 
  pivot_wider("Año",
              names_from=`N - Titulo Facu Nivel`,
              values_from=c(ingr_prof)) %>%
  rename("Ingresantes nivel inicial estatal" = "Nivel inicial") %>% 
  rename("Ingresantes nivel primaria estatal" = "Nivel primaria") %>%
  rename("Ingresantes nivel secundario y/o superior estatal" = "Nivel Secundario y o superior") %>% 
  rename("Ingresantes multinivel estatal" = "Multinivel") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_ingresantes_2.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_ingresantes_2 <-   
  ggplot(ev_ingresantes_RA_total_grado_por_gestion_2,
         aes(x=Año, y=ingr_prof, color=`N - Titulo Facu Nivel`))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=ingr_prof, label = ingr_prof))+
  scale_colour_manual(values = color_nivel)+
  guides(color = guide_legend(title = "Nivel"))+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position="bottom"
  )+
  labs(title = "Evolución de los ingresantes estatal según nivel",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Ingresantes Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_ingresantes_2.pdf", grafico_evolucion_ingresantes_2, width = 8, height = 6)

## Grafico Evolución de los ingresantes 2.1  ##

for (nivel_loop in c("Multinivel","Nivel Secundario y o superior")) {
  
  ev_ingresantes_RA_total_grado_por_gestion_2.1 <- ingresantes_y_matricula_RA_total_grado %>% 
    filter(Gestión == "Estatal") %>% 
    filter(`N - Titulo Facu Nivel` == nivel_loop) %>% 
    group_by(Año, `N - Titulo Facu Agrupado`) %>%
    summarise(ingr_prof = sum(Ingresantes , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Año) 
  
  TD_evolucion_ingresantes_2.1 <- ev_ingresantes_RA_total_grado_por_gestion_2.1 %>% 
    pivot_wider("Año",
                names_from=`N - Titulo Facu Agrupado`,
                values_from=c(ingr_prof)) %>%
    view() %>% 
    write.xlsx(paste("Resultados/TD_evolucion_ingresantes_2.1_",nivel_loop,".xlsx"),rowNames = FALSE, fileEncoding = "latin1")
  
  grafico_evolucion_ingresantes_2.1 <-   
    ggplot(ev_ingresantes_RA_total_grado_por_gestion_2.1,
           aes(x=Año, y=ingr_prof, color=`N - Titulo Facu Agrupado`))+
    geom_line()+
    geom_point()+
    geom_text_repel(
      aes(x=Año, y=ingr_prof, label = ingr_prof))+
    #scale_colour_manual(values = color_nivel)+
    guides(color = guide_legend(title = "Carrera Agrupada"))+
    scale_x_discrete(limits = c(2016:2023)) +
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position="bottom"
    )+
    labs(title = paste("Evolución de ingresantes estatal para",tolower(nivel_loop)),
         subtitle = "Para años 2016-2023",
         x = "Año",
         y = "Ingresantes Profesorados",
         caption = "Fuente: elaboración propia en base a Relevamiento Anual")
  
  ggsave(filename = paste("Resultados/grafico_evolucion_ingresantes_2.1_",nivel_loop,".pdf"), grafico_evolucion_ingresantes_2.1, width = 10, height = 6)
  
}          

## Grafico Evolución de los ingresantes 3  ##

ev_ingresantes_RA_total_grado_por_gestion_3 <- ingresantes_y_matricula_RA_total_grado %>% 
  filter(Gestión == "Privada") %>% 
  group_by(Año, `N - Titulo Facu Nivel`) %>%
  summarise(ingr_prof = sum(Ingresantes , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Año) %>%
  view()

TD_evolucion_ingresantes_3 <- ev_ingresantes_RA_total_grado_por_gestion_3 %>% 
  pivot_wider("Año",
              names_from=`N - Titulo Facu Nivel`,
              values_from=c(ingr_prof)) %>%
  rename("Ingresantes nivel inicial privada" = "Nivel inicial") %>% 
  rename("Ingresantes nivel primaria privada" = "Nivel primaria") %>%
  rename("Ingresantes nivel secundario y/o superior privada" = "Nivel Secundario y o superior") %>% 
  rename("Ingresantes multinivel privada" = "Multinivel") %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_ingresantes_3.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_ingresantes_3 <-   
  ggplot(ev_ingresantes_RA_total_grado_por_gestion_3,
         aes(x=Año, y=ingr_prof, color=`N - Titulo Facu Nivel`))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=ingr_prof, label = ingr_prof))+
  scale_colour_manual(values = color_nivel)+
  guides(color = guide_legend(title = "Nivel"))+
  scale_x_discrete(limits = c(2016:2023)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
    legend.position="bottom"
  )+
  labs(title = "Evolución de los ingresantes privada según nivel",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "Ingresantes Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_ingresantes_3.pdf", grafico_evolucion_ingresantes_3, width = 8, height = 6)

## Grafico Evolución de los ingresantes 3.1  ##

for (nivel_loop in c("Multinivel","Nivel Secundario y o superior")) {
  
  ev_ingresantes_RA_total_grado_por_gestion_3.1 <- ingresantes_y_matricula_RA_total_grado %>% 
    filter(Gestión == "Privada") %>% 
    filter(`N - Titulo Facu Nivel` == nivel_loop) %>% 
    group_by(Año, `N - Titulo Facu Agrupado`) %>%
    summarise(ingr_prof = sum(Ingresantes , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Año) 
  
  TD_evolucion_ingresantes_3.1 <- ev_ingresantes_RA_total_grado_por_gestion_3.1 %>% 
    pivot_wider("Año",
                names_from=`N - Titulo Facu Agrupado`,
                values_from=c(ingr_prof)) %>%
    view() %>% 
    write.xlsx(paste("Resultados/TD_evolucion_ingresantes_3.1_",nivel_loop,".xlsx"),rowNames = FALSE, fileEncoding = "latin1")
  
  grafico_evolucion_ingresantes_3.1 <-   
    ggplot(ev_ingresantes_RA_total_grado_por_gestion_3.1,
           aes(x=Año, y=ingr_prof, color=`N - Titulo Facu Agrupado`))+
    geom_line()+
    geom_point()+
    geom_text_repel(
      aes(x=Año, y=ingr_prof, label =ingr_prof))+
    #scale_colour_manual(values = color_nivel)+
    guides(color = guide_legend(title = "Carrera Agrupada"))+
    scale_x_discrete(limits = c(2016:2023)) +
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0),
      legend.position="bottom"
    )+
    labs(title = paste("Evolución de ingresantes privada para",tolower(nivel_loop)),
         subtitle = "Para años 2016-2023",
         x = "Año",
         y = "Ingresantes Profesorados",
         caption = "Fuente: elaboración propia en base a Relevamiento Anual")
  
  ggsave(filename = paste("Resultados/grafico_evolucion_ingresantes_3.1_",nivel_loop,".pdf"), grafico_evolucion_ingresantes_3.1, width = 8, height = 6)
  
}          


#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Trabajo con aspirantes e ingresantes SIU ####

#### Aspirantes contra ingresantes ####

## Grafico aspirantes contra ingresantes ingresantes 1  ##

aspir_ingr_siu <- read_excel("Recorte base 2016-2023 - Sólo 1° 2023.xlsx")

aspir_ingr_siu <- aspir_ingr_siu %>% 
  filter(`N - Titulo Martu Agrupado` != "Otros") %>% 
  mutate(`N - Titulo Martu Agrupado` = case_when(`N - Titulo Martu Agrupado` == "Nivel Secundario y/o superior" ~ "Nivel Secundario y o superior",
                                                 TRUE  ~ `N - Titulo Martu Agrupado`)) %>% 
  mutate(`N - Titulo Martu Agrupado`= fct_relevel(`N - Titulo Martu Agrupado`,
                                                  "Nivel inicial", "Nivel primaria","Nivel Secundario y o superior",
                                                  "Multinivel")) %>% 
  mutate(`N - Titulo Martu.0` = fct_relevel(`N - Titulo Martu`,
                                          "Educación Inicial", "Educación Primaria", "Lengua y Literatura",	"Matemática",	
                                          "Química",	"Física",	"Biología",	"Ciencia Política",	"Ciencias de la Administración",	
                                          "Ciencias Jurídicas",	"Ciencias de la Educación",	"Economía",	"Filosofía",	"Geografía",	
                                          "Historia",	"Psicología",	"Informática",	"Educación Física",	"Música",	"Danza",	
                                          "Artes visuales",		"Educación Especial",	
                                          "Educación tecnológica",	"Inglés",	"Italiano",	"Portugues",	"Alemán",	"Francés"))


ev_aspir_ingr_siu_1 <- aspir_ingr_siu %>% 
  group_by(`N - Titulo Martu Agrupado`, `N - Titulo Martu`) %>% 
  summarise(Ingresantes = sum(`Cuenta ingresantes`)) %>% 
  ungroup()

ev_aspir_ingr_siu_2 <- aspir_ingr_siu %>% 
  group_by(`N - Titulo Martu Agrupado`, `N - Titulo Martu`) %>% 
  summarise(Aspirantes = n()) %>% 
  ungroup()

ev_aspir_ingr_siu_3 <- ev_aspir_ingr_siu_1 %>% 
  left_join(ev_aspir_ingr_siu_2, by = c("N - Titulo Martu Agrupado" = "N - Titulo Martu Agrupado",
                                        "N - Titulo Martu" = "N - Titulo Martu")) %>% 
  mutate(prop_ingre_aspir = percent(Ingresantes/Aspirantes, accuracy = 0.1)) 

ev_aspir_ingr_siu_4 <- ev_aspir_ingr_siu_3 %>% 
  group_by(`N - Titulo Martu Agrupado`) %>% 
  summarise(Ingresantes = sum(Ingresantes),
            Aspirantes = sum(Aspirantes)) %>% 
  mutate(prop_ingre_aspir = percent(Ingresantes/Aspirantes, accuracy = 0.1)) 

TD_ingresantes_sobre_aspirantes_1 <- ev_aspir_ingr_siu_4 %>% 
  rename("Nivel Estatal" = `N - Titulo Martu Agrupado`) %>% 
  rename("Proporción de ingresantes sobre aspirantes" = prop_ingre_aspir) %>% 
  write.xlsx("Resultados/TD_ingresantes_sobre_aspirantes_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_ingresantes_sobre_aspirantes_1 <- 
  ggplot(ev_aspir_ingr_siu_4,
         aes(x=`N - Titulo Martu Agrupado`, y=prop_ingre_aspir, fill=`N - Titulo Martu Agrupado`))+
  geom_col()+
  scale_fill_manual(values = color_nivel)+
  guides(fill="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Proporción de ingresantes sobre aspirantes según nivel",
       subtitle = "Para primer cuatrimestre 2023 - Sólo estatal",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Siu Guarani")

ggsave(filename = "Resultados/grafico_ingresantes_sobre_aspirantes_1.pdf", grafico_ingresantes_sobre_aspirantes_1, width = 8, height = 6)

## Grafico aspirantes contra ingresantes ingresantes 2  ##

for (nivel_loop in c("Multinivel","Nivel Secundario y o superior")) {
  
  ev_aspir_ingr_siu_3.1 <- ev_aspir_ingr_siu_3 %>% 
    filter(`N - Titulo Martu Agrupado` == nivel_loop)
  
  TD_ingresantes_sobre_aspirantes_2 <- ev_aspir_ingr_siu_3.1 %>% 
    rename("Carrera Agrupada" = `N - Titulo Martu`) %>% 
    rename("Proporción de ingresantes sobre aspirantes" = prop_ingre_aspir) %>% 
    select("Carrera Agrupada", Ingresantes, Aspirantes, "Proporción de ingresantes sobre aspirantes") %>% 
    write.xlsx(paste("Resultados/TD_ingresantes_sobre_aspirantes_2",nivel_loop,".xlsx"),rowNames = FALSE, fileEncoding = "latin1")
  
  grafico_ingresantes_sobre_aspirantes_2 <- 
  ggplot(ev_aspir_ingr_siu_3.1,
         aes(x = Ingresantes/Aspirantes, 
             y = reorder(`N - Titulo Martu`, desc(`N - Titulo Martu`)), 
             fill = as.numeric(`N - Titulo Martu`))) +
    geom_col() +
    scale_fill_gradient(low = "steelblue", high = "lightblue")+
    #scale_fill_viridis()+
    guides(fill="none")+
    guides(x="none")+
    geom_text(aes(x=Ingresantes/Aspirantes, 
                  y=reorder(`N - Titulo Martu`,desc(`N - Titulo Martu`)),
                  label=percent(Ingresantes/Aspirantes, accuracy = .1)),
              hjust=1,vjust=0.5,color="black",size=3)+
    theme_minimal()+
    theme(
      plot.caption = element_text(hjust = 0),
    )+
    labs(title = paste("Proporción de ingresantes sobre aspirantes",tolower(nivel_loop)),
         subtitle = "Para primer cuatrimestre 2023 - Sólo estatal",
         y = "",
         x = "",
         caption = "Fuente: elaboración propia en base a Siu Guarani")
  
  ggsave(filename = paste("Resultados/grafico_ingresantes_sobre_aspirantes_2",nivel_loop,".pdf"), grafico_ingresantes_sobre_aspirantes_2, width = 8, height = 6)
  
}

#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Importo la base en que unifiqué RA para EGRESADOS ####

egresados_RA_total_grado <- read_excel("99 - Ingresantes y Matricula RA 2015-2023 - Egresados RA 2015-2022.xlsx", sheet = "Egresados RA 2015-2022")

## Le corro los procesamientos necesarios para poder trabajarla ##

# Saco TEOLOGÍA y todos los que designé como xxx que no nos van a interesar para el informe
# Elimino el año 2015 porque no traía datos de artística

egresados_RA_total_grado <- egresados_RA_total_grado %>%
  filter(`N - Titulo Facu Agrupado` != "xxx")  %>% 
  filter(Año > 2015) %>% 
  mutate(`N - Titulo Facu Nivel` = case_when( `N - Titulo Facu Nivel` == "Nivel Secundario y/o superior" ~ "Nivel Secundario y o superior",
                                              TRUE  ~ `N - Titulo Facu Nivel`)) %>% 
  mutate(`N - Titulo Facu Agrupado` = fct_relevel(`N - Titulo Facu Agrupado`,
                                                  "Educación Inicial", "Educación Primaria", "Lengua y Literatura",	"Matemática",	
                                                  "Química",	"Física",	"Biología",	"Ciencia Política",	"Ciencias de la Administración",	
                                                  "Ciencias Jurídicas",	"Ciencias de la Educación",	"Economía",	"Filosofía",	"Geografía",	
                                                  "Historia",	"Psicología",	"Informática",	"TIC", "Educación Física",	"Música",	"Danza",	
                                                  "Artes visuales",	"Teatro",	"Expresión corporal",	"Educación Especial",	
                                                  "Educación tecnológica",	"Inglés",	"Italiano",	"Portugues",	"Alemán",	"Francés")) %>% 
  mutate(`N - Titulo Facu Nivel` = fct_relevel(`N - Titulo Facu Nivel`,
                                               "Nivel inicial", "Nivel primaria","Nivel Secundario y o superior",
                                               "Multinivel")) %>% 
  mutate(`Gestión` = fct_relevel(`Gestión`,"Estatal",
                                 "Privada"))


#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Trabajo con EGRESADOS estatal y privada en base a la unificación de RA ####

#### Evolución de los egresados ####

## Grafico Evolución de los egresados 1  ##

ev_egresados_RA_total_grado_por_gestion <- egresados_RA_total_grado %>%
  group_by(Gestión, Año) %>%
  summarise(egre_prof = sum(`Total Egresados` , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

TD_evolucion_egresados_1 <- ev_egresados_RA_total_grado_por_gestion %>% 
  pivot_wider(Año,
              names_from="Gestión",
              values_from=c(egre_prof)) %>%
  rename("Egresados Estatal por Año" = Estatal) %>% 
  rename("Egresados Privada por Año" = Privada) %>% 
  view() %>% 
  write.xlsx("Resultados/TD_evolucion_egresados_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_evolucion_egresados_1 <-   
  ggplot(ev_egresados_RA_total_grado_por_gestion,
         aes(x=Año, y=egre_prof, color=Gestión))+
  geom_line()+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=egre_prof, label = egre_prof))+
  scale_colour_manual(values = color_gestion)+
  scale_x_discrete(limits = c(2016:2022)) +
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Evolución de los egresados según gestión",
       subtitle = "Para años 2016-2022",
       x = "Año",
       y = "Egresados Profesorados",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_evolucion_egresados_1.pdf", grafico_evolucion_egresados_1, width = 8, height = 6)


#----------------------------------------------------------------------------------------------------------.
#----------------------------------------------------------------------------------------------------------.

#### Egresados, matrícula e ingresantes ####

## Grafico matrícula cotra ingresantes 1  ##

ev_egre_ingr_matri_1 <- ingresantes_y_matricula_RA_total_grado %>%
  group_by(Gestión, Año, `N - Titulo Facu Agrupado`, `N - Titulo Facu Nivel`) %>%
  summarise(ingr_prof = sum(Ingresantes , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

ev_egre_ingr_matri_2 <- ingresantes_y_matricula_RA_total_grado %>%
  group_by(Gestión, Año, `N - Titulo Facu Agrupado`, `N - Titulo Facu Nivel`) %>%
  summarise(matri_prof = sum(Matricula , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

ev_egre_ingr_matri_3 <- egresados_RA_total_grado %>%
  group_by(Gestión, Año, `N - Titulo Facu Agrupado`, `N - Titulo Facu Nivel`) %>%
  summarise(egre_prof = sum(`Total Egresados` , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

ev_egre_ingr_matri_4 <- ev_egre_ingr_matri_1 %>% 
  left_join(ev_egre_ingr_matri_2, by = c("Gestión" = "Gestión", 
                                         "Año" = "Año",
                                         "N - Titulo Facu Agrupado" = "N - Titulo Facu Agrupado",
                                         "N - Titulo Facu Nivel" = "N - Titulo Facu Nivel")) %>%  
  left_join(ev_egre_ingr_matri_3, by = c("Gestión" = "Gestión", 
                                         "Año" = "Año",
                                         "N - Titulo Facu Agrupado" = "N - Titulo Facu Agrupado",
                                         "N - Titulo Facu Nivel" = "N - Titulo Facu Nivel")) #%>% 
#filter(Año < 2023) 

ev_egre_ingr_matri_5 <- ev_egre_ingr_matri_4 %>% 
  group_by(Gestión, Año) %>%
  summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
            matri_prof = sum(matri_prof , na.rm = TRUE),
            egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) 

ev_egre_ingr_matri_6 <- ev_egre_ingr_matri_5 %>% 
  select(-egre_prof) %>% 
  pivot_longer(cols = c("ingr_prof", "matri_prof"),
               names_to= "condicion",
               values_to= "cantidad") 

TD_matri_ingr_1 <- ev_egre_ingr_matri_5 %>% 
  select(-egre_prof) %>% 
  mutate(ingr_div_matri = percent(ingr_prof/matri_prof, accuracy = 0.1)) %>% 
  pivot_wider(Año,
              names_from="Gestión",
              values_from=c(ingr_prof, matri_prof, ingr_div_matri)) %>%
  select(Año, ingr_prof_Estatal, matri_prof_Estatal,  ingr_div_matri_Estatal,
         ingr_prof_Privada, matri_prof_Privada,  ingr_div_matri_Privada) %>% 
  rename("Ingresantes Estatal" = ingr_prof_Estatal) %>% 
  rename("Matrícula Estatal" = matri_prof_Estatal) %>%
  rename("Proporción de ingresantes sobre matrícula Estatal" = ingr_div_matri_Estatal) %>%
  rename("Ingresantes Privada" = ingr_prof_Privada) %>% 
  rename("Matrícula Privada" = matri_prof_Privada) %>%
  rename("Proporción de ingresantes sobre matrícula Privada" = ingr_div_matri_Privada)

write.xlsx("Resultados/TD_matri_ingr_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

grafico_matri_ingr_1 <- 
  ggplot(ev_egre_ingr_matri_6,
         aes(x=Año, y=cantidad, color= Gestión, linetype=condicion))+
  geom_line(size=1)+
  geom_point()+
  geom_text_repel(
    aes(x=Año, y=cantidad, label = cantidad))+
  geom_text_repel(data=ev_egre_ingr_matri_6 %>% filter(condicion == "matri_prof" & Gestión == "Estatal" & Año == 2016),
                  aes(y=cantidad,
                      label = "Matrícula Estatal"),
                  hjust=1,vjust=-3,color="darkgrey",size=3)+
  geom_text_repel(data=ev_egre_ingr_matri_6 %>% filter(condicion == "matri_prof" & Gestión == "Privada" & Año == 2016),
                  aes(y=cantidad,
                      label = "Matrícula Privada"),
                  hjust=1,vjust=-3,color="darkgrey",size=3)+
  geom_text_repel(data=ev_egre_ingr_matri_6 %>% filter(condicion == "ingr_prof" & Gestión == "Estatal" & Año == 2016),
                  aes(y=cantidad,
                      label = "Ingresante Estatal"),
                  hjust=0,vjust=4,color="darkgrey",size=3)+
  geom_text_repel(data=ev_egre_ingr_matri_6 %>% filter(condicion == "ingr_prof" & Gestión == "Privada" & Año == 2016),
                  aes(y=cantidad,
                      label = "Ingresante Privada"),
                  hjust=0,vjust=4,color="darkgrey",size=3)+ 
  scale_x_discrete(limits = c(2016:2023))+
  scale_color_manual(values = color_gestion)+
  guides(color="none")+
  guides(linetype="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Comparación de evolución de matrícula e ingresantes según gestión",
       subtitle = "Para años 2016-2023",
       x = "Año",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_matri_ingr_1.pdf", grafico_matri_ingr_1, width = 8, height = 6)

#----------------------------------------------------------------------------------------------------------.

## Grafico matrícula cotra ingresantes 2  ##

ev_egre_ingr_matri_7 <- ev_egre_ingr_matri_5 %>% 
  select(-egre_prof) %>% 
  mutate(ingr_div_matri = percent(ingr_prof/matri_prof,accuracy = 0.1)) %>% 
  pivot_longer(cols = c("ingr_prof", "matri_prof"),
               names_to= "condicion",
               values_to= "cantidad")

grafico_matri_ingr_2 <- 
  ggplot(ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal"),
         aes(x=Año, y=cantidad, color= Gestión, linetype=condicion))+
  geom_line(size=1)+
  geom_point()+
  geom_text_repel(data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal" & (Año == 2016 | Año == 2019 | Año == 2022 | Año == 2023)),
                  aes(x=Año, y=cantidad, label = cantidad))+
  geom_segment(aes(x=2016, y=6672, xend=2016, yend=24917), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal" & Año == 2016 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2019, y=6839, xend=2019, yend=25856), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal" & Año == 2019 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2022, y=6626, xend=2022, yend=25691), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal" & Año == 2022 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2023, y=5101, xend=2023, yend=23408), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Estatal" & Año == 2023 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  scale_x_discrete(limits = c(2016:2023))+
  scale_color_manual(values = color_gestion)+
  guides(color="none")+
  guides(linetype="none")+  
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Variación de la proporción entre matrícula e ingresantes estatales a lo largo de los años",
       subtitle = "Con la mirada puesta en 2016-2019-2022-2023",
       x = "Año",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_matri_ingr_2.pdf", grafico_matri_ingr_2, width = 8, height = 6)

grafico_matri_ingr_2.1 <- 
  ggplot(ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada"),
         aes(x=Año, y=cantidad, color= Gestión, linetype=condicion))+
  geom_line(size=1)+
  geom_point()+
  geom_text_repel(data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada" & (Año == 2016 | Año == 2019 | Año == 2022 | Año == 2023)),
                  aes(x=Año, y=cantidad, label = cantidad))+
  geom_segment(aes(x=2016, y=2521, xend=2016, yend=7029), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada" & Año == 2016 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2019, y=2598, xend=2019, yend=8253), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada" & Año == 2019 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2022, y=1774, xend=2022, yend=6719), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada" & Año == 2022 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  geom_segment(aes(x=2023, y=1883, xend=2023, yend=6070), colour = "darkgrey", size = 0.1, arrow = arrow(length = unit(0.3,"cm"),ends = "both"))+
  geom_text(
    data=ev_egre_ingr_matri_7 %>% filter(Gestión == "Privada" & Año == 2023 & condicion == "ingr_prof"),
    aes(x=Año, y =cantidad ,label=ingr_div_matri),
    hjust=-0.2,vjust=-16,color="darkgrey",size=4)+
  scale_x_discrete(limits = c(2016:2023))+
  scale_color_manual(values = color_gestion)+
  guides(color="none")+
  guides(linetype="none")+  
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0)
  )+
  labs(title = "Variación de la proporción entre matrícula e ingresantes privada a lo largo de los años",
       subtitle = "Con la mirada puesta en 2016-2019-2022-2023",
       x = "Año",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_matri_ingr_2.1.pdf", grafico_matri_ingr_2.1, width = 8, height = 6)

#----------------------------------------------------------------------------------------------------------.

## Indicador promedio de egresos sobre promedio de ingresos  ##

TD_egre_sobre_ingr_1 <- ev_egre_ingr_matri_4 %>% 
  filter(Año < 2023) %>% 
  group_by(Gestión) %>%
  summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
            egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>% 
  mutate(egre_div_ingr = percent(egre_prof/ingr_prof, accuracy = 0.1)) %>% 
  select(-egre_prof, -ingr_prof) %>% 
  pivot_wider(#id_cols = matri_prof,
    names_from="Gestión",
    values_from=c(egre_div_ingr)) %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Estatal" = "Estatal") %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Privada" = "Privada") %>%  
  view() %>% 
  write.xlsx("Resultados/TD_egre_sobre_ingr_1.xlsx",rowNames = FALSE, fileEncoding = "latin1")

TD_egre_sobre_ingr_1.A <- ev_egre_ingr_matri_4 %>% 
  filter(Año < 2023) %>% 
  group_by(Gestión, `N - Titulo Facu Nivel`) %>%
  summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
            egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>% 
  mutate(egre_div_ingr = percent(egre_prof/ingr_prof, accuracy = 0.1)) %>% 
  select(-egre_prof, -ingr_prof) %>% 
  pivot_wider(id_cols = `N - Titulo Facu Nivel`,
    names_from="Gestión",
    values_from=c(egre_div_ingr)) %>% 
  rename("Nivel de Carrera" = "N - Titulo Facu Nivel") %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Estatal" = "Estatal") %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Privada" = "Privada") %>%  
  view() %>% 
  write.xlsx("Resultados/TD_egre_sobre_ingr_1.A.xlsx",rowNames = FALSE, fileEncoding = "latin1")
  
  TD_egre_sobre_ingr_1.B <- ev_egre_ingr_matri_4 %>% 
    filter(Año < 2023) %>% 
    group_by(Gestión, `N - Titulo Facu Nivel`, `N - Titulo Facu Agrupado`) %>%
    summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
              egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Gestión) %>% 
    mutate(egre_div_ingr = percent(egre_prof/ingr_prof, accuracy = 0.1)) %>% 
    select(-egre_prof, -ingr_prof) %>% 
    pivot_wider(id_cols = c(`N - Titulo Facu Nivel`, `N - Titulo Facu Agrupado`),
                names_from="Gestión",
                values_from=c(egre_div_ingr)) %>% 
    rename("Nivel de Carrera" = "N - Titulo Facu Nivel") %>% 
    rename("Carrera Agrupada" = "N - Titulo Facu Agrupado") %>% 
    rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Estatal" = "Estatal") %>% 
    rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Privada" = "Privada") %>%  
    view() %>% 
    write.xlsx("Resultados/TD_egre_sobre_ingr_1.B.xlsx",rowNames = FALSE, fileEncoding = "latin1")
  
  TD_egre_sobre_ingr_1.B.1 <- ev_egre_ingr_matri_4 %>% 
    filter(Año < 2023) %>% 
    group_by(Año, Gestión, `N - Titulo Facu Nivel`, `N - Titulo Facu Agrupado`) %>%
    summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
              egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Gestión) %>% 
    pivot_wider(id_cols = c(Año, `N - Titulo Facu Nivel`, `N - Titulo Facu Agrupado`),
                names_from="Gestión",
                values_from=c(ingr_prof, egre_prof)) %>% 
    view() %>% 
    write.xlsx("Resultados/TD_egre_sobre_ingr_1.B.1.xlsx",rowNames = FALSE, fileEncoding = "latin1")
  

## Gráfico de egresos sobre promedio de ingresos  ##

ev_egre_ingr_matri_9 <- ev_egre_ingr_matri_4 %>% 
  filter(Año < 2023) %>% 
  group_by(Gestión, `N - Titulo Facu Nivel`) %>%
  summarise(ingr_prof = sum(ingr_prof , na.rm = TRUE), 
            egre_prof = sum(egre_prof , na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Gestión) %>% 
  mutate(egre_div_ingr = egre_prof/ingr_prof) 

TD_egre_sobre_ingr_2 <- ev_egre_ingr_matri_9 %>% 
  select(-ingr_prof, -egre_prof) %>% 
  mutate(egre_div_ingr = percent(egre_div_ingr, accuracy = 0.1)) %>% 
  pivot_wider(`N - Titulo Facu Nivel`,
              names_from="Gestión",
              values_from=c(egre_div_ingr)) %>%
  rename("Nivel de carrera" = `N - Titulo Facu Nivel`) %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Estatal" = "Estatal") %>% 
  rename("Proporción de Egresados sobre Ingresantes 2016 - 2022 Privada" = "Privada") %>%  
  view() %>% 
  write.xlsx("Resultados/TD_egre_sobre_ingr_2.xlsx",rowNames = FALSE, fileEncoding = "latin1") 


grafico_egre_sobre_ingr_2 <- 
  ggplot(ev_egre_ingr_matri_9,
         aes(x=`N - Titulo Facu Nivel`,
             y=egre_div_ingr, 
             fill=`Gestión`))+
  geom_col(position = "dodge")+
  geom_text(data=ev_egre_ingr_matri_9 %>% filter(Gestión == "Estatal"),
            aes(x=`N - Titulo Facu Nivel`,
                y=egre_div_ingr, 
                label=percent(egre_div_ingr, accuracy = 0.1)),
            hjust=1.5,vjust=-0.5,color="black",size=3)+
  geom_text(data=ev_egre_ingr_matri_9 %>% filter(Gestión == "Privada"),
            aes(x=`N - Titulo Facu Nivel`,
                y=egre_div_ingr, 
                label=percent(egre_div_ingr, accuracy = 0.1)),
            hjust=-0.5,vjust=-0.5,color="black",size=3)+
  scale_fill_manual(values = color_gestion)+
  guides (y="none")+
  theme_minimal()+
  theme(
    plot.caption = element_text(hjust = 0),
  )+
  labs(title = "Proporción de egresados sobre ingresantes 2016 - 2022",
       subtitle = "Según nivel y gestión",
       x = "",
       y = "",
       caption = "Fuente: elaboración propia en base a Relevamiento Anual")

ggsave(filename = "Resultados/grafico_egre_sobre_ingr_2.pdf", grafico_egre_sobre_ingr_2, width = 8, height = 6)



# hola


