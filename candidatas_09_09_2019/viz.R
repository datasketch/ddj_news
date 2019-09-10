library(tidyverse)
library(hgchmagic)

data <- read_csv('data/candidatas.csv')
data$GENERO <- plyr::revalue(data$GENERO, c('F' = 'Femenino', 'M' = 'Masculino'))

territorial <- data %>% 
                filter(CIRCUNSCRIPCION == 'TERRITORIAL') 

# procentaje de participación por género
genero <- territorial %>% 
           group_by(GENERO) %>% 
            summarise(total = n()) %>% 
             mutate(porcentaje = (total/sum(total))*100) %>% 
               select(-total)


opts <- list(
             nDigits = 2,
             suffix = '%',
             agg_text = ' ',
             colors = c( "#BDCAD1", "#53255E")
)
v1 <- hgch_pie_CatNum(genero, opts = opts)
htmlwidgets::saveWidget(v1, 'genero_territorial.html')

# municipios con más participación femenina

alcaldia <-  data %>% 
                filter(CORPORACION_CARGO == 'ALCALDIA') 

muncipios <- alcaldia %>% 
               group_by(MUNICIPIO, GENERO) %>% 
                summarise(total = n()) %>% 
                 mutate(porcentaje = (total/sum(total))*100)


par_fem <- muncipios %>%
            filter(GENERO == 'Femenino') %>% 
              arrange(-porcentaje) 
par_fem <- par_fem[1:10,]
par_fem <- par_fem %>% 
            select(MUNICIPIO)

top_fem <- par_fem %>% 
             left_join(muncipios) %>% 
              select(GENERO, MUNICIPIO, porcentaje)
            
opts <- list(
  agg_text = ' ',
  graph_type = "stacked",
  suffix = '%',
  colors = c( "#BDCAD1", "#53255E"),
  verLabel = ' ',
  horLabel = ' ',
  order2 = c('AGUAZUL', 'CASABIANCA', 'SOCORRO')
)
v2 <- hgch_bar_CatCatNum(top_fem, opts = opts) %>% 
       hc_yAxis(max = 100)
htmlwidgets::saveWidget(v2, 'genero_alcaldia.html')


# Género por departamento

gobernacion <- data %>% 
                filter(CORPORACION_CARGO == 'GOBERNADOR')


deptos <- gobernacion %>% 
           group_by(GENERO, DEPARTAMENTO) %>% 
            summarise(Total = n()) %>% 
             arrange(-Total)

opts <- list(
  agg_text = ' ',
  graph_type = "stacked",
  colors = c( "#BDCAD1", "#53255E"),
  verLabel = ' ',
  horLabel = ' ',
  orientation = 'hor'
)
v3 <- hgch_bar_CatCatNum(deptos, opts = opts)
htmlwidgets::saveWidget(v3, 'genero_gobernador.html')


# genero por partido en territoriales

opts <- list(
  agg_text = ' ',
  graph_type = "stacked",
  suffix = '%',
  colors = c( "#BDCAD1", "#53255E"),
  verLabel = ' ',
  horLabel = ' '
)

asamblea <- data %>% 
             filter(CORPORACION_CARGO == 'ASAMBLEA',
                    TIPO_PARTIDO == 'PARTIDO O MOVIMIENTO POLITICO CON PERSONERIA JURIDICA')

partido <- asamblea %>% 
             group_by(PARTIDO, GENERO) %>% 
              summarise(Total = n()) %>% 
                mutate(porcentaje = (Total/sum(Total))*100)

par_fem <- partido %>%
            filter(GENERO == 'Femenino') %>% 
              arrange(-porcentaje) 
par_fem <- par_fem[1:10,]
par_fem <- par_fem %>% 
            select(PARTIDO)

top_fem <- par_fem %>%
             left_join(partido) %>% 
               select(GENERO, PARTIDO, porcentaje)

opts <- list(
  agg_text = ' ',
  graph_type = "stacked",
  suffix = '%',
  colors = c( "#53255E", "#BDCAD1"),
  verLabel = ' ',
  horLabel = ' ',
  order1 = c('FEMENINO', 'MASCULINO'),
  order2 = unique(top_fem$PARTIDO),
  orientation = 'hor',
  labelWrapV = c(100, 100)
)
v4 <- hgch_bar_CatCatNum(top_fem, opts = opts) %>% 
       hc_yAxis(max = 100)
htmlwidgets::saveWidget(v4, 'genero_asamblea_juridica.html')
