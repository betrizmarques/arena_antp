# Este script foi criado para armaenar os cálculos e criação de gráficos para o
# artigo da Arena ANTP - 2025
library(tidyverse)
library(waffle)
library(roadtrafficdeaths)
# Diagnóstico-------------------------------------------------------------------

mortes <- rtdeaths 

mortes_media_anos <- mortes %>% 
  filter(ano_ocorrencia %in% c(2018, 2019, 2020))

count(mortes_media_anos)/3


# Proposições e Resultados------------------------------------------------------

base_principal <- read.csv('dados/base_principal.csv') %>% 
  mutate(meta_atingida_categorica = case_when(
    meta_atingida < 0 ~ "Aumentou",
    meta_atingida < 100 ~ "Reduziu",
    meta_atingida > 100 ~ "Atingiu a meta" 
  ))

agrupados <- base_principal %>% 
  group_by(meta_atingida_categorica) %>% 
  summarise(qtde = n())

ggplot(agrupados, aes(x = reorder(meta_atingida_categorica, -qtde), y = qtde))+
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(qtde/5044*100, 2), '%')),
    vjust = -0.5, 
    size = 3
  ) +
  labs(
    title = "Proporção dos municípios conforme desempenho",
    x = NULL,
    y = "Quantidade de municípios"
  ) +
  theme_minimal()
