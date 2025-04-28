# dissertacao mestrado ppgcp/ufpe 2021-2025 :)
# autora: Georgia Ribeiro
# codigo 3. mais graficos e descritivos após classificação temática

# ----- notas ----- #
# paleta de cores: ["0b3954","008b00","824c71","1b9aaa","f19953"]
Set3_MoreYellow <- c("#8DD3C7", "#DCB665", "#BEBADA", "#FB8082", "#80B1D3", "#F19501", "#CBE3A3", "#FCCDE5", "#C999BA")
# - fim das notas - #

# carregar pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(ggplot2)

# importar bases
df_pls3 = read.csv("data/treated_data/df_pls3.csv")
df_classtem = read.csv("data/treated_data/df_class_tematica_final.csv")
df_classtem = select(df_classtem, !c("X")) # remover index

# unir bases
df = left_join(df_pls3, df_classtem, by = 'CodigoMateria')


## GRAFICO distribuição de temas por ano -- visualizacao poluida
df %>% filter(amostra_meioamb == 1) %>%
  group_by(AnoMateria, class_tematica_sug) %>%  summarize(count = n()) %>%
  ggplot(aes(x = AnoMateria, y = count, group = class_tematica_sug, color = class_tematica_sug)) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_colour_manual(values = Set3_MoreYellow) +
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  ylab("Quantidade de PLs") +
  ggtitle("Projetos de Lei por ano e temáticas ambientais") +
  theme_bw()
#ggsave("images/PLs_subtemas_by_year.png", width = 6, height = 3, dpi = 300)


# grafico opcao 2 - ruim igual
df %>% 
  filter(amostra_meioamb == 1) %>%
  group_by(AnoMateria, class_tematica_sug) %>%
  summarize(count = n()) %>%
  ggplot(aes(AnoMateria, count)) +
  geom_line(size = 0.7) +
  geom_point() +
  facet_wrap(~ class_tematica_sug, ncol = 1, strip.position = "top") + # Move os títulos para o topo
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8), # Ajusta o estilo do título #face = "bold"
    strip.background = element_blank(), # Remove o fundo do strip
    panel.spacing = unit(0.5, "lines")  # Ajusta o espaçamento entre os painéis
  ) +
  labs(x = "Ano da Matéria", y = "Quantidade de PLs")


# grafico opcao 3 - ok
df %>% 
  filter(amostra_meioamb == 1) %>%
  mutate(class_tematica_sug = str_replace(class_tematica_sug, " e ", "\ne ")) %>%
  group_by(AnoMateria, class_tematica_sug) %>%
  summarize(count = n()) %>%
  ggplot(aes(AnoMateria, count)) +
  geom_line(size = 0.5, fill = 'darkgray') +
  geom_point() +
  facet_grid(rows = vars(class_tematica_sug)) +
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 10)) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, size = 9), # Texto horizontal e alinhado
    strip.background = element_blank(), # Remove o fundo das caixas
    panel.spacing = unit(0.8, "lines")  # Ajusta o espaçamento entre os grids
  ) +
  labs(x = "Ano da Matéria", y = "Quantidade de PLs")
ggsave("images/PLs_classfinal_by_year.png", width = 10, height = 8, dpi = 300)

## incremento (add media por categoria tematica)
df_summary <- df %>%
  filter(amostra_meioamb == 1) %>%
  group_by(AnoMateria, class_tematica_sug) %>%
  summarize(count = n()) %>%
  group_by(class_tematica_sug) %>%
  mutate(mean_count = mean(count)) # Calcula a média por categoria

df_summary %>%
  ggplot(aes(AnoMateria, count)) +
  geom_line(size = 0.5) +
  geom_point() +
  facet_grid(
    rows = vars(class_tematica_sug),
    labeller = as_labeller(function(x) paste0(x, "\nMédia: ", round(mean(df_summary$mean_count[df_summary$class_tematica_sug == x]), 1)))
  ) +
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 10)) +
  # Adiciona o texto da média logo abaixo do título
  theme_minimal() +
  theme(strip.text.y = element_text(
        angle = 0, hjust = 0, vjust = 0.5, size = 10),  #face = "italic"
    strip.background = element_blank(),
    panel.spacing = unit(0.5, "lines")) +
  labs(x = "Ano da Matéria", y = "Quantidade de PLs")
ggsave("images/PLs_classfinal_by_year_media.png", width = 10, height = 8, dpi = 300)


