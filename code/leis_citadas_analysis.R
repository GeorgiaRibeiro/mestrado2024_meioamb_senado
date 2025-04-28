#dissertacao mestrado ppgcp/ufpe 2021-2025 :)
# autora: Georgia Ribeiro
# codigo 5. analise quanto as leis citadas nos PLs

# ----- notas ----- #
# paleta de cores: ["0b3954","008b00","824c71","1b9aaa","f19953"]
# - fim das notas - #

# carregar pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(ggplot2)
library(data.table)

# importar bases
df_pls3 = read.csv("data/treated_data/df_pls3.csv") # base de dados completa
df_amb2 = read.csv("data/treated_data/df_amb2.csv") 
df_subtemas = read.csv("data/results_data/df_class_tematica_final.csv") # codigo de topic modeling

#---- identificar leis citadas: estrategia 1 (mais simples. primeira lei citada) ----
library(dplyr)
library(stringr)

df_leis <- df_amb2 %>%
  mutate(FindLei = str_extract(EmentaMateria, "Lei.{0,21}")) %>%
  mutate(FindLei2 = str_remove(FindLei, ",.*")) %>%
  select(CodigoMateria, TipoConteudo, EmentaMateria, FindLei2, FindLei)

## salvar resultado
if (file.exists("data/treated_data/")){
  # save
  write.csv(df_leis, "data/treated_data/df_amb2_leiscit.csv")
} else {
  # create directory and save
  dir.create(file.path("data/treated_data/"))
  write.csv(df_leis, "data/treated_data/df_amb2_leiscit.csv")
}

#---- identificar leis citadas: estrategia 2 (mais complexa, mais de uma lei citada) ----
# Extração, contagem e formatação

df_leis_str2 <- df_amb2 %>%
  mutate(
    ListaLeis_aux = str_extract_all(EmentaMateria, "\\b\\d{1,5}\\.\\d{1,5}\\b"), #"\\d{1,5}\\.\\d{1,5}"),
    QtdLeis = sapply(ListaLeis, length),
    FindLeis = sapply(ListaLeis_aux, function(x) paste(x, collapse = ", "))
  ) %>%
  select(CodigoMateria, TipoConteudo, EmentaMateria, FindLeis, QtdLeis)


## salvar resultado
fwrite(df_leis_str2, file ="data/treated_data/df_amb2_leiscit_2strg.csv")



# ---- quantidade de citacoes recebidas por leis ----
df_count_cit <- separate_rows(df_leis_str2, FindLeis, sep = ", ")

df_count_cit_out = df_count_cit %>%
group_by(FindLeis) %>%
  summarize(count_dist = n_distinct(CodigoMateria)) %>%
  arrange(desc(count_dist))

## salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_count_cit_out, "data/results_data/df_count_cit_out.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_count_cit_out, "data/results_data/df_count_cit_out.csv")
}

# ---- tipo de couteudo e qtd de citacoes recebidas por leis [nao agrega na analise dos resultados] ----
df_count_cit_tipoConteudo_out = df_count_cit %>%
  group_by(TipoConteudo, FindLeis) %>%
  summarize(count_dist = n_distinct(CodigoMateria)) %>%
  arrange(TipoConteudo, desc(count_dist))

## salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_count_cit_tipoConteudo_out, "data/results_data/df_count_cit_tipoConteudo_out.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_count_cit_tipoConteudo_out, "data/results_data/df_count_cit_tipoConteudo_out.csv")
}

# ---- tipo de couteudo e qtd de citacoes a leis por PLs ----
df_count_QtdLeisCit_tipoConteudo_out = df_count_cit %>%
  group_by(TipoConteudo, QtdLeis) %>%
  summarize(count_dist = n_distinct(CodigoMateria)) %>%
  arrange(TipoConteudo, QtdLeis, desc(count_dist))

## salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_count_QtdLeisCit_tipoConteudo_out, "data/results_data/df_count_QtdLeisCit_tipoConteudo_out.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_count_QtdLeisCit_tipoConteudo_out, "data/results_data/df_count_QtdLeisCit_tipoConteudo_out.csv")
}

# conferindo casos de PLs diferentes com a mesma ementa (3 casos apenas)
df_leis_str2 %>%
  group_by(EmentaMateria) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## grafico de qtd de citacoes realizadas
df_leis_str2 %>%
  mutate(QtdLeis = as.character(QtdLeis)) %>%
  group_by(QtdLeis) %>%
  summarize(count = n()) %>%
ggplot(aes(x=QtdLeis, y=count)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "Quantidade de Leis Citadas", y = "Quantidade de PLs") +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.7 = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave("images/qtd_citacao_leis.png", width = 7, height = 4, dpi = 300)

# conferindo casos de PLs diferentes com a mesma ementa (3 casos apenas)
df_amb2 %>%
  group_by(AnoMateria, TipoConteudo) %>%
  summarize(count = n_distinct(CodigoMateria)) %>%
  arrange(AnoMateria)


# ---- TipoConteudo (criação por subtema) ---- 
## alternativa a analise de palavras: usar subtemas no codigo de leis citadas
df_subtemas = df_subtemas %>% select(CodigoMateria, class_tematica_sug) %>%
  rename(class_tematica = class_tematica_sug)
df_tipo_sub = df_amb2 %>% select(CodigoMateria, DescricaoIdentificacaoMateria, TipoConteudo,
                                 EmentaMateria, DataApresentacao) %>%
  left_join(df_subtemas,
            join_by(CodigoMateria == CodigoMateria))

## salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_tipo_sub, "data/results_data/df_tipo_sub.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_tipo_sub, "data/results_data/df_tipo_sub.csv")
}


## - qtd PLs temáticos por tipo
df_tipo_sub %>%
  group_by(TipoConteudo, class_tematica) %>%
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  mutate(class_tematica = str_replace(class_tematica, " ", "\n"),
         class_tematica = case_when(
           class_tematica == "Conservação\nda Natureza e Florestas" ~ "Conservação da\nNatureza e Florestas",
           class_tematica == "Crimes\ne Infrações Ambientais" ~ "Crimes e\nInfrações Ambientais",
           class_tematica == "Política\nFundiária e Reforma Agrária" ~ "Política Fundiária\ne Reforma Agrária",
           class_tematica == "Empreendimentos\nAmbientais e Espaços Urbanos" ~ "Empreendimentos\nAmbientais e\nEspaços Urbanos",
           TRUE ~ class_tematica)) %>%
  ggplot(aes(x=class_tematica, y=count_PLs)) + 
  geom_bar(stat='identity', fill = "darkgray", size = 1) + facet_wrap(~TipoConteudo, ncol = 1) +
  geom_text(aes(label = count_PLs), position = position_dodge(width = 0.9), vjust = -1) +
  scale_y_continuous(limits = c(0, 1))+
  labs(x = "Subtemas", y = "Quantidade de PLs") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     axis.text.x = element_text(size = 9),
                     axis.text.7 = element_text(size = 12),
                     axis.text = element_text(size = 10)) + 
  scale_y_continuous(limits = c(0, 40))
ggsave("images/qtd_subtema_tipoconteudo.png", width = 11, height = 5, dpi = 300)
