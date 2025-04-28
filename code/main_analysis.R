# dissertacao mestrado ppgcp/ufpe 2021-2025 :)
# autora: Georgia Ribeiro
# codigo 1. validacao da base coletada e descritivos

# ----- notas ----- #
# paleta de cores: ["0b3954","008b00","824c71","1b9aaa","f19953"]
# - fim das notas - #

# carregar pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(ggplot2)
## para nuvem de palavras
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)

# importar bases
df_pls = read.csv("data/original_data/pls2011a19_original_data.csv") # original

df_pls3 = read.csv("data//treated_data/df_pls3.csv") # pre-processada
df_amb2 = read.csv("data//treated_data/df_amb2.csv") # apenas PLs ambientais

df_subtemas = read.csv("data/results_data/df_class_tematica_final.csv") # codigo de topic modeling

# ---- VALIDACAO DA BASE ORIGINAL ---- 
length(df_pls$CodigoMateria) # number of rows
n_distinct(df_pls$CodigoMateria) # number of unique rows

##  DescricaoSubtipoMateria (out: "Projeto de Lei" E "Projeto de Lei do Senado")
df_pls %>%
  group_by(DescricaoSubtipoMateria) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n())

## CasaIniciadoraNoLegislativo (out: "SF" - OK)
df_pls %>%
  group_by(CasaIniciadoraNoLegislativo) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n())

##  DescricaoClasse e Hierarquica
df_pls %>%
  group_by(DescricaoClasse) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)

df_pls %>%
  group_by(DescricaoClasseHierarquica) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)

## Ano 
df_pls %>%
  group_by(AnoMateria) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)

# ---- AJUSTES INICIAIS NA BASE ----
## Resolver linhas duplicadas (concatenar multiplas categorias, out: linhas unicas por PL)
df_pls2 = df_pls %>% 
  group_by(CodigoMateria) %>% 
  mutate(CodigoClasseConcat = paste0(CodigoClasse, collapse = ", "),
         DescricaoClasseConcat = paste0(DescricaoClasse, collapse = ", "),
         DescricaoClasseHiearConcat = paste0(DescricaoClasseHierarquica, collapse = " / ")) %>%
  select(-c('X', 'CodigoClasse','DescricaoClasse', 'DescricaoClasseHierarquica')) %>%
  unique()

n_distinct(df_pls$CodigoMateria) == length(df_pls2$CodigoMateria) # Validacao num. linhas - OK

## Remover ano 2019 (ultima legislatura termina em jan/2019, e PLs desse ano só iniciam em fev.)
df_pls2 = df_pls2 %>% 
  filter(AnoMateria != 2019)

unique(df_pls2$AnoMateria) # Validacao anos incluidos - OK

### salvar e concluir ajustes básicos:
# check if sub directory exists 
if (file.exists("data/treated_data/")){
  # save
  write.csv(df_pls2, "data/treated_data/df_pls2.csv")
} else {
  # create directory and save
  dir.create(file.path("data/treated_data/"))
  write.csv(df_pls2, "data/treated_data/df_pls2.csv")
}



# ---- VARIAVEIS AUXILARES (output: df_pls3) ----
## criar variavel filtro dos PLs de meio ambiente
### verificar temas distintos
df_pls2 %>%
  group_by(DescricaoClasseConcat) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)

### criar variável 
df_pls3 = df_pls2 %>%
  mutate(amostra_meioamb = case_when(grepl("Meio Ambiente", DescricaoClasseHiearConcat)|
                                     grepl("Política Fundiária", DescricaoClasseHiearConcat)|
                                     grepl("Reforma Agrária", DescricaoClasseHiearConcat) ~ 1))

### Validar resultados - OK
df_pls3 %>%
  group_by(amostra_meioamb) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)

df_pls3 %>%
  filter(amostra_meioamb == 1) %>%
  group_by(DescricaoClasseHiearConcat, DescricaoClasseConcat) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 300)


### Conferir temas correlatos nao incluídos (out: Agropecuária e Abastecimento = 63)
df_pls3 %>%
  filter(grepl("Agropecuária e Abastecimento", DescricaoClasseConcat)) %>%
  group_by(DescricaoClasseConcat) %>%
  summarize(count = n()) %>%
  print(n = 300)

## ajustar variável com subtemas
df_pls3 = df_pls3 %>%
  mutate(Subtemas = case_when(
    grepl("Meio Ambiente", DescricaoClasseConcat) ~ "Meio Ambiente",
    grepl("Crimes e Infrações Ambientais", DescricaoClasseConcat) ~ "Crimes e Infrações Ambientais",
    grepl("Poluição", DescricaoClasseConcat) ~ "Poluição",
    .default = DescricaoClasseConcat))

### Validar resultados - OK
df_pls3 %>%
  filter(amostra_meioamb == 1) %>%
  group_by(Subtemas) %>%
  summarize(count = n())

## criar variável tipo da iniciativa
df_pls3 = df_pls3 %>%
  mutate(TipoIniciativa = case_when(
                            grepl("outros", Autor) ~ "Coletiva",
                            grepl("\\)\\, Senador", Autor) ~ "Coletiva",
                            !grepl("Senador", Autor) ~ "Coletiva",
                            .default = "Individual"
                          ))

### Validar resultados - Ok
df_pls3 %>%
  group_by(TipoIniciativa, Autor) %>%
  summarize(count_dist = n_distinct(CodigoMateria), count = n()) %>%
  print(n = 400)

### salvar e concluir  variaveis auxiliares:
# check if sub directory exists 
if (file.exists("data/treated_data/")){
  # save
  write.csv(df_pls3, "data/treated_data/df_pls3.csv")
} else {
  # create directory and save
  dir.create(file.path("data/treated_data/"))
  write.csv(df_pls3, "data/treated_data/df_pls3.csv")
}

# ---- ANALISE DESCRITIVA BASE GERAL (output: graficos) ---- 
## distribuição dos PLs por tipo da iniciativa e temática ambiental
df_pls3 %>%
  group_by(TipoIniciativa) %>%
  summarize(n_pl_geral = n(),
            n_pl_ambiental = sum(amostra_meioamb == 1, na.rm = TRUE)) %>%
  mutate(perc_geral = (n_pl_geral / sum(n_pl_geral)) * 100,
         perc_ambiental = (n_pl_ambiental / sum(n_pl_ambiental)) * 100) %>%
  select(c('TipoIniciativa','n_pl_geral','perc_geral','n_pl_ambiental','perc_ambiental')) %>%
  arrange(desc(n_pl_geral))

## distribuição dos PLs por subtemática ambiental original do senado
df_pls3 %>%
  filter(amostra_meioamb == 1) %>%
  group_by(Subtemas) %>%
  summarize(n = n()) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(desc(n))


## grafico de PLs por subtemática ambiental e ano
df_pls3 %>% filter(amostra_meioamb == 1) %>%  group_by(AnoMateria, Subtemas) %>%  summarize(count = n()) %>%
  ggplot(aes(x = AnoMateria, y = count, group = Subtemas, color = Subtemas)) +
  geom_line(size = 0.7) +
  geom_point() +
  scale_colour_manual(values=c("#1b9aaa","#008b00","#824c71","#f691d5","#f19953"))+
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  ylab("Quantidade de PLs") +
  ggtitle("Projetos de Lei por ano e subtemas ambientais") +
  theme_bw() +
  theme(panel.grid.major = element_blank())
ggsave("images/PLs_subtemas_by_year.png", width = 6, height = 3, dpi = 300)


## grafico destacando PLs ambientais dos demais por ano
df_pls3 %>% mutate(label_PLs = case_when(amostra_meioamb == 1 ~ "PLs ambientais",
                               .default = "PLs sobre outras temáticas")) %>%
            group_by(AnoMateria, label_PLs) %>%
            summarize(count = n(), .groups = "drop") %>%
  ggplot(aes(x = AnoMateria, y = count, group = label_PLs, color = label_PLs)) +
  geom_line(size = 0.7) +
  geom_point() +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0), vjust = -1, size = 3.5, color = 'gray30', show.legend = FALSE) +
  scale_colour_manual(values=c("green4", "darkslategray3"))+
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  scale_y_continuous(breaks = seq(0, 900, by = 100), limit=c(0,900)) +
  labs(x="", y="Quantidade de PLs", fill = "", colour = "", 
       title = "Projetos de Lei ambientais em relação aos demais temas") +
  scale_fill_discrete(guide="none") + 
  theme_bw() + #theme_minimal
  theme(panel.grid.major = element_blank())
ggsave("images/PLs_by_year.png", width = 6, height = 3, dpi = 300)


# ---- FILTRANDO PLs AMBIENTAIS (output: df_amb) ---- 
## (e mantendo apenas colunas relevantes pra analise)
df_amb = df_pls3 %>%
  filter(amostra_meioamb == 1) %>%
  select(-c("SiglaCasaIdentificacaoMateria", "SiglaSubtipoMateria", "DescricaoSubtipoMateria", "NumeroMateria", 
            "DescricaoObjetivoProcesso", "IdentificacaoProcesso", "CasaIniciadoraNoLegislativo",
            "IndicadorComplementar", "SiglaCasaLeitura"
            ))


# ---- ANALISE VARIAVEIS ORIGINAIS ---- 
## ---- IndexacaoMateria: indica o tipo da materia ----
unique(df_amb$IndexacaoMateria)
length(unique(df_amb$IndexacaoMateria))

### aumentar padronizacao
df_amb2 = df_amb %>% mutate(
  #IndexacaoAjustada = str_replace(IndexacaoMateria, "PROJETO DE LEI, SENADO", "-"),
  IndexacaoAjustada2 = substr(str_replace(IndexacaoMateria, "PROJETO DE LEI, SENADO", "-"), 1, 50))

unique(df_amb2$IndexacaoAjustada2)


# ---- CRIANDO VARIAVEIS (output: df_amb2)---- 
## ---- TipoConteudo: com base na IndexacaoMateria ----
### usar palavras chaves da ementa quando a IndexacaoMateria nao for suficiente
terms_criacao <- c('Propõe', 'Estabelece', 'Dispõe', 'Proíbe', 'Cria', 'Institui')
terms_alteracao <- c('Altera')

df_amb2 = df_amb2  %>% mutate(
  TipoConteudo = case_when(grepl("ALTERAÇÃO", IndexacaoMateria)
                           & !grepl(paste(terms_criacao, collapse='|'), EmentaMateria) ~ "Alteração",
                           grepl("CRIAÇÃO", IndexacaoMateria) ~ "Criação",
                           grepl("LEI NOVA", IndexacaoMateria) ~ "Criação",
                           grepl(paste(terms_alteracao, collapse='|'), EmentaMateria) ~"Alteração",
                           grepl(paste(terms_criacao, collapse='|'), EmentaMateria) ~"Criação"))

### salvar e concluir  variaveis auxiliares:
# check if sub directory exists 
if (file.exists("data/treated_data/")){
  # save
  write.csv(df_amb2, "data/treated_data/df_amb2.csv")
} else {
  # create directory and save
  dir.create(file.path("data/treated_data/"))
  write.csv(df_amb2, "data/treated_data/df_amb2.csv")
}

### validar resultados - OK
df_amb2 %>%
  group_by(TipoConteudo) %>%
  summarize(count = n())

#dfTeste = filter(df_amb2, is.na(TipoConteudo))

# verificar inconsistencias
dfTeste = filter(df_amb2, TipoConteudo == "Criação" & grepl("ALTERAÇÃO", IndexacaoMateria)) # out = 13, apenas 2 erros
dfTeste = df_amb2 %>% filter(TipoConteudo == "Alteração" & grepl("CRIAÇÃO", IndexacaoMateria)) %>% # out = 11. nenhum erro.
  select(IndexacaoMateria, EmentaMateria)

# ---- APROFUNDANDO ANALISE DO "TIPO CONTEUDO" (output: Nuvens de palavras)  ---- 
## ---- versao indexacao completa ----
df_cloud = df_amb2 %>% select(CodigoMateria, TipoConteudo, IndexacaoMateria) 

# Criar o corpus
docs <- corpus(df_cloud, docid_field = "CodigoMateria", text_field = "IndexacaoMateria")

check_dfm = docs |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("portuguese")) |>
  dfm() 

# Plotar
par(mar = rep(0.5, 4))  # Ajusta as margens da plotagem
docs |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("portuguese")) |>
  dfm() |>
  dfm_group(groups = TipoConteudo) |>
  dfm_trim(min_termfreq = 5, verbose = TRUE) |>
  textplot_wordcloud(comparison = TRUE,
                     adjust = TRUE,
                     max_size = 6,
                     min_size = 1.5,
                     labelsize = 0.7,
                     # max_words = 150,
                     min_count = 5,
                    #scale=c(5, 1)
  )
# nao sei como salvar via codigo, equivalente ao ggsave.


## ---- versao reduzida ----
df_cloud = df_amb2 %>% select(CodigoMateria, TipoConteudo, IndexacaoAjustada2) 

# Criar o corpus
docs <- corpus(df_cloud, docid_field = "CodigoMateria", text_field = "IndexacaoAjustada2")

# Plotar
par(mar = rep(0.5, 4))  # Ajusta as margens da plotagem
docs |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("portuguese")) |>
  dfm() |>
  dfm_group(groups = TipoConteudo) |>
  dfm_trim(min_termfreq = 2, verbose = TRUE) |>
  textplot_wordcloud(comparison = TRUE,
                     adjust = TRUE,
                     max_size = 8,
                     min_size = 2,
                     labelsize = 0.7,
                     # max_words = 150,
                     #min_count = 2,
                     #scale=c(2, 1)
  )
# nao sei como salvar via codigo, equivalente ao ggsave.

# tentar salvar df com palavras chaves [dificuldade: estar agrupado por tipo nao por PL]
df_amb2_corpus = docs |>
  tokens(remove_punct = TRUE) |>
  tokens_remove(stopwords("portuguese")) |>
  dfm() |>
  dfm_group(groups = TipoConteudo) |>
  dfm_trim(min_termfreq = 2, verbose = TRUE)

## alternativa a analise de palavras: usar subtemas no codigo de leis citadas
