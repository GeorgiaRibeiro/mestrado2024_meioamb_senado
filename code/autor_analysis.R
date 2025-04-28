# dissertacao mestrado ppgcp/ufpe 2021-2025 :)
# autora: Georgia Ribeiro
# codigo 4. analise quanto aos autores dos PLs

# ----- notas ----- #
# paleta de cores: ["0b3954","008b00","824c71","1b9aaa","f19953"]
# - fim das notas - #

# carregar pacotes
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(ggplot2)

# importar bases
df_pls3 = read.csv("data/treated_data/df_pls3.csv") # base de dados completa
df_autor_atrb = read.csv("data/treated_data/df_autor_atrb.csv") # atributos dos autores
df_temas_pls = read.csv("data/results_data/df_class_tematica_final.csv") # temas dos PLs
df_all = read.csv("data/results_data/df_all_attributes.csv") # atributos dos autores e PLs
df_diap = read.csv("data/original_data/bancada_ruralista_2011_2019.csv") # atributos dos autores e PLs


# ---- FILTRANDO DADOS SOBRE OS AUTORES DE PLs AMBIENTAIS ----
df_autor = df_pls3 %>%
  filter(amostra_meioamb == 1) %>%
  select(c("CodigoMateria", "AnoMateria", "DataApresentacao", "Autor", "TipoIniciativa"))

df_autor %>%
  group_by(TipoIniciativa) %>%
  summarize(count_dist = n_distinct(Autor), count = n()) %>%
  print()

## ---- Único autor por linha e remover Comissões ----
df_autor2 = df_autor %>%
  filter(grepl('Senador', Autor)) %>%
  mutate(autor_un = str_replace(Autor, " e outros.", "")) %>% 
  mutate(autor_un = strsplit(as.character(autor_un), ", ")) %>% # Separa autores coletivos por linha
  unnest(autor_un)

## ---- Manter apenas atributos dos autores: 
df_autor_atrb = df_autor2 %>%
  select(-c("CodigoMateria", "TipoIniciativa")) %>%
  unique()
  
## ---- Identificar sexo, partido e uf (output: df_autor_atrb) ----
df_autor_atrb <- df_autor_atrb %>% 
  mutate(
    Sexo = case_when(grepl("Senadora", autor_un) ~ "F", TRUE ~ "M"),
    autor_un = str_replace(autor_un, "Senador |Senadora ", ""),  # Remove cargo
    partido_uf = str_remove_all(str_extract(autor_un, "\\(.*\\)"), "[()]"),  # Captura o texto dentro dos parenteses e remove os parenteses
    partido = str_extract(partido_uf, "[A-Z]+"),  # Extrai a sigla do partido
    uf = str_extract(partido_uf, "[A-Z]{2}$"),  # Extrai a UF (duas letras no final)
    autor_un = str_replace(autor_un, " \\(.*\\)", "") # Mantem apenas o nome na coluna de autor unico
  ) %>% 
  select(-partido_uf)                                

### salvar e concluir  variaveis auxiliares:
# check if sub directory exists 
if (file.exists("data/treated_data/")){
  # save
  write.csv(df_autor_atrb, "data/treated_data/df_autor_atrb.csv")
} else {
  # create directory and save
  dir.create(file.path("data/treated_data/"))
  write.csv(df_autor_atrb, "data/treated_data/df_autor_atrb.csv")
}



# ---- ANALISE DESCRITIVA DOS AUTORES ----
## ---- autores que mudaram de partido entre legislaturas (7 autores. output: df_an_autor_2partidos) ----
df_autor_atrb %>%
  group_by(autor_un) %>%
  summarize(count_dist = n_distinct(partido)) %>%
  arrange(desc(count_dist))

df_an_autor_2partidos = df_autor_atrb %>%
  group_by(autor_un) %>%
  summarize(count_dist = n_distinct(partido)) %>%
  arrange(desc(count_dist)) %>%
  filter(count_dist == 2)

df_an_autor_2partidos = df_an_autor_2partidos %>%
  inner_join(df_autor_atrb, by = c("autor_un"))

### salvar e concluir  variaveis auxiliares:
# check if sub directory exists 
if (file.exists("data/results_data/")){
  # save
  write.csv(df_an_autor_2partidos, "data/results_data/df_an_autor_2partidos.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_an_autor_2partidos, "data/results_data/df_an_autor_2partidos.csv")
}

## ---- quantidade de autores por ano / legislatura (output: grafico)----
### resgatar autores coletivos não individuais (comissões)
df_coletivo = df_autor %>%
  filter(!grepl('Senador', Autor)) %>%
  select(c("CodigoMateria","AnoMateria", "DataApresentacao", "Autor")) %>% mutate(Flag_comissao = "Comissão") %>% 
  rename(AutorUnique = Autor) %>%
  unique()

#df_coletivo = df_coletivo %>% select(-c("CodigoMateria"))
### unir autores e identificar legislaturas
df_an_autor_ano = df_autor_atrb %>% 
  select(c("AnoMateria", "DataApresentacao", "autor_un")) %>% mutate(Flag_comissao = "Senador(a)") %>%
  rename(AutorUnique = autor_un) %>% # unique() %>%
  union(df_coletivo) %>%
  mutate(
    DataApresentacao = as.Date(DataApresentacao, format = "%Y-%m-%d"),  # Converte para Date
    Legislatura = case_when(
      DataApresentacao >= as.Date("2011-02-01") &
        DataApresentacao <= as.Date("2015-01-31") ~ "54ª",
      DataApresentacao >= as.Date("2015-02-01") &
        DataApresentacao <= as.Date("2019-01-31") ~ "55ª"
    )) %>%
  select(-c("DataApresentacao"))# %>% unique()

## fix 
df_an_autor_ano = df_autor_atrb %>% 
  mutate(CodigoMateria = NA) %>%
  select(c("CodigoMateria", "AnoMateria", "DataApresentacao", "autor_un")) %>% mutate(Flag_comissao = "Senador(a)") %>%
  rename(AutorUnique = autor_un) %>% # unique() %>%
  union(df_coletivo) %>%
  mutate(
    DataApresentacao = as.Date(DataApresentacao, format = "%Y-%m-%d"),  # Converte para Date
    Legislatura = case_when(
      DataApresentacao >= as.Date("2011-02-01") &
        DataApresentacao <= as.Date("2015-01-31") ~ "54ª",
      DataApresentacao >= as.Date("2015-02-01") &
        DataApresentacao <= as.Date("2019-01-31") ~ "55ª"
    )) %>%
  select(-c("DataApresentacao"))# %>% unique()
  
### qtd de autores distintos no total (85)
nrow(df_an_autor_ano[!duplicated(df_an_autor_ano[,c('AutorUnique')]),])

### qtd de autores distintos sendo comissões (5)
df_an_autor_ano %>%
  filter(Flag_comissao == "Comissão") %>%
  summarize(count_dist = n_distinct(AutorUnique))

### qtd de autores distintos por legislatura (54ª = 45, 55ª = 57)
df_an_autor_ano %>%
  group_by(Legislatura) %>%
  summarize(count_dist = n_distinct(AutorUnique))

### qtd de autores distintos por ano - plotar grafico
TwoColors <- c("#80B1D3", "#DCB665")

df_an_autor_ano %>%
  group_by(AnoMateria, Flag_comissao) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(AnoMateria, Flag_comissao, fill = list(count = 0)) %>%
  ggplot(aes(x = AnoMateria, y = count, fill = Flag_comissao, label = count)) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) + # Adiciona rótulos
  scale_fill_manual(name = "", values = TwoColors) +
  scale_x_continuous(breaks = seq(2011, 2018, by = 1)) +
  labs(x = "", y = "Quantidade de Autores") +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 8),
        legend.background = element_rect(size=0.3, linetype="solid",  colour ="lightgray"),
        panel.grid.major = element_blank())
ggsave("images/qtd_autores_amb_ano_fix.png", width = 9, height = 5, dpi = 300)
  
### qtd de autores distintos por ano - gerar tabela
df1 = df_an_autor_ano %>%
  group_by(AnoMateria) %>%
  summarize(count_dist_autor = n_distinct(AutorUnique)) %>%
    arrange(AnoMateria)

## ---- quantidade de partidos e UFs por ano ----
df2 = df_autor_atrb %>%
  group_by(AnoMateria) %>%
  summarize(count_dist_partido = n_distinct(partido), count_dist_uf = n_distinct(uf)) %>%
  arrange(AnoMateria)

### ---- unir quantitativos por ano (output: df_an_descrit_ano)----
df_an_descrit_ano = inner_join(df1, df2, by = c("AnoMateria"))

summary(df_an_descrit_ano)

### salvar e concluir  variaveis auxiliares:
# check if sub directory exists 
if (file.exists("data/results_data/")){
  # save
  write.csv(df_an_descrit_ano, "data/results_data/df_an_descrit_ano.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_an_descrit_ano, "data/results_data/df_an_descrit_ano.csv")
}

## ---- quantidade de autores por partido (output: df_autor_atrb_legis)----
df_autor_atrb_legis = df_autor_atrb %>% mutate(
  DataApresentacao = as.Date(DataApresentacao, format = "%Y-%m-%d"),  # Converte para Date
  Legislatura = case_when(
    DataApresentacao >= as.Date("2011-02-01") &
      DataApresentacao <= as.Date("2015-01-31") ~ "54ª",
    DataApresentacao >= as.Date("2015-02-01") &
      DataApresentacao <= as.Date("2019-01-31") ~ "55ª"
  )) %>%
  select(-c("DataApresentacao")) %>% unique()

df_autor_atrb_legis %>%
  group_by(Legislatura, partido) %>%
  summarize(count_dist_autor = n_distinct(autor_un)) %>%
  arrange(Legislatura, desc(count_dist_autor))

## ---- quantidade de autores por UF ----
df_autor_atrb_legis %>%
  group_by(Legislatura, uf) %>%
  summarize(count_dist_autor = n_distinct(autor_un)) %>%
  arrange(Legislatura, desc(count_dist_autor))

df_autor_atrb_legis %>%
  group_by(Legislatura, uf) %>%
  summarize(count_dist_autor = n_distinct(autor_un)) %>%
  arrange(desc(Legislatura), desc(count_dist_autor))

## ---- quantidade de autores por sexo ----
df_autor_atrb_legis %>%
  group_by(Legislatura, Sexo) %>%
  summarize(count_dist_autor = n_distinct(autor_un)) %>%
  arrange(Legislatura, desc(count_dist_autor))



# ---- ATRIBUTOS COMBINADOS: AUTORES e PLs (outputs: df_all_attributes) ----
df_all = df_autor %>% 
  left_join(df_temas_pls, by = c("CodigoMateria")) %>% select(-c("X")) %>%
  left_join(df_autor_atrb_legis, by = c("Autor", "AnoMateria"))  %>% #select(-c("X")) %>%
  rename(class_tematica = class_tematica_sug) %>%
  unique()

df_all = df_all %>%
  mutate(
    # preencher legislatura para linhas das comissoes (ausentes no df joined)
    autor_un = if_else(is.na(autor_un), Autor, autor_un), 
  
    # resgatar comissões (ausentes no df de atributos indiv.)
    Legislatura = case_when(
      DataApresentacao >= as.Date("2011-02-01") &
        DataApresentacao <= as.Date("2015-01-31") ~ "54ª",
      DataApresentacao >= as.Date("2015-02-01") &
        DataApresentacao <= as.Date("2019-01-31") ~ "55ª"),
      
    # adicionar recorte regional
    regiao = case_when(
      uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      uf %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      uf %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      uf %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ NA_character_)
    ) %>%
  select("CodigoMateria","DataApresentacao","AnoMateria","Legislatura",
                           "class_tematica", "TipoIniciativa", "Autor","autor_un",
                           "Sexo","partido","uf","regiao")

### salvar e concluir:
# check if sub directory exists 
if (file.exists("data/results_data/")){
  # save
  write.csv(df_all, "data/results_data/df_all_attributes.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_all, "data/results_data/df_all_attributes.csv")
}

# ---- ANALISE DESCRITIVA: qtd de PLs ambientais por atributos do autor (outputs:dfs rank...) ----
## ---- autores com mais proposições ambientais ----
df_all %>% group_by(Legislatura, autor_un) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "54ª") %>% select(c("count_PLs")) %>% summary()

df_all %>% group_by(Legislatura, autor_un) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "55ª") %>% select(c("count_PLs")) %>% summary()

df_rank_PLsAmb_autor = df_all %>% group_by(Legislatura, autor_un) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) 
write.csv(df_rank_PLsAmb_autor, "data/results_data/df_rank_PLsAmb_autor.csv")

## ---- partidos com mais proposições ambientais ----
df_all %>% group_by(Legislatura, partido) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "54ª")

df_all %>% group_by(Legislatura, partido) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "55ª")

df_rank_PLsAmb_partido = df_all %>% group_by(Legislatura, partido) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) 
write.csv(df_rank_PLsAmb_partido, "data/results_data/df_rank_PLsAmb_partido.csv")

### grafico top 5 por legislatura
#### df top 5 e outros
df_top5 = df_all %>% 
  group_by(Legislatura, partido) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  mutate(rank = rank(-count_PLs)) %>% 
  filter(!is.na(Legislatura)) %>% 
  mutate(partido = case_when(rank > 5 ~ "Outros", TRUE ~ partido)) %>%
  group_by(Legislatura, partido) %>% 
  summarize(count_PLs = sum(count_PLs)) %>%
  mutate(rank = rank(-count_PLs), rank = ifelse(partido == "Outros", max(rank) + 1, rank)) %>%
  arrange(Legislatura, desc(count_PLs))  # Ordenar corretamente dentro de cada legislatura

# Criar fator ordenado *dentro de cada legislatura*
df_top5$partido <- factor(df_top5$partido, 
                          levels = c(unique(df_top5$partido[df_top5$partido != "Outros"])
                                     , "Outros"))

#### plotar
df_top5 %>%
  ggplot(aes(x = factor(Legislatura), y = count_PLs, fill = partido)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = count_PLs),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) + 
  geom_text(aes(label = partido),
            position = position_dodge(width = 0.9), vjust = -2.5, size = 3, color = "black") +
  scale_fill_manual(values = c(
    "MDB"  = "#A8D5BA", "PSDB" = "#aed2f5", "PDT"  = "#73a9de", 
    "PSB"  = "#FFD699", "PT"   = "#FF9999", "PL"   = "#1f65ab", "Outros"   = "#d4d4d4" 
  )) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  labs(x = "Legislatura", y = "Quantidade de PLs") +
  theme_minimal() +
  theme(legend.position = "none", # Remove legenda
        panel.grid.major = element_blank())
ggsave("images/qtd_PLsAmb_legis_partido.png", width = 9, height = 5, dpi = 300)

## ---- uf (e regiões) com mais proposições ambientais ----
df_all %>% group_by(Legislatura, uf) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "54ª")

df_all %>% group_by(Legislatura, uf) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs)) %>%
  filter(Legislatura == "55ª")

df_all %>% group_by(Legislatura, regiao) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs))


df_rank_PLsAmb_uf_regiao = df_all %>% group_by(Legislatura, uf) %>% 
  summarize(count_PLs_uf = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs_uf)) %>%
  inner_join(na.omit(distinct(df_all[c('uf', 'regiao')])), by = c('uf')) %>% # resgatar de-para com regiao
  group_by(Legislatura , regiao) %>%
  mutate(count_PLs_regiao = sum(count_PLs_uf))
write.csv(df_rank_PLsAmb_uf_regiao, "data/results_data/df_rank_PLsAmb_uf_regiao.csv")

### grafico top 5 por região
#### df top 5 e outros
df_all %>% 
  group_by(Legislatura, regiao) %>% 
  summarize(count_PLs = n_distinct(CodigoMateria), .groups = "drop") %>%
  filter(!is.na(Legislatura)) %>%
  mutate(regiao = fct_reorder(regiao, count_PLs, .desc = TRUE)) %>%  # Ordena regiao pelo count_PLs
  ggplot(aes(x = factor(Legislatura), y = count_PLs, fill = regiao)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = count_PLs), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) + 
  geom_text(aes(label = regiao), position = position_dodge(width = 0.9), 
            vjust = -2.5, size = 3, color = "black") +
  scale_fill_brewer(palette = 'Set3') +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  labs(x = "Legislatura", y = "Quantidade de PLs") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.major = element_blank())

ggsave("images/qtd_PLsAmb_legis_regiao.png", width = 9, height = 5, dpi = 300)




# ---- ANALISE DESCRITIVA: distribuição temática por atributos do autor (outputs:dfs rank...)
## - qtd PLs temáticos por regiao
df_countPLs_uf_out = df_all %>%
  group_by(uf) %>%
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(desc(count_PLs))


### salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_countPLs_uf_out, "data/results_data/df_countPLs_uf_out.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_countPLs_uf_out, "data/results_data/df_countPLs_uf_out.csv")
}

df_countPLs_uf_legs_out = df_all %>%
  group_by(Legislatura, uf) %>%
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, desc(count_PLs))
  

### salvar resultado
if (file.exists("data/results_data/")){
  # save
  write.csv(df_countPLs_uf_legs_out, "data/results_data/df_countPLs_uf_legs_out.csv")
} else {
  # create directory and save
  dir.create(file.path("data/results_data/"))
  write.csv(df_countPLs_uf_legs_out, "data/results_data/df_countPLs_uf_legs_out.csv")
}

summary(df_countPLs_uf_legs_out[df_countPLs_uf_legs_out$Legislatura == "54ª", ])

summary(df_countPLs_uf_legs_out[df_countPLs_uf_legs_out$Legislatura == "55ª", ])

## - qtd PLs temáticos por ano
df_all %>%
  group_by(AnoMateria, class_tematica) %>%
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  ggplot(aes(x=AnoMateria, y=count_PLs)) + 
  geom_line(color = "darkgray", size = 1) + facet_wrap(~class_tematica) + theme_bw()
ggsave("images/qtd_PLsAmb_tematica_ano.png", width = 9, height = 5, dpi = 300)
  

## - qtd PLs temáticos por regiao
df_all %>%
  group_by(regiao, class_tematica) %>%
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  mutate(regiao = case_when(regiao == "Centro-Oeste" ~ "Centro-\nOeste", TRUE ~ regiao)) %>%
  ggplot(aes(x=regiao, y=count_PLs)) + 
  geom_bar(stat='identity', fill = "darkgray", size = 1) + facet_wrap(~class_tematica) +
  theme_bw()
ggsave("images/qtd_PLsAmb_tematica_regiao.png", width = 11, height = 5, dpi = 300)


## - qtd PLs temáticos por ano e top 5 partidos
df_part_maisativos = df_all %>%
  group_by(AnoMateria, class_tematica, partido) %>% # talvez incluir legislatura
  summarize(count_PLs = n_distinct(CodigoMateria)) %>%
  group_by(partido) %>%
  mutate(total_PLs_amb = sum(count_PLs), rank = rank(total_PLs_amb)) %>%
  arrange(desc(total_PLs_amb),partido,  AnoMateria)

  ggplot(aes(x=AnoMateria, y=count_PLs, col = partido)) + 
 # geom_line(aes(x = date, y = value, col = some_other_key)) +
  geom_line() +
  facet_wrap(~class_tematica) + theme_bw()
ggsave("images/qtd_PLsAmb_tematica_ano.png", width = 9, height = 5, dpi = 300)


# ---- ANALISE DESCRITIVA: ATRIBUTOS FONTE EXTERNA (DIAP) ----
# conferir colunas chaves para unir as bases
unique(df_autor_atrb$partido)
unique(df_diap$partido)
filter(df_diap, partido == 'SD')
filter(df_autor_atrb, autor_un == 'Vicentinho Alves') # mudou de partido na legislatura, n considerar partido no join.

## ---- ajustar base ----
df_diap_aj = df_diap %>% 
  separate(legislatura, into = c("inicio_legis", "fim_legis"), sep = "-") %>%
  mutate(senador = if_else(senador == 'Álvaro', 'Alvaro', senador),
         bancada_ruralista = 1,
         inicio_legis = as.numeric(inicio_legis),
         fim_legis = as.numeric(fim_legis)) %>%
  select(c('senador', 'inicio_legis', 'fim_legis', 'profissao', 'bancada_ruralista')) %>%
  unique()

# unir bases
df_all_diap_atrib = df_all %>%
  mutate(AnoMateria = as.numeric(AnoMateria)) %>%
  left_join(df_diap_aj,
            join_by(autor_un == senador, AnoMateria >= inicio_legis, AnoMateria < fim_legis))

## validar uniao - OK
nrow(df_diap_aj %>% unique() %>% drop_na())
nrow(df_all_diap_atrib %>% select(c('autor_un', 'AnoMateria', 'Legislatura', 'profissao', 'bancada_ruralista')) %>%
       unique() %>% drop_na())
             
df_tst = df_all_diap_atrib %>% select(c('autor_un', 'AnoMateria', 'Legislatura', 'profissao', 'bancada_ruralista')) %>%
  unique() %>% inner_join(df_diap_aj, by = c('autor_un' = 'senador'))       

## finalizar base da bancada ruralista
df_all_diap_atrib2 = df_all_diap_atrib %>%
    mutate(bancada_ruralista = if_else(is.na(bancada_ruralista), 0, bancada_ruralista)) %>%
  select(-c('inicio_legis', 'fim_legis', 'X'))

## ---- descritivo ----
### qtd e proporção de autores da bancada ruralista e quantos projetos propuseram
df_all_diap_atrib2 %>%
  group_by(Legislatura, bancada_ruralista) %>%
  summarize(count_dist_autor = n_distinct(autor_un), count_dist_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, bancada_ruralista)

df_all_diap_atrib2 %>%
  group_by(Legislatura) %>%
  mutate(count_dist_autor_leg = n_distinct(autor_un), count_dist_PLs_leg = n_distinct(CodigoMateria)) %>%
  group_by(Legislatura, bancada_ruralista) %>%
  mutate(count_dist_autor = n_distinct(autor_un), count_dist_PLs = n_distinct(CodigoMateria),
         perc_autor_leg = (count_dist_autor / count_dist_autor_leg) * 100,
         perc_PLs_leg = (count_dist_PLs / count_dist_PLs_leg) * 100) %>%
  select(c('Legislatura','bancada_ruralista','count_dist_autor','perc_autor_leg','count_dist_PLs', 'perc_PLs_leg')) %>%
  unique() %>%
  arrange(Legislatura, bancada_ruralista)

### qtd de autores por profissao comparar quem propos ou nao pl ambiental
df_all_diap_atrib2 %>%
  filter(!is.na(profissao)) %>%
  group_by(Legislatura, bancada_ruralista) %>%
  summarize(count_dist_autor = n_distinct(autor_un), count_dist_PLs = n_distinct(CodigoMateria)) %>%
  arrange(Legislatura, bancada_ruralista)

length(unique(df_all_diap_atrib2$autor_un))
nrow(df_all_diap_atrib2 %>% filter(!is.na(profissao)) %>% select(c('autor_un')) %>% unique())
