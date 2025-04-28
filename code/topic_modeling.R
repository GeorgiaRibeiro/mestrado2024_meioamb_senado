# dissertacao mestrado ppgcp/ufpe 2021-2025 :)
# autora: Georgia Ribeiro
# codigo 2. classificação temática dos projetos de lei 

# ----- notas ----- 
# paleta de cores: ["0b3954","008b00","824c71","1b9aaa","f19953"]

# View a single RColorBrewer palette by specifying its name
##display.brewer.pal(n = 8, name = 'RdBu')
# View all palletes by type
#display.brewer.all(type="qual")

# Hexadecimal color specification 
##brewer.pal(n = 8, name = "RdBu")

# - fim das notas - 

# ---- carregar pacotes ---- 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(ggwordcloud)

#install.packages(c("topicmodels", "tm", "tidyverse", "tidytext", "udpipe", "philentropy"))
library(topicmodels)
library(tm)
library(tidyverse)
library(tidytext)
library(udpipe)
library(philentropy) # calculo de similaridade - Distância de Jensen-Shannon


# ---- importar base  ---- 
df = read.csv("data/treated_data/df_pls3.csv")

# ---- SEPARANDO DF AMBIENTAL---- 
## (e mantendo apenas colunas relevantes pra analise)
df_amb = df %>%
  filter(amostra_meioamb == 1) %>%
  select(c("CodigoMateria", "Autor", "DataApresentacao", "Subtemas", "EmentaMateria"))

# ---- CLASSIFICAÇÃO DOS PLS ---- 
# -- 1 pré processamento -- 
# - 1.1 remocao de caracteres extras - 
df_amb$docs <- df_amb$EmentaMateria %>%
  tolower()  %>%
  removePunctuation(ucp = TRUE) %>%
  removeNumbers() %>%
  removeWords(stopwords("portuguese"))

df_amb$docs <- chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇº°",
                      "aeiouaeiouyyaeiouaeiouaeiouaeiouaoaonnaeiouaeiouycc  ",df_amb$docs)


# - 1.2 lemantização (reducao das palavras) ps: processo mais preciso que o stemming.- 
pt_br_model <- udpipe_download_model(language = "portuguese")
ud_model <- udpipe_load_model(pt_br_model$file_model)

annotated <- udpipe_annotate(ud_model, x = df_amb$docs, doc_id = df_amb$CodigoMateria)
annotated_df <- as.data.frame(annotated)

annotated_df_grouped <- annotated_df %>%
  group_by(doc_id) %>%
  summarise(lemmatized_text = paste(lemma, collapse = " ")) %>%
  rename(CodigoMateria = doc_id, docs_lemed = lemmatized_text) %>%
  mutate_at(vars(CodigoMateria), as.numeric)

df_amb = left_join(df_amb, annotated_df_grouped, by = 'CodigoMateria')
df_amb = df_amb %>%
  mutate(docs_lemed = gsub(" m ", " n ", docs_lemed)) # corrigir substituição do "Lei nº" por "m"
  
# - 1.3 remover palavras repetitivas e sem significado temático
extra_stopwords = c("n","NA", "sobre", "providencias", "lei", "artigo","art", "arts", "dispoe", "dispor","dispoar","incluir", "acrescentar",
                    "alterar", "autorizar", "criar", "proibir", "instituir", "estabelecer", "outras", "outro", "providencia",
                    "janeiro", "fevereiro", "marco", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
df_amb = df_amb %>%
  mutate(docs_clean = removeWords(docs_lemed, extra_stopwords))  %>%
  mutate(docs_clean = gsub("  ", " ", docs_clean))

# -- 3 criar Documento Term Matrix  -- 
dtm <- DocumentTermMatrix(VCorpus(VectorSource(df_amb$docs_clean)))
#dtm <- removeSparseTerms(dtm, 0.99)  # Remove termos muito raros, nao feito devido a pouca qtd de palavras

# - 3.1 validar se existem documentos sem textos
row_sums <- slam::row_sums(dtm) # Verificar as somas de termos por documento
print(row_sums)                 
print(which(row_sums == 0)) # rows 90 e 158, cod materia = 124112 e 125507

# - 3.2 remover documentos sem texto (categorizar como "outros")
dtm <- dtm[row_sums > 0, ]  # Mantém apenas documentos com pelo menos uma palavra

# - 3.3 metricas pós remoção
row_sums <- slam::row_sums(dtm) # Verificar as somas de termos por documento
print(mean(row_sums))
print(max(row_sums))
print(which(row_sums == 48)) # rows 65, cod materia = 118684
print(min(row_sums))
print(which(row_sums == 2)) # row 50 e 70, cod materia = 114197 e 116261

# -- 4 Rodar Modelo LDA -- 
lda_model <- LDA(dtm, k = 10, control = list(seed = 1234))
terms(lda_model, 10)  # Mostra as 10 palavras mais relevantes por tópico
topics(lda_model)  # Mostra o tópico dominante para cada documento
perplexity(lda_model, dtm) # Menores valores indicam melhor ajuste. (167.99, com "docs_lemmed". 165.4305, com "docs_clean")


# -- 5 avaliacao -- 
# - Matriz de probabilidades dos tópicos por documento - 
theta <- posterior(lda_model)$topics 

# Conferir as probabilidades para os primeiros 5 documentos
print(theta[1:5, ])

# Salvar
df_theta_asdf <- as.data.frame(theta)
write.csv(df_theta_asdf, "data/treated_data/df_theta_lda_posterior_topics_matriz.csv", row.names = FALSE)

# - Tranformar matriz em df com formato long -#
df_theta <- as.data.frame(as.table(theta))
colnames(df_theta) <- c("doc_id", "topic", "probability") # Ajustar os nomes das colunas
df_theta <- arrange(df_theta, doc_id, topic) # Ordenar o data frame por documento e tópico 

# Add coluna que identifica se o topico é dominante
df_theta <- df_theta %>%
  group_by(doc_id) %>%
  mutate(is_topic_dominant = ifelse(probability == max(probability), 1, 0)) 

# Salvar resultado
write.csv(df_theta, "data/treated_data/df_lda_results_alltopics.csv", row.names = FALSE)

# -- 6. analise do topico dominante para nomear e ajustar se necessario -- 

# - Extraindo a Matriz de tópicos por palavras (Beta) - 
beta <- posterior(lda_model)$terms

# Definir o número de termos a exibir por tópico
n <- 5
# Identificar os top 5 termos por tópico
top5_terms <- apply(beta, 1, function(x) {
  top_indices <- order(x, decreasing = TRUE)[1:n]
  terms <- colnames(beta)[top_indices]
  probabilities <- x[top_indices]
  data.frame(term = terms, probability = probabilities)
})


# Redefinir o número de termos a exibir por tópico
n <- 15
# Identificar os top 10 termos por tópico
top15_terms <- apply(beta, 1, function(x) {
  top_indices <- order(x, decreasing = TRUE)[1:n]
  terms <- colnames(beta)[top_indices]
  probabilities <- x[top_indices]
  data.frame(term = terms, beta = probabilities)
})

# Organizar em um único DataFrame para visualização
df_top5_terms <- do.call(rbind, lapply(seq_along(top5_terms), function(topic) {
  data.frame(topic = topic, top5_terms[[topic]], stringsAsFactors = FALSE)
}))

df_top15_terms <- do.call(rbind, lapply(seq_along(top15_terms), function(topic) {
  data.frame(topic = topic, top15_terms[[topic]], stringsAsFactors = FALSE)
}))

# Salvar resultado
write.csv(df_top15_terms, "data/treated_data/df_lda_top15_terms_by_topic.csv", row.names = FALSE)


# - Plotando top10 termos por tópico - 
# criando paleta com 10 cores
library(RColorBrewer) 
Set3_10 <- colorRampPalette(brewer.pal(8, "Set3"))(10)

df_top10_terms = df_top15_terms %>%group_by(topic) %>% mutate(ranking = row_number()) %>% filter(ranking <= 10)
df_top10_terms %>%
  mutate(topic = factor(topic, levels = sort(unique(as.numeric(topic))),
                        labels = paste("Tópico", sort(unique(as.numeric(topic))))), # Cria fator ordenado com rótulos
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, nrow = 2, ncol= 5, scales = "free") +
  labs(x = NULL, y = "LDA Beta (Importância do termo)", title = "Top 10 termos por tópico") +
  theme_minimal() +
  scale_fill_manual(values = Set3_10) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  coord_flip()+
  scale_x_reordered()
ggsave("images/Top10_terms_topicDominant_LDA.png", width = 14, height = 6, dpi = 300)


# - Comparando topicos com categorizacao do senado - 
df_theta = read.csv("data/treated_data/df_lda_results_alltopics.csv")
df_theta_td = df_theta %>% filter(is_topic_dominant == 1) %>% select(doc_id, topic) 

df_analyse = df_amb %>%
  mutate(doc_id = row_number()) %>%
  left_join(df_theta_td, by="doc_id")

# Salvar resultado
write.csv(df_analyse, "data/treated_data/df_pls_LDAtopicDominant.csv", row.names = FALSE)

df_analyse_subtop = df_analyse %>% group_by(Subtemas, topic) %>%
  summarize(count = n())

df_analyse_topsub = df_analyse %>% group_by(topic, Subtemas) %>%
  summarize(count = n())

# - Criar tabela de frequencia cruzada (crosstab) -- 
df_matriz_cruz = df_analyse_topsub %>%
  pivot_wider(
    names_from = topic,  # Colunas da matriz
    values_from = count, # Valores da matriz
    values_fill = 0      # Preenche valores ausentes com 0
  )

df_matriz_cruz  %>%
  pivot_longer(cols = -Subtemas, # Todas as colunas, exceto Subtemas
               names_to = "topic",
               values_to = "count") %>%
  mutate(topic = str_replace(topic, "NA", "'Outros'"), # Substitui NAs por "Outros"
         topic = factor(topic,
                        levels = c(as.character(1:10), "'Outros'"), # Ordena de 1 a 10 e depois "Outros"
                        labels = c(paste("Tópico", 1:10), "Tópico 'Outros'") # Adiciona os rótulos
                        )) %>%
  ggplot(aes(x = topic, y = Subtemas, fill = count)) +
  geom_tile() +
  geom_text(aes(label=count)) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "white", high = "#1b9aaa") + # Ajuste as cores como preferir
  labs(x = "Classificação LDA",
       y = "Classificação Temática do Senado Federal",
       fill = "Contagem",
       title = "Comparação entre classificações") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=9, face="bold"),
        axis.text.y = element_text(size=9, face="bold"))
ggsave("images/matriz_comp_classifSenado_LDATopics.png", width = 14, height = 6, dpi = 300)
  
# - Revisão de conteudo qualitativa / manual - 
df_analyse %>% filter(topic == 1) %>% select(c(Subtemas, EmentaMateria)) %>% arrange(Subtemas)

# - Dendograma para identificar topicos similares (não ajudou) - 
# Resgatar matriz theta (documento x topicos)
df_theta_matriz = read.csv("data/treated_data/df_theta_lda_posterior_topics_matriz.csv")

matriz_transposed <- t(df_theta_matriz) # Transpor a matriz γ para que os tópicos sejam as linhas
dist_matriz <- distance(matriz_transposed, method = "jensen-shannon") # Calcular a matriz de distância usando Jensen-Shannon

# Agrupamento hierárquico
dist_matriz <- dist(scale(dist_matriz)) # Normalizar os dados (valores extremos dão erro "Error in if (is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536")")
summary(as.vector(dist_matriz)) # conferir valores
hclust_model <- hclust(dist_matriz, method = "ward.D2")
plot(hclust_model)

# opcao 2 - Agnes
library(cluster)
hclust_model2 <- agnes(dist_matriz, method = "ward")
plot(hclust_model2)


# - Nuvem de palavras topicos LDA - 

# 1. Tokenizar os textos, preservando pontuações
stopwords_pt <- stopwords("pt")
extra_stopwords = c("nº", "nºs", "NA", "sobre", "providencias", "lei", "artigo","art", "arts", "dispõe","inclui", "acrescenta",
                    "dá", "altera", "autoriza", "cria", "proibe", "institui", "estabelece", "outras", "outro", "providencia",
                    "janeiro", "fevereiro", "marco", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")

tokens <- df_analyse %>%
  mutate(
    topic = ifelse(is.na(topic), "Outros", as.character(topic)), # Substitui topicos nulos por "Outros"
    topic = paste("Tópico", topic) # Adiciona "Tópico" a todos os valores
  ) %>% 
  unnest_tokens(word, EmentaMateria, token = "words") %>% # Tokenização
  filter(!word %in% append(stopwords_pt, extra_stopwords)) %>% # Remove stopwords
  filter(!str_detect(word, "\\d")) %>%    # Remove números
  count(topic, word, sort = TRUE) # Contar palavras por tópico


tokens_mais2 = filter(tokens, n > 2)
# 2. Criar as nuvens de palavras com ggwordcloud
tokens_mais2 %>%
  filter(topic %in% c('Tópico 3', 'Tópico 4','Tópico 5', 'Tópico 6')) %>%
  mutate(topic = factor(topic, levels = c('Tópico 3', 'Tópico 4','Tópico 5', 'Tópico 6'))) %>%
  ggplot(aes(label = word, size = n, color = topic)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_colour_manual(values=c("#C8CA34","#f691d5","#8DABE9", "#DCB665")) +
  scale_size_area(max_size = 30) + # Ajuste do tamanho máximo das palavras
  facet_wrap(~topic) +
  theme_minimal() +
  theme(panel.spacing = unit(0.5, "lines"),         # Reduz o espaço entre os painéis
        #plot.margin = margin(5, 5, 5, 5),          # Reduz as margens do gráfico
        strip.text = element_text(size = 14)       # Ajusta o tamanho dos rótulos dos painéis
  ) +
  labs(title = "Nuvens de Palavras por Tópico",
       subtitle = "Texto das ementas com tratamentos mínimos",
       x = NULL, y = NULL
  )
ggsave("images/wordcloud_LDATopics_3a6.png", width = 10, height = 8, dpi = 300)


tokens_mais2 %>%
  filter(topic %in% c('Tópico 7', 'Tópico 8','Tópico 9', 'Tópico 10')) %>%
  mutate(topic = factor(topic, levels = c('Tópico 7', 'Tópico 8','Tópico 9', 'Tópico 10'))) %>%
  ggplot(aes(label = word, size = n, color = topic)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_colour_manual(values=c("#1b9aaa","#008b00","#824c71","#f19953")) +
  scale_size_area(max_size = 30) + # Ajuste do tamanho máximo das palavras
  facet_wrap(~topic) +
  theme_minimal() +
  theme(panel.spacing = unit(0.5, "lines"),         # Reduz o espaço entre os painéis
        #plot.margin = margin(5, 5, 5, 5),          # Reduz as margens do gráfico
        strip.text = element_text(size = 14)       # Ajusta o tamanho dos rótulos dos painéis
  ) +
  labs(title = "Nuvens de Palavras por Tópico",
       subtitle = "Texto das ementas com tratamentos mínimos",
       x = NULL, y = NULL
  )
ggsave("images/wordcloud_LDATopics_7a10.png", width = 10, height = 8, dpi = 300)


# ---- REVISAO DA CLASSIFICAÇÃO DOS PLS ----
df = read.csv("data/treated_data/df_pls_LDAtopicDominant_Revised.csv")

df[df==""]<-NA

df = df %>%
  mutate(class_tematica_sug = if_else(!is.na(review_reclass), review_reclass,
                                                label_proposto_e1))

## salvar resultado da classificação
write.csv(select(df, CodigoMateria, class_tematica_sug), "data/results_data/df_class_tematica_final.csv")

# descritiva topicos dominantes apresentados pelo LDA
df %>% group_by(topic,label_proposto_e1) %>% summarise(count = n())

# descritiva classificação final após revisao e realocações
df %>% group_by(class_tematica_sug) %>% summarise(count = n())

# gráfico distribuição dos Pls por classificação final 
library(ggplot2)
install.packages('ggmosaic')
library(ggmosaic)


## distribuição dos PLs subtemáticas ambientais do senado e nova classificaçao temática final
df_grouped = df %>%group_by(Subtemas, class_tematica_sug) %>%
  summarize(count = n()) %>%
  mutate(perc = (count / 205) * 100) %>% #n/sum(n) quando unicategorico
  arrange(Subtemas, count)


## grafico de PLs por subtemática ambiental e ano - nao testei ainda
df_grouped %>%
  ggplot(aes(x = class_tematica_sug, y = count, group = class_tematica_sug, color = class_tematica_sug)) +
  geom_line(size = 0.7) +
  geom_point() +
  #scale_colour_manual(values=c("#1b9aaa","#008b00","#824c71","#f691d5","#f19953"))+
  ylab("Quantidade de PLs") +
  ggtitle("Projetos de Lei por subtemas do Senado e cova classificação temática") +
  theme_bw()
ggsave("images/PLs_subtemas_by_year.png", width = 6, height = 3, dpi = 300)


### grafico mosaicplot -- visualização péssima
ggplot(data = select(df, Subtemas, class_tematica_sug)) +
  geom_mosaic(aes(x = product(Subtemas, class_tematica_sug), fill = Subtemas)) +
  xlab("Nova classificação temática") + ylab("Subtemas Senado")

### grafico barras empilhadas -- visualização ruim
ggplot(df_grouped, aes(x = Subtemas, y = count, fill = class_tematica_sug, label = count)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


### grafico barras empilhadas -- ok
Set3_MoreYellow <- c("#8DD3C7", "#DCB665", "#BEBADA", "#FB8082", "#80B1D3", "#F19501", "#CBE3A3", "#FCCDE5", "#C999BA")

df_grouped_ord = df_grouped %>%
  arrange(Subtemas, desc(count)) %>%
  mutate(group_order = forcats::fct_inorder(interaction(Subtemas, class_tematica_sug)))

df_grouped_ord %>%
  mutate(Subtemas = str_replace(Subtemas, "Política Fundiária e Reforma Agrária", "Política Fundiária e\nReforma Agrária")) %>%
  ggplot(aes(x = Subtemas, y = count, fill = class_tematica_sug, group = group_order, label = count)) +
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 3) + # Adiciona rótulos
  scale_fill_manual(values = Set3_MoreYellow) +
  labs(x = "Subtemas do Senado", y = "Quantidade de PLs") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 7),
        legend.background = element_rect(size=0.3, linetype="solid",  colour ="lightgray"),
        panel.grid.major = element_blank()) +
  guides(fill = guide_legend(title = "Classificação\nAutoral", nrow = 3))
ggsave("images/PLs_classfinal_by_subtemas.png", width = 9, height = 5, dpi = 300)


#theme = theme(legend.byrow = TRUE)
  #geom_text(size = 3, position = position_stack(vjust = 0.5))
