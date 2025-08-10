#Análise de pefil academico do Ibmec DF

# --- 0. PREPARAÇÃO DO AMBIENTE ---

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("tidyr")
install.packages("tm")
install.packages("wordcloud2")
installed.packages("stringr")

# Carregar as bibliotecas
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr) # Para separar respostas múltiplas
library(tm)        # Para processamento de texto
library(wordcloud2)# Para a nuvem de palavras
library(stringr)

# Carregar os dados

dados <- read_csv("/Users/luigiajello/Downloads/perfil_academico/ibmec_dataset_perfil_academico.csv")



# --- 1. GRÁFICOS DE BARRAS PARA RESPOSTAS ÚNICAS ---
# Gráfico para 'curso'
dados %>%
  drop_na(curso) %>%
  ggplot(aes(y = fct_infreq(curso))) +
  geom_bar(fill = "#1f77b4") +
  labs(title = "Distribuição por Curso", y = "Curso", x = "Quantidade") +
  theme_minimal()

# Gráfico para 'semestre'
dados$semestre <- factor(dados$semestre, levels = c("1° Semestre", "2° Semestre",
                                                    "3° Semestre", "4° Semestre", 
                                                    "5° Semestre", "6° Semestre", 
                                                    "7° Semestre", "8° Semestre", 
                                                    "9° Semestre", "10° Semestre"))
dados %>%
  drop_na(semestre) %>%
  ggplot(aes(x = semestre)) +
  geom_bar(fill = "#ff7f0e") +
  labs(title = "Distribuição por Semestre", x = "Semestre", y = "Quantidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para 'situacao_atual'
dados %>%
  drop_na(situacao_atual) %>%
  ggplot(aes(y = fct_infreq(situacao_atual))) +
  geom_bar(fill = "#2ca02c") +
  labs(title = "Situação Atual dos Alunos", y = "Situação", x = "Quantidade") +
  theme_minimal()

# Gráfico para 'genero'
dados %>%
  drop_na(genero) %>%
  ggplot(aes(y = fct_infreq(genero))) +
  geom_bar(fill = "#d62728") +
  labs(title = "Distribuição por Gênero", y = "Gênero", x = "Quantidade") +
  theme_minimal()

# Gráfico para 'porque_ibmec'
dados %>%
  drop_na(porque_ibmec) %>%
  ggplot(aes(y = fct_infreq(porque_ibmec))) +
  geom_bar(fill = "#9467bd") +
  labs(title = "Principal Motivação para Escolher o Ibmec",
       y = "Motivação", x = "Quantidade") +
  theme_minimal()

# Gráfico para 'motivo_curso'
dados %>%
  drop_na(motivo_curso) %>%
  ggplot(aes(y = fct_infreq(motivo_curso))) +
  geom_bar(fill = "#8c564b") +
  labs(title = "Principal Motivação para Escolha do Curso",
       y = "Motivação", x = "Quantidade") +
  theme_minimal()

# Gráfico para 'distancia'
dados %>%
  drop_na(distancia) %>%
  ggplot(aes(y = fct_infreq(distancia))) +
  geom_bar(fill = "#e377c2") +
  labs(title = "Distância Percorrida até a Faculdade",
       y = "Distância", x = "Quantidade") +
  theme_minimal()


# --- 2. GRÁFICOS PARA RESPOSTAS MÚLTIPLAS ---

# Função para criar gráficos de respostas múltiplas
plot_multiple_choice <- function(df, column, separator, chart_title) {
  df %>%
    select({{column}}) %>%
    drop_na() %>%
    separate_rows({{column}}, sep = separator) %>%
    mutate(resposta = str_trim({{column}})) %>%
    count(resposta, sort = TRUE) %>%
    ggplot(aes(x = reorder(resposta, n), y = n)) +
    geom_col(fill = "#7f7f7f") +
    coord_flip() +
    labs(title = chart_title, x = "Respostas", y = "Contagem") +
    theme_minimal()
}

# Gráfico para 'area_interesse'
plot_multiple_choice(dados, area_interesse, ",",
                     "Áreas de Interesse Mais Comuns")

# Gráfico para 'atividades_extracurriculares'
plot_multiple_choice(dados, atividades_extracurriculares, ",", 
                     "Atividades Extracurriculares de Interesse")

# Gráfico para 'objetivo_profissional'
plot_multiple_choice(dados, objetivo_profissional, ",", 
                     "Objetivos Profissionais dos Alunos")


# --- 3. NUVEM DE PALAVRAS ---

# Carrega os dados e transforma em um objeto de texto (Corpus)
texto_diferencial <- dados$diferencial_ibmec %>%
  na.omit() %>%
  VectorSource() %>%
  Corpus()

# Limpa o texto removendo pontuação, números e palavras comuns
corpus_limpo <- texto_diferencial %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("portuguese")) %>%
  tm_map(removeWords, c("que", "para", "como", "ibmec", "faculdade",
                        "ter", "ser", "pelo", "pela"))

# Cria uma matriz para contar a frequência de cada palavra
tdm <- TermDocumentMatrix(corpus_limpo)
matriz <- as.matrix(tdm)
frequencia <- sort(rowSums(matriz), decreasing = TRUE)

# Organiza os dados em um data frame final para a nuvem
df_frequencia <- data.frame(word = names(frequencia), freq = frequencia)
# Gerar a nuvem de palavras
wordcloud2(df_frequencia, size = 0.7, shape = 'circle', color = 'random-dark')


#%>% siginifica "E ENTÃO", algo como se fosse "faça isso E ENTÃO outra coisa"