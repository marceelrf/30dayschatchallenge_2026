library(tidyverse)
library(rvest)

# 1. Definir a URL
url <- "https://pt.wikipedia.org/wiki/Lista_de_times_que_cederam_jogadores_%C3%A0_Sele%C3%A7%C3%A3o_Brasileira_em_Copas_do_Mundo"

# 2. Ler o conteúdo da página
pagina <- read_html(url)

# 3. Extrair todas as tabelas da página
# O rvest procura pela tag HTML <table>
tabelas <- pagina %>% html_nodes("table") %>% html_table(fill = TRUE)

# 4. Selecionar a tabela específica
# Geralmente, a página tem várias tabelas. Você precisa ver qual posição ela ocupa.
# Vamos assumir que a tabela principal é a primeira:
df_jogadores_copas <- tabelas[[1]]

# Visualizar o resultado
head(df_jogadores_copas)

df_jogadores_copas <- janitor::clean_names(df_jogadores_copas)

df_jogadores_copas <- df_jogadores_copas %>% 
  mutate(clube = str_replace_all(clube, "\\[\\d+\\]", ""))

write_csv(df_jogadores_copas,file = here::here("data/jogadores_selecao_copas.csv"))
