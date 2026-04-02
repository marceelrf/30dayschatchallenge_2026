library(tidyverse)
library(ggimage)
library(brasileirao)

# 1. Carregar dados
df <- read_csv(here::here("data/jogadores_selecao_copas.csv"))

# 2. Preparar base de escudos (limpeza antes do join)
br_teams <- brasileirao::teams |> 
  mutate(clube_join = case_when(
    team == "Athletico PR" ~ "Athletico Paranaense",
    team == "Atlético MG" ~ "Atlético Mineiro",
    TRUE ~ team
  ))

# 3. Processar pictograma
df_pict <- df |> 
  select(-copas_do_mundo) |> 
  inner_join(br_teams, by = c("clube" = "clube_join")) |> 
  mutate(clube = fct_reorder(clube, total_de_jogadores)) |>
  uncount(total_de_jogadores) |> 
  group_by(clube) |> 
  mutate(x_posicao = row_number()) |> 
  ungroup() |> 
  mutate(logo = map_chr(team, ~brasileirao::get_badge(.x)))

# 4. Plot
df_pict |> 
  ggplot(aes(x = x_posicao, y = clube)) +
  geom_image(aes(image = logo), size = 0.05) + 
  
  # AJUSTE DE ESPAÇO: expand adiciona folga nas laterais (multiplicador, adição)
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) + 
  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),        
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),        
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(20, 20, 20, 20), # Margem extra ao redor do gráfico todo
    panel.background = element_rect(fill = "#ffffcc",colour = NA),
    plot.background = element_rect(fill = "#ffffcc")
  ) +
  labs(title = "Clubs that have provided the most players to the Brazilian national team")

# 5. Salvar
ggsave(filename = "plots/day02.png",
       width = 20, height = 8, units = "in", dpi = 300, bg = "#ffffcc")

ggsave(filename = "plots/day02.svg",
       width = 20, height = 8, units = "in", dpi = 300, bg = "#ffffcc")
