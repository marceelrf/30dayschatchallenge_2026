library(tidyverse)
library(cowplot)
library(magick)

df_jogadores_copas <- read_csv(file = here::here("data/jogadores_selecao_copas.csv"))

p_limite <- 10

df_agrupado <- df_jogadores_copas %>%
  mutate(clube_agrupado = fct_lump_min(clube, 
                                        min = p_limite, 
                                        w = total_de_jogadores,
                                        other_level = "Others")) %>%
  group_by(clube_agrupado) %>%
  summarise(total = sum(total_de_jogadores)) %>%
  arrange(desc(total))
  

df_plot <- df_agrupado %>%
  mutate(porcentagem = total / sum(total),
         ymax = cumsum(porcentagem),
         ymin = c(0, head(ymax, n=-1)),
         label_pos = (ymax + ymin) / 2) %>% 
  mutate(clube_agrupado = fct_reorder(clube_agrupado, total,.desc = T))



cores <- c("Others" = "gray50",
           "Botafogo" = "gray80",
           "São Paulo" = "#ee1f26",
           "Vasco da Gama" = "black",
           "Flamengo" = "#c02513",
           "Fluminense" = "#8d0529",
           "Palmeiras" = "#056337",
           "Corinthians" = "gray5",
           "Santos" = "gray95",
           "Real Madrid" = "white",
           "Atlético Mineiro" = "grey70",
           "Barcelona"= "#9d2048",
           "Cruzeiro" = "#2d4f99",
           "Roma" = "#940a2b"
           )


p <- ggplot(df_plot, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=clube_agrupado)) +
  geom_rect(color = "black",linewidth = 1) +
  coord_polar(theta="y") +
  scale_fill_manual(values = cores) +
  xlim(c(1, 4)) +
  theme_void() +
  labs(title = paste0("Clubs with more than 10 players called up to the Brazilian national team for World Cups"),
       fill = "Clubs") +
  theme(legend.position = "bottom",
        text = element_text(family = "serif"),
        plot.title = element_text(size = 18,face = "bold"))

cbf_logo <- "https://assets.football-logos.cc/logos/brazil/700x700/brazil-national-team.caceec49.png"

ggdraw(p) +
  draw_image(cbf_logo, 
             x = 0.5, y = 0.5,
             hjust = 0.5, vjust = 0.375,
             width = 0.4, height = 0.4)

ggsave(filename = "plots/day01.svg",scale = 2,bg = "white")
