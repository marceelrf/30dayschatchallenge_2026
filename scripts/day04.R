library(tidyverse)
library(gghighlight)
library(showtext)
font_add_google("Public Sans", "public_sans")
showtext_auto()

df <- RKaggle::get_dataset("gregorygeorge62/hdi-1990-2022")

BRICS <- c("Brazil","India","China","Russia","South Africa")

colors_brics <- c(
  "Brazil"       = "#2E7D32",
  "Russia"       = "#1565C0", 
  "India"        = "#E65100", 
  "China"        = "#B71C1C", 
  "South Africa" = "#FDD835", 
  "Other"       = "#D1D1D1"  
)

df_plot <- 
  df |> 
  filter(Year %in% c(1990, 2022)) |> 
  mutate(Entity = fct_other(Entity, BRICS,other_level = "Other")) |> 
  group_by(Entity,Year) |> 
  summarise(HDI = mean(`Human Development Index`))

df_plot |> 
  ggplot(aes(x = Year, y = HDI, color = Entity)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual(values = colors_brics, name = "Country") +
  scale_x_continuous(breaks = c(1990,2022)) +
  scale_size_continuous(range = c(2, 5)) +
  labs(
    title = "Trends in Average HDI: BRICS vs. Other Countries",
    subtitle = "The average for the “Others” group (short tail) is calculated separately.",
    x = NULL, # Remover o rótulo "Year"
    y = "HDI",
    caption = "Data from: https://www.kaggle.com/datasets/gregorygeorge62/hdi-1990-2022" 
  ) +
  theme_minimal() +
  theme(
    # Aplicação Global da Fonte
    text = element_text(family = "public_sans"),
    
    # Títulos e Subtítulos
    plot.title = element_text(face = "bold", size = 24, color = "#222222"),
    plot.subtitle = element_text(size = 20, color = "#555555", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, color = "#555555"),
    # Eixos
    axis.title.y = element_text(size = 22, color = "#222222"),
    axis.text = element_text(size = 22, color = "#222222"),
    axis.ticks = element_blank(), # Remove as marcas de eixos
    
    # Gridlines (menos intrusivas)
    panel.grid.major = element_line(color = "#F0F0F0"),
    panel.grid.minor = element_blank(),
    
    # Legenda (posicionar à direita e estilizar)
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 22),
    legend.text = element_text(size = 20),
    legend.background = element_rect(fill = "#FAFAFA", color = NA),
    legend.key = element_blank(), # Remove o fundo dos ícones da legenda
    
    # Ajuste de espaçamento geral
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(filename = "plots/day04.png",dpi = 150,bg = "white")

# df |> 
#   filter(Year %in% c(1990, 2022)) |> 
#   select(Entity, Year, HDI = `Human Development Index`) |> 
#   ggplot(aes(x = Year, y = HDI, color = Entity)) +
#   geom_point(size = 2) +
#   geom_line() +
#   gghighlight(Entity %in% BRICS)
