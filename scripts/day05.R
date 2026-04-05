library(tidyverse)
library(showtext)
font_add_google("Boldonse", "boldonse")
font_add_google("Public Sans", "public_sans")
showtext_auto()

prop <- .3

depth <- seq(10,50,by =5)

reads_max_allele <- round(prop*depth,digits = 0)

p <- 0.5

probs <- map2_dbl(reads_max_allele,
              depth,
              \(x,y) binom.test(x = x, n = y, p = 0.5)$p.value)

tibble(depth = depth, probs = probs) %>% 
  ggplot(aes(x = depth, y = probs)) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point(color = "#e74c3c", size = 2) +
  geom_hline(yintercept = .05, linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(probs) * 1.1)) +
  labs(y = "p-value (Binomial Test)", 
       x = "Depth",
       title = "The Effect of Depth on the Detection of Allelic Imbalance",
       subtitle = "Ratio set at 0.3") +
  theme_minimal(base_size = 18) +
  theme(
    text = element_text(family = "public_sans"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(face = "bold",family = "boldonse"),
    plot.subtitle = element_text(color = "grey30",family = "boldonse")
  )

ggsave("plots/day05.png",dpi = 300,scale = 1.25)
