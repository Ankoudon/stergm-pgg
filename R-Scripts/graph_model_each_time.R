# Library
library(tidyverse)

# Change directory if necessary
setwd("~/Desktop/pgg_stergm/")

# Set a tibble
main <- tibble(
  estimate = numeric(),
  se = numeric(),
  time = numeric(),
  param = character()
)

# Read all the csv files for each time step
for (i in 1:7) {
  
  # Read csv
  data <- read_csv(paste0("Results/model_", i, ".csv"))
  
  # Add a column for the parameter descriptions
  data <- data |> select(estimate, se) |> 
    mutate(
      time = i,
      param = c("edge-form", "triangle-form", "coop-form", "def-form",
                "wealth-form", "edge-per", "triangle-per", "coop-per",
                "def-per", "wealth-per"),
    )
  
  # Combine each tibble
  main <- bind_rows(main, data)
  
}

# Add a column for the p-value
main <- main |>
  mutate(p = if_else(
    abs(estimate/se) > 1.96, "p-value < 0.05", "otherwise"),
    p = factor(p, levels = c("p-value < 0.05", "otherwise")),
    time = as.character(time)
  )

# Graph for the edge formation
g_1 <- main |>
  filter(param == "edge-form") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-5.5, 3),
                     breaks = seq(-4.5, 3, by = 3)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 25)) +
  labs(
    x = "Time",
    y = "Edge"
  )


# Graph for the triangle formation
g_2 <- main |>
  filter(param == "triangle-form") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-5.5, 3),
                     breaks = seq(-4.5, 3, by = 3)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Triangle"
  )

# Graph for the homophily-cooperation
g_3 <- main |>
  filter(param == "coop-form") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-5.5, 3),
                     breaks = seq(-4.5, 3, by = 3)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Homophily-cooperation"
  )

# Graph for the homophily-defection
g_4 <- main |>
  filter(param == "def-form") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-5.5, 3),
                     breaks = seq(-4.5, 3, by = 3)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Homophily-defection"
  )

# Graph for the absolute wealth difference
g_5 <- main |>
  filter(param == "wealth-form") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-5.5, 3),
                     breaks = seq(-4.5, 3, by = 3)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Absolute wealth difference"
  )

# Graph for the edge persistence
g_6 <- main |>
  filter(param == "edge-per") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-4, 5.5),
                     breaks = seq(-4, 5.5, by = 2)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 25)) +
  labs(
    x = "Time",
    y = "Edge"
  )

# Graph for the triangle persistence
g_7 <- main |>
  filter(param == "triangle-per") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "") +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-4, 5.5),
                     breaks = seq(-4, 5.5, by = 2)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Triangle"
  )

# Graph for the homophily-cooperation persistence
g_8 <- main |>
  filter(param == "coop-per") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "", guide = 'none') +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-4, 5.5),
                     breaks = seq(-4, 5.5, by = 2)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Homophily-cooperation"
  )


# Graph for the homophily-defection persistence
g_9 <- main |>
  filter(param == "def-per") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "", guide = 'none') +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-4, 5.5),
                     breaks = seq(-4, 5.5, by = 2)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Homophily-defection"
  )

# Graph for the absolute wealth difference persistence
g_10 <- main |>
  filter(param == "wealth-per") |>
  ggplot(aes(x = time, y = estimate, color = p)) +
  scale_color_manual(
    values = c("p-value < 0.05" = "black",
               "otherwise" = "gray"),
    name = "", guide = 'none') +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
  theme_minimal() +
  scale_y_continuous(limits = c(-4, 5.5),
                     breaks = seq(-4, 5.5, by = 2)) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)) +
  labs(
    x = "",
    y = "Absolute wealth difference"
  )

# Arrange the graphs (persistence), 13 by 6
ggpubr::ggarrange(
  g_6,
  g_7,
  g_8,
  g_9,
  g_10,
  ncol = 5,
  nrow = 1,
  common.legend = TRUE, 
  legend = "bottom",
  font.label = list(size = 25)
)

# Arrange the graphs (formation), 13 by 6
ggpubr::ggarrange(
  g_1,
  g_2,
  g_3,
  g_4,
  g_5,
  ncol = 5,
  nrow = 1,
  font.label = list(size = 25),
  common.legend = TRUE, 
  legend = "bottom" 
)



