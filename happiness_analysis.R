# ============================================
# World Happiness Analysis
# Week 2 Assignment - R Data Visualization
# ============================================

# 1. Load packages
library(tidyverse)
library(readxl)

# 2. Load dataset
happiness <- read_excel("data/WHR26_Data_Figure_2.1.xlsx")

# 3. Clean data
happiness_clean <- happiness %>%
  filter(
    !is.na(`Country name`),
    !is.na(`Life evaluation (3-year average)`)
  )

# ============================================
# 4. Visualization 1 - Top 10 Countries
# ============================================

top10_2025 <- happiness_clean %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  arrange(desc(`Life evaluation (3-year average)`)) %>%
  slice_head(n = 10)

ggplot(
  top10_2025,
  aes(
    x = reorder(
      `Country name`,
      `Life evaluation (3-year average)`
    ),
    y = `Life evaluation (3-year average)`
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Life Evaluation",
    subtitle = "Latest Year in the Dataset",
    x = "Country",
    y = "Life Evaluation Score"
  ) +
  theme_minimal()

ggsave(
  "graphs/top10_happiness.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================
# 5. Visualization 2 - Histogram
# ============================================

ggplot(
  happiness_clean,
  aes(x = `Life evaluation (3-year average)`)
) +
  geom_histogram(bins = 20) +
  labs(
    title = "Distribution of Life Evaluation Scores",
    x = "Life Evaluation Score",
    y = "Number of Observations"
  ) +
  theme_minimal()

ggsave(
  "graphs/happiness_histogram.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================
# 6. Visualization 3 - Scatter Plot
# ============================================

ggplot(
  happiness_clean,
  aes(
    x = `Explained by: Log GDP per capita`,
    y = `Life evaluation (3-year average)`
  )
) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Relationship Between GDP and Life Evaluation",
    x = "Log GDP per Capita",
    y = "Life Evaluation Score"
  ) +
  theme_minimal()

ggsave(
  "graphs/gdp_happiness_scatter.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================
# 7. Visualization 4 - Line Chart
# ============================================

yearly_happiness <- happiness_clean %>%
  group_by(Year) %>%
  summarise(
    Average_Happiness = mean(
      `Life evaluation (3-year average)`,
      na.rm = TRUE
    )
  )

ggplot(
  yearly_happiness,
  aes(
    x = Year,
    y = Average_Happiness
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Life Evaluation Over Time",
    x = "Year",
    y = "Average Life Evaluation"
  ) +
  theme_minimal()

ggsave(
  "graphs/happiness_trend.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================
# 8. Visualization 5 - Box Plot
# ============================================

happiness_clean <- happiness_clean %>%
  mutate(
    GDP_Group = ntile(
      `Explained by: Log GDP per capita`,
      4
    )
  )

happiness_clean <- happiness_clean %>%
  mutate(
    GDP_Group = factor(
      GDP_Group,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Low GDP",
        "Lower-Middle GDP",
        "Upper-Middle GDP",
        "High GDP"
      )
    )
  )

ggplot(
  happiness_clean,
  aes(
    x = GDP_Group,
    y = `Life evaluation (3-year average)`
  )
) +
  geom_boxplot() +
  labs(
    title = "Life Evaluation Across GDP Groups",
    x = "GDP Group",
    y = "Life Evaluation Score"
  ) +
  theme_minimal()

ggsave(
  "graphs/happiness_boxplot.png",
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================
# 9. Check saved graphs
# ============================================

list.files("graphs")
