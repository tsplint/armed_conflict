#load libraries
library(tidyverse)
library(ggplot2)

#load data
final_data <- read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)

#create mortality difference
mortality_trends <- final_data |>
  select(iso, year, matmor) |>
  filter(year >= 2000, year <= 2017) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(
    diff_matmor = matmor - matmor[year == 2000]
  ) |>
  ungroup()

print(mortality_trends, n = 20)

# find increasing mortality countries 
increasing_countries <- mortality_trends |>
  filter(year == 2017, diff_matmor > 0) |>
  select(iso, diff_matmor) |>
  arrange(desc(diff_matmor))

# View countries with increasing mortality
print(increasing_countries, n = Inf)

# Count countries
nrow(increasing_countries)

# create plot data 
plot_data <- mortality_trends |>
  semi_join(increasing_countries, by = "iso") |>
  filter(!is.na(matmor))

# check countries 
n_distinct(plot_data$iso)

# create graph with ggplot
fig01_matmor_increase <- ggplot(
  plot_data,
  aes(
    x = year,
    y = matmor,
    color = iso,
    group = iso
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = seq(2000, 2017, by = 2)
  ) +
  scale_color_hue() +
  labs(
    title = "Maternal Mortality Trends",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality per 100,000 live births",
    color = "Country (ISO)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

fig01_matmor_increase


# save as png 
dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/fig01_increasing_maternal_mortality.png",
  plot = fig01_matmor_increase,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)
