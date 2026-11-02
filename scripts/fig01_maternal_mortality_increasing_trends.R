# Figure 1 maternal mortality trends from 2000-2017 in countires with increasing mortality
# includes codex's suggested changes to make the graph more visually appealing
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

# Codex: Use a unique colour for every country, with line types as an extra cue.
country_order <- sort(unique(plot_data$iso))
palette <- c(
  "#E6194B", "#3CB44B", "#4363D8", "#F58231",
  "#911EB4", "#008080", "#B06000", "#000000",
  "#42BFD0", "#A64D79", "#808000", "#800000",
  "#555555"
)
country_colours <- setNames(rep(palette, length.out = length(country_order)), country_order)
country_lines <- setNames(
  rep(c("solid", "longdash"), each = 8,
      length.out = length(country_order)),
  country_order
)

# create graph with ggplot
fig01_matmor_increase <- ggplot(
  plot_data,
  aes(
    x = year,
    y = matmor,
    color = iso,
    linetype = iso,
    group = iso
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_x_continuous(
    breaks = c(seq(2000, 2016, by = 2), 2017)
  ) +
  scale_y_log10(
    breaks = c(5, 10, 20, 50, 100, 200, 500),
    labels = scales::label_comma()
  ) +
  scale_color_manual(values = country_colours) +
  scale_linetype_manual(values = country_lines) +
  labs(
    title = "Maternal mortality trends, 2000–2017",
    subtitle = "Countries with higher mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality per 100,000 live births\n(log scale)",
    color = "Country (ISO)",
    linetype = "Country (ISO)",
    caption = paste(
      "Log scale: equal vertical distances represent equal proportional changes.",
      "Selection reflects net change between endpoints, not an increase in every year.",
      sep = "\n"
    )
  ) +
  guides(
    color = guide_legend(ncol = 4, byrow = TRUE),
    linetype = guide_legend(ncol = 4, byrow = TRUE)
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11, colour = "#20354A"),
    plot.subtitle = element_text(size = 9, colour = "#59636D", margin = margin(b = 8)),
    plot.caption = element_text(size = 7, colour = "#59636D", hjust = 0, margin = margin(t = 6)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8),
    legend.title.position = "top",
    legend.text = element_text(size = 8),
    legend.key.width = grid::unit(0.7, "cm"),
    legend.key.height = grid::unit(0.35, "cm"),
    legend.margin = margin(2, 2, 2, 2),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    panel.grid.major.y = element_line(colour = "#E5E9ED", linewidth = 0.35),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(8, 8, 8, 8)
  )

fig01_matmor_increase


# save as png 
dir.create("outputs", showWarnings = FALSE)

ggsave(
  filename = "outputs/fig01_increasing_maternal_mortality.png",
  plot = fig01_matmor_increase,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)
