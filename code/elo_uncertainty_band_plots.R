library(tidyverse)

weekly <- read.csv("~/Downloads/weekly_elo_uncertainty_summary.csv")

# Make sure weeks sort naturally
weekly <- weekly %>%
  mutate(
    week_label = week,
    week_num = suppressWarnings(as.integer(week)),
    week_num = ifelse(is.na(week_num), 1, week_num)
  ) %>%
  arrange(week_index, player_name)

# ----------------------------------------
# Pick top 10 per role based on final week mean Elo
# ----------------------------------------
final_week <- max(weekly$week_index, na.rm = TRUE)

top_players <- weekly %>%
  filter(week_index == final_week) %>%
  group_by(role) %>%
  slice_max(order_by = mean_final_elo, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_name, role)

plot_dat <- weekly %>%
  semi_join(top_players, by = c("player_name", "role"))

# ----------------------------------------
# Helper: uncertainty trajectory plot
# ----------------------------------------
make_uncertainty_plot <- function(df, role_name) {
  df_role <- df %>%
    filter(role == role_name)
  
  ggplot(df_role, aes(x = week_index, y = mean_final_elo, group = player_name)) +
    geom_ribbon(
      aes(ymin = q025, ymax = q975, fill = player_name),
      alpha = 0.15,
      color = NA
    ) +
    geom_line(aes(color = player_name), linewidth = 0.9) +
    geom_point(aes(color = player_name), size = 1.5) +
    geom_line(
      aes(y = observed_elo, color = player_name),
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    scale_x_continuous(
      breaks = sort(unique(df_role$week_index)),
      labels = df_role %>%
        distinct(week_index, week_label) %>%
        arrange(week_index) %>%
        pull(week_label)
    ) +
    labs(
      title = paste(role_name, "Elo Uncertainty Over the Season"),
      subtitle = "Solid line = bootstrap mean, shaded band = 95% interval, dashed line = observed Elo",
      x = "Week",
      y = "Elo",
      color = "Player",
      fill = "Player"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}

p_rushers_uncertainty  <- make_uncertainty_plot(plot_dat, "Rusher")
p_blockers_uncertainty <- make_uncertainty_plot(plot_dat, "Blocker")

p_rushers_uncertainty
p_blockers_uncertainty

make_facet_plot <- function(df, role_name) {
  df_role <- df %>%
    filter(role == role_name)
  
  # order facets by final-week mean Elo
  facet_levels <- df_role %>%
    filter(week_index == max(week_index)) %>%
    arrange(desc(mean_final_elo)) %>%
    pull(player_name)
  
  week_breaks <- df_role %>%
    distinct(week_index, week_label) %>%
    arrange(week_index)
  
  df_role <- df_role %>%
    mutate(player_name = factor(player_name, levels = facet_levels))
  
  ggplot(df_role, aes(x = week_index, y = mean_final_elo)) +
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    geom_line(aes(y = observed_elo), linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~ player_name) +
    scale_x_continuous(
      breaks = week_breaks$week_index,
      labels = week_breaks$week_label
    ) +
    labs(
      title = paste(role_name, "Elo Uncertainty Over the Season"),
      subtitle = "Solid line = bootstrap mean, shaded band = 95% interval, dashed line = observed Elo",
      x = "Week",
      y = "Elo"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )
}

p_rushers_facet  <- make_facet_plot(plot_dat, "Rusher")
p_blockers_facet <- make_facet_plot(plot_dat, "Blocker")

p_rushers_facet
p_blockers_facet



