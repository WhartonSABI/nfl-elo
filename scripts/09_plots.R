source("scripts/00_config.R")
library(ggplot2)

curves <- fread(file.path(settings$output_dir, "cv_curves.csv"))
cv_plot <- ggplot(curves, aes(lambda, cv_loss, colour = scope)) +
  geom_line() + scale_x_log10() + facet_wrap(~ model, scales = "free_y") +
  labs(x = "Ridge penalty", y = "Cross-validation log loss") + theme_bw()
calibration <- read_result("validation")$calibration
calibration_plot <- ggplot(calibration, aes(predicted, observed)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) + geom_point(aes(size = rows)) +
  facet_wrap(~ model + outcome) + labs(x = "Predicted probability", y = "Observed rate") + theme_bw()
weekly <- read_result("weekly_uncertainty")
weekly_plot <- ggplot(weekly[role_interactions > 0], aes(week, score, group = player_key)) +
  geom_line(alpha = .15) + facet_grid(role ~ model, scales = "free_y") +
  labs(x = "Week", y = "Opponent-adjusted rating") + theme_bw()
for (name in c("cv_plot", "calibration_plot", "weekly_plot")) {
  plot <- get(name)
  if (interactive()) print(plot)
  ggsave(file.path(settings$output_dir, paste0(name, ".png")), plot, width = 8, height = 5, dpi = 160)
}
