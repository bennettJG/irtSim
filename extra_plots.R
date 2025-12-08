# Ben TODO export these for supplementary material
# Run after the writeup file, so that data and plotting functions will be loaded
plot_bias_rmse(
  metrics_long |> filter(groupdiff == 0.4 & assumption == "Equivalent groups"),
  "Theta",
  "Theta"
)
plot_bias_rmse(
  ametrics_long |> filter(groupdiff == 0.4 & assumption == "Equivalent groups"),
  "Discrimination",
  "a"
)
plot_bias_rmse(
  bmetrics_long |> filter(groupdiff == 0.4 & assumption == "Equivalent groups"),
  "Difficulty",
  "b"
)

ggplot(
  groupdiffs |>
    filter(groupdiff == 0 & assumption == "Equivalent groups") |>
    pivot_longer(starts_with("X"), names_to = "iter", values_to = "diff") |>
    mutate(n_anchor = factor(n_anchor)),
  aes(x = n_anchor, y = diff)
) +
  facet_wrap(vars(assumption, groupdiff, anchor_type)) +
  geom_jitter(alpha = 0.2, height = 0, color = "green") +
  geom_violin(color = "blue", fill = "transparent") +
  stat_summary(color = "blue", fun.data = "mean_sdl") +
  theme_bw()

ggplot(
  se_Thetas_long |> mutate(n_anchor = factor(n_anchor)),
  aes(x = n_anchor, y = avg_se_Theta)
) +
  facet_wrap(
    vars(assumption, groupdiff, anchor_type),
    ncol = 1,
    strip.position = "right"
  ) +
  geom_jitter(alpha = 0.2, height = 0, color = "green") +
  geom_violin(color = "blue", fill = "transparent") +
  stat_summary(color = "blue", fun.data = "mean_sdl") +
  theme_bw()

list_point_thetas <- list()
for (i in 1:length(list_results)) {
  point_thetas <- data.frame(matrix(nrow = 1000, ncol = 5))
  names(point_thetas) <- c(
    "pred",
    "pred_SE",
    "true_theta",
    "n_anchor",
    "condition"
  )
  results <- list_results[[i]]$results[[1]]
  point_thetas[, "pred"] <- results$pred_theta[, 1]
  point_thetas[, "pred_SE"] <- results$pred_theta[, 2]
  point_thetas[, "true_theta"] <- c(unlist(results$true_parameters$Thetas))
  point_thetas[, "n_anchor"] <- rep(list_results[[i]]$condition$n_anchor, 1000)
  point_thetas[, "condition"] <- rep(
    paste0(
      list_results[[i]]$condition$assumption,
      ", group diff=",
      list_results[[i]]$condition$groupdiff,
      ", anchor type=",
      list_results[[i]]$condition$anchor_type
    ),
    1000
  )
  list_point_thetas[[i]] <- point_thetas |> arrange(true_theta)
}

point_thetas_large <- bind_rows(list_point_thetas) |>
  mutate(n_anchor = as.factor(n_anchor), condition = as.factor(condition)) |>
  split(~condition)

for (i in 1:length(point_thetas_large)) {
  png(
    filename = paste0(
      "figures/theta_point_est/",
      point_thetas_large[[i]]$condition,
      ".png"
    ),
    width = 6,
    height = 9,
    res = 72,
    units = "in",
    pointsize = 12,
    bg = "white"
  )
  print(
    ggplot(
      point_thetas_large[[i]],
      aes(x = true_theta, y = pred - true_theta)
    ) +
      labs(title = point_thetas_large[[i]]$condition) +
      facet_wrap(
        vars(n_anchor),
        ncol = 1,
        strip.position = "right",
        axes = "all"
      ) +
      geom_errorbar(
        aes(
          ymin = pred - true_theta - pred_SE,
          ymax = pred - true_theta + pred_SE,
          x = true_theta
        ),
        color = "green",
        alpha = 0.3,
        linewidth = 1
      ) +
      geom_point(color = "darkgreen", size = 1, alpha = 0.5) +
      geom_abline(slope = 0, intercept = 0, color = "blue") +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, color = 'blue4', size = 16)
      )
  )
  dev.off()
}
