library(dplyr)
library(ggplot2)
library(patchwork)
library(knitr)

df_theta_all <- readRDS("df_theta_all.rds")
df_bit_all <- readRDS("df_bit_all.rds")
df_theta_se_all <- readRDS("df_theta_se_all.rds")
df_bit_se_all <- readRDS("df_bit_se_all.rds")

p_theta_metrics <- ggplot(df_theta_all, aes(x = true_theta, y = metric_value, color = metric, linetype = metric)) +
  geom_line(size = 1.2) +
  labs(x = expression("True " * theta), y = NULL) +
  scale_color_grey(start = 0, end = 0.7) +
  facet_wrap(~ items, ncol = 1, labeller = labeller(items = function(x) paste(x, "items"))) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

p_bit_metrics <- ggplot(df_bit_all, aes(x = true_bit, y = metric_value, color = metric, linetype = metric)) +
  geom_line(size = 1.2) +
  labs(x = "True bit score", y = NULL) +
  scale_color_grey(start = 0, end = 0.7) +
  facet_wrap(~ items, ncol = 1, scales = "free_x", labeller = labeller(items = function(x) paste(x, "items"))) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

p_theta_se <- ggplot(df_theta_se_all, aes(x = true_theta, y = SE_value, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  labs(x = expression("True " * theta), y = NULL) +
  scale_color_grey(start = 0, end = 0.7) +
  facet_wrap(~ items, ncol = 1, labeller = labeller(items = function(x) paste(x, "items"))) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

p_bit_se <- ggplot(df_bit_se_all, aes(x = true_bit, y = SE_value, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  labs(x = "True bit score", y = NULL) +
  scale_color_grey(start = 0, end = 0.7) +
  facet_wrap(~ items, ncol = 1, scales = "free_x", labeller = labeller(items = function(x) paste(x, "items"))) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

combined_metrics <- (p_bit_metrics + p_theta_metrics) +
  plot_layout(guides = "collect") + plot_annotation(theme = theme(legend.position = "bottom"))

combined_se <- (p_bit_se + p_theta_se) +
  plot_layout(guides = "collect") + plot_annotation(theme = theme(legend.position = "bottom"))

ggsave("combined_metrics.pdf", combined_metrics, width = 1.2*6.5, height = 1.2*6, device = "pdf")
ggsave("combined_se.pdf", combined_se, width = 1.2*6.5, height = 1.2*6, device = "pdf")
