# Regenerates documentation_files/unk_beta_binomial_example.png, the worked
# example of the beta and binomial steps in reassign_unk_marks().
# Run from the project root: source("documentation_files/unk_beta_binomial_example.R")

suppressMessages({library(ggplot2); library(patchwork); library(dplyr)})

n_um <- 30; n_ad <- 10   # known-mark fish in the example group
k    <- 6                # UNK fish in one catch record
a <- n_um + 0.5; b <- n_ad + 0.5   # Jeffreys prior, as in reassign_unk_marks()

# Seed chosen so the example shows a typical outcome (rate near 74%, 4 of 6 to UM)
seed <- purrr::detect(1:500, \(s) {
  set.seed(s); d <- rbeta(20, a, b)
  abs(d[1] - 0.74) < 0.02 && rbinom(1, k, d[1]) == 4
})
set.seed(seed)
draws <- rbeta(20, a, b); p <- draws[1]
um <- rbinom(1, k, p)

cols <- c(UM = "#762a83", AD = "#1b7837")

# Panel 1: the known fish in one group
fish <- tibble(i = 1:(n_um + n_ad), mark = rep(c("UM", "AD"), c(n_um, n_ad)),
               x = (i - 1) %% 10, y = -((i - 1) %/% 10))
p1 <- ggplot(fish, aes(x, y, fill = mark)) +
  geom_point(shape = 21, size = 6, colour = "white") +
  scale_fill_manual(values = cols, name = NULL) +
  coord_equal(clip = "off") + scale_y_continuous(expand = expansion(add = 0.6)) + theme_void() +
  labs(title = "1. Known-mark fish in one group",
       subtitle = sprintf("%d UM + %d AD = %d fish  (observed UM rate %.0f%%)",
                          n_um, n_ad, n_um + n_ad, 100 * n_um / (n_um + n_ad))) +
  theme(legend.position = "bottom")

# Panel 2: beta distribution of the UM rate, compared with a sparse group
grid <- tibble(p = seq(0, 1, length.out = 501)) |>
  mutate(`30 UM, 10 AD` = dbeta(p, a, b), `7 UM, 3 AD (sparse)` = dbeta(p, 7.5, 3.5)) |>
  tidyr::pivot_longer(-p, names_to = "group", values_to = "density")
p2 <- ggplot(grid, aes(p, density, colour = group, linetype = group)) +
  geom_line(linewidth = 1) +
  geom_rug(data = tibble(p = draws), aes(p), inherit.aes = FALSE, colour = "grey40", length = unit(0.06, "npc")) +
  geom_vline(xintercept = p, colour = "#d95f02", linewidth = 1) +
  annotate("label", x = p + 0.04, hjust = 0, y = max(grid$density) * 0.95,
           label = sprintf("this run's draw\np = %.2f", p), colour = "#d95f02", size = 3.3) +
  scale_colour_manual(values = c("black", "grey55"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "2. Beta step: how sure are we of the UM rate?",
       subtitle = "Curve = plausible UM rates; ticks = 20 possible draws; one draw is used",
       x = "UM rate", y = NULL) +
  theme_minimal() + theme(legend.position = "bottom", axis.text.y = element_blank())

# Panel 3: binomial split of one UNK record
split <- tibble(n_um = 0:k, prob = dbinom(0:k, k, p), picked = n_um == um)
p3 <- ggplot(split, aes(factor(n_um), prob, fill = picked)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(prob, accuracy = 1)), vjust = -0.4, size = 3.3) +
  scale_fill_manual(values = c(`TRUE` = "#762a83", `FALSE` = "grey80"), guide = "none") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  labs(title = sprintf("3. Binomial step: split a record of %d UNK fish", k),
       subtitle = sprintf("Chance of each outcome at p = %.2f; this run: %d UM + %d AD", p, um, k - um),
       x = "UNK fish assigned to UM", y = NULL) +
  theme_minimal()

fig <- (p1 | p2 | p3) + plot_layout(widths = c(0.8, 1.2, 1)) +
  plot_annotation(title = "How an UNK fin mark is reassigned",
                  caption = "Beta(n_UM + 0.5, n_AD + 0.5) for the UM rate, then Binomial(UNK fish, rate) for the split.")
ggsave(here::here("documentation_files", "unk_beta_binomial_example.png"),
       fig, width = 15, height = 5.2, dpi = 150, bg = "white")
