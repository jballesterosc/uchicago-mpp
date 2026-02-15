library(ggplot2)

pmf <- data.frame(
  x = 0:6,
  p = c(0, 0.2, 0, 0.3, 0, 0.1, 0.4)
)

# sanity check
stopifnot(all(pmf$p >= 0), abs(sum(pmf$p) - 1) < 1e-9)

ggplot(pmf, aes(x, p)) +
  geom_segment(aes(xend = x, y = 0, yend = p), linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(
    data = subset(pmf, p > 0),
    aes(label = scales::number(p, accuracy = 0.01), y = p + 0.04),
    size = 3.5
  ) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(title = "PMF of Y", x = "Y", y = "f(y)") +
  theme_minimal(base_size = 12)
