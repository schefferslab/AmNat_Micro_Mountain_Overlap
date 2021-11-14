

data <- tibble(
  Latitude = seq(0, 75, by = 5),
  Overlap = seq(-15, 15, by = 2)
)


ggplot(data, aes(Latitude, Overlap)) +
  geom_jitter() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18))


ggsave("figures/latitude/latitude_overlap_expect.png",
       width = 9.5, height = 6)

