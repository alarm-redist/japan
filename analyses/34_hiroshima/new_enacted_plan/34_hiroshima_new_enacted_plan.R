new_districts <- read.csv(here::here("analyses/34_hiroshima/new_enacted_plan/34_hiroshima_new_districts.csv"))

# add new districts
new_enacted <- pref %>%
  left_join(new_districts, by = "code")

## Color Palette
PAL <- c('#6D9537', '#DCAD35', '#9A9BB9', '#7F4E28', '#2A4E45', '#364B7F')

# Plot Map
ggplot() +
  geom_sf(data = new_enacted, aes(fill = factor(new_dist))) +
  scale_fill_manual(values = PAL, guide = "none") +

  geom_sf(data = boundary_0, aes(color = type), show.legend = "line",
          fill = NA, lwd = c(0.4, 1.2)) +
  scale_color_manual(values = c("#373C38", "#D0C1BA50")) +

  geom_sf(data = cities, size = 2, shape = 21, fill = "red") +
  geom_sf_text(data = cities, aes(label = names),
               color = c("black", "white", "white"),
               size = 3,
               nudge_x = c(-0.015, 0, 0.05), # adjust the position of the labels
               nudge_y = c(0.05, 0.03, 0), # adjust the position of the labels
               family = "HiraginoSans-W3") +

  ggthemes::theme_map(base_family = "HiraginoSans-W3") +
  theme(legend.position = "right", legend.title = element_blank())

