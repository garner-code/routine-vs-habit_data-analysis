################################################################################
#################  Sadie Lane 2026 histo dv function        ####################
################################################################################

histogram <- function() {
rainbow <- paletteer::paletteer_c("viridis::turbo", n = 85)
df |>
  ggplot() +
  geom_col(
    aes(x = {{ x }}, y = {{ y }}, colour = {{ x }}, fill = {{ x }}),
    position = position_dodge(0.9)
  ) +
  geom_errorbar(
    aes(x = {{ x }}, y = {{ y }}, ymin = {{ ymin }}, ymax = {{ ymax }}),
    width = 0.2, position = position_dodge(0.9)
  ) +
  facet_grid(block ~ switch) +
  scale_color_paletteer_c("viridis::turbo") +
  scale_fill_paletteer_c("viridis::turbo")
}
