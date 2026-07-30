#violin + box plot to compare various categories (can be more than 2 woops)
#shows median at line, mean at black diamond.
violin_box_two_cat <- function(df, x, y) {
  df |>
    ggplot(aes(x = {{ x }}, y = {{ y }} )) +
    geom_violin(
      aes(
        colour = {{ x }},
        fill = {{ x }},
      ), alpha = 0.5
    ) +
    geom_boxplot(
      aes(colour = {{ x }}),
      width = 0.1
    ) +
    stat_summary(
      fun = mean, geom = "point", shape = 18, size = 3, color = "black"
    )
}
