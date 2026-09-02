################################################################################
##############    Sadie Lane 2026 Hons Project - plot style   ##################
################################################################################
plot_style <- function() {
  title_sz <- 16
  label_sz <- 14
  theme_classic() +
    theme(
      plot.title = element_text(size = title_sz),
      axis.text.x = element_text(size = label_sz),
      axis.text.y = element_text(size = label_sz),
      legend.text = element_text(size = label_sz),
      axis.title.x = element_text(size = title_sz),
      axis.title.y = element_text(size = title_sz),
      legend.title = element_text(size = label_sz),
      strip.text.x = element_text(size = label_sz)
    )
}
