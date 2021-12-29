#' @title Draws a bubble heatmap
#' @description This function draws a bubble heatmap where tested pathways
#' appear in rows and comparisons in columns. The bubbles are colored by NES and
#' their size is proportional to the FDR.
#' @import scales
#' @name ggbubbleHeatmap
#' @param df Dataframe with the results of all GSEA analyses. Returned by
#' \code{\link[ggseabubble]{GSEAtable}}).
#' @return A \code{ggplot2} object.
#' @examples
#' @export

ggbubbleHeatmap <- function(df) {
  # Color gradient.
  color.gradient <- c(min = "#3B7FB6", center = "white", max = "#DF2727")
  NES <- df %>% select(NES) %>% pull
  min.val <- floor(min(NES, 0))
  max.val <- ceiling(max(NES, 0))
  if (min.val == 0) {
    min.idx <- which(names(color.gradient) == "min")
    color.gradient <- color.gradient[-c(min.idx)]
  }
  if (max.val == 0) {
    max.idx <- which(names(color.gradient) == "max")
    color.gradient <- color.gradient[-c(max.idx)]
  }
  # BubbleHeatmap theme.
  bubble.theme <- list(
    theme_classic(),
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(hjust = 0, margin = margin(rep(3, 4))),
          axis.line = element_blank(),
          legend.box.just = "left"),
    scale_x_discrete(position = "top"),
    labs(x = NULL, y = NULL))
  # Plot.
  p <- ggplot(df, aes(x = COMPARISON, y = NAME)) +
    geom_point(aes(color = NES, size = -log10(FDR.q.val)), shape = 16) +
    scale_color_gradientn(colours = color.gradient, guide = "colorbar",
                          values = scales::rescale(unique(c(min.val, 0,
                                                            max.val))),
                          limits = c(min.val, max.val)) + bubble.theme
  return(p)
}
