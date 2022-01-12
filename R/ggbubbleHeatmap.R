#' @title Draws a bubble heatmap
#' @description This function draws a bubble heatmap where tested pathways
#' appear in rows and comparisons in columns. The bubbles are colored by NES and
#' their size is proportional to the FDR.
#' @importFrom reshape2 dcast
#' @import tidyverse
#' @import ggtree
#' @import patchwork
#' @name ggbubbleHeatmap
#' @param df Dataframe with the results of all GSEA analyses. Returned by
#' \code{\link[ggseabubble]{GSEAtable}}.
#' @param n.perm Number of permutations used during the GSEA analysis.
#' @param FDR.threshold Threshold of significance. All bubbles with a FDR >=
#' \code{FDR.threshold} will have no fill in order to make the visualization
#' easier.
#' @param bubble.size.range Numeric vector of length 2 indicating the minimum
#' and maximum bubble size.
#' @param cluster.rows Logical. Whether rows should be clustered.
#' @param cluster.cols Logical. Whether cols should be clustered.
#' @param cluster.distance (\code{\link[stats]{dist}}'s \code{method}) Distance
#' measure to be used.
#' @param cluster.method (\code{\link[stats]{hclust}}'s \code{method})
#' Clustering method to be used.
#' @return A \code{patchwork} object.
#' @examples
#' @export

ggbubbleHeatmap <- function(df, n.perm = 1000, FDR.threshold = 0.05,
                            bubble.size.range = c(1, 10), cluster.rows = TRUE,
                            cluster.cols = FALSE,
                            cluster.distance = "euclidean",
                            cluster.method = "complete") {
  # Get wide df.
  wide.df <- reshape2::dcast(df, NAME ~ COMPARISON, value.var = "NES") %>%
    `rownames<-`(.[, "NAME"]) %>% select(-NAME)
  if (cluster.rows) {
    # Clustering.
    hc.rows <- hclust(dist(wide.df, method = cluster.distance),
                      method = cluster.method)
    # Reorder df NAMES.
    df$NAME <- factor(df$NAME, levels = hc.rows$labels[hc.rows$order])
    # Tree plot.
    tree.rows <- ggtree::ggtree(hc.rows, layout = "rectangular")
  } else {
    tree.rows <- patchwork::plot_spacer()
  }
  if (cluster.cols) {
    # Clustering.
    hc.cols <- hclust(dist(t(wide.df), method = cluster.distance),
                      method = cluster.method)
    # Reorder df COMPARISON.
    df$COMPARISON <- factor(df$COMPARISON, levels = hc.cols$labels[hc.cols$order])
    # Tree plot.
    tree.cols <- ggtree::ggtree(hc.cols, layout = "rectangular") +
      scale_x_reverse() + coord_flip()
  } else {
    tree.cols <- patchwork::plot_spacer()
  }
  # Transform the FDR = 0.
  df <- df %>%
    mutate(FDR.q.val = if_else(FDR.q.val == 0 , 1/(n.perm * 10), FDR.q.val))
  # Significance variable.
  df <- df %>%
    mutate(Significance = if_else(FDR.q.val < FDR.threshold, TRUE, FALSE))
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
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(vjust = 0.5, hjust = 0),
          axis.line = element_blank(),
          legend.box.just = "left"),
    scale_y_discrete(position = "right"),
    labs(x = NULL, y = NULL),
    scale_size_continuous(range = bubble.size.range))
  # Bubble plot.
  bubble <- ggplot(df, aes(x = COMPARISON, y = NAME)) +
    geom_point(aes(color = NES, size = -log10(FDR.q.val)), shape = 16) +
    geom_point(data = subset(df, !Significance), stroke = 1, shape = 21,
               aes(size = -log10(FDR.q.val), color = NES, fill = Significance)) +
    scale_color_gradientn(colours = color.gradient, guide = "colorbar",
                          values = scales::rescale(unique(c(min.val, 0,
                                                            max.val))),
                          limits = c(min.val, max.val)) +
    scale_fill_manual(values = "white",
                      labels = paste("FDR.q.val >= ", FDR.threshold)) +
    guides(colour = guide_colorbar(order = 1), size = guide_legend(order = 2),
           fill = guide_legend(order = 3,
                               override.aes = list(size = max(bubble.size.range)))) +
    bubble.theme
  # Merge plots.
  patchwork.heights <- patchwork::plot_layout(heights = c(1, 5))
  patchwork.theme <- theme(plot.margin = unit(rep(0, 4), "cm"))
  column1 <- plot_spacer()/tree.rows + patchwork.heights & patchwork.theme
  column2 <- tree.cols/bubble + patchwork.heights & patchwork.theme
  final <- patchwork::wrap_plots(column1, column2, ncol = 2, widths = c(1, 5))
  return(final)
}
