#' @title Draws a bubble heatmap
#' @description This function draws a bubble heatmap where tested pathways
#' appear in rows and comparisons in columns. The bubbles are colored by NES and
#' their size is proportional to the FDR.
#' @importFrom scales rescale
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
  # --- Checks ---
  # Check df.
  if (!is.data.frame(df)) stop('df must be a dataframe.')
  needed.columns <- c("NAME", "COMPARISON", "NES", "FDR.q.val")
  in.df <- needed.columns %in% colnames(df)
  if (!all(in.df)) {
    stop(paste0('Missing columns in df: ',
                paste0(needed.columns[!in.df], collapse = ", "), '.'))
  }
  # Check n.perm.
  if (!is.numeric(n.perm)) stop('n.perm must be numeric.')
  if (length(n.perm) != 1 | n.perm[1]%%1 != 0 | n.perm[1] < 0) {
    stop('n.perm must be a non-negative integer.')
  }
  # Check FDR.threshold.
  if (!is.numeric(FDR.threshold)) stop('FDR.threshold must be numeric.')
  if (length(FDR.threshold) != 1 | FDR.threshold[1] < 0 | FDR.threshold[1] > 1) {
    stop('FDR.threshold must be a positive number between 0 and 1.')
  }
  # Check bubble.size.range.
  if (!is.numeric(bubble.size.range)) stop('bubble.size.range must be numeric.')
  if (length(bubble.size.range) != 2 | any(bubble.size.range < 0)) {
    stop('bubble.size.range must be numeric vector of 2 positive numbers.')
  }
  # Check cluster.rows.
  if (length(cluster.rows) != 1 | !is.logical(cluster.rows)) {
    stop('cluster.rows must be TRUE or FALSE.')
  }
  # Check cluster.cols.
  if (length(cluster.cols) != 1 | !is.logical(cluster.cols)) {
    stop('cluster.cols must be TRUE or FALSE.')
  }
  # --- Code ---
  # Get wide df.
  wide.df <- df %>%
    tidyr::pivot_wider(id_cols = "NAME", names_from = "COMPARISON",
                       values_from = "NES") %>%
    tibble::column_to_rownames("NAME")
  wide.df[is.na(wide.df)] <- 0
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
      ggplot2::scale_x_reverse() + ggplot2::coord_flip()
  } else {
    tree.cols <- patchwork::plot_spacer()
  }
  # Transform the FDR = 0.
  df <- df %>%
    dplyr::mutate(FDR.q.val = dplyr::if_else(FDR.q.val == 0 , 1/(n.perm * 10),
                                             FDR.q.val))
  # Significance variable.
  df <- df %>%
    dplyr::mutate(Significance = dplyr::if_else(FDR.q.val < FDR.threshold, TRUE,
                                                FALSE))
  # Color gradient.
  color.gradient <- c(min = "#3B7FB6", center = "white", max = "#DF2727")
  NES <- df %>%
    dplyr::select(NES) %>%
    dplyr::pull()
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
    ggplot2::theme_classic(),
    ggplot2:: theme(axis.ticks.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                                        hjust = 1),
                    axis.ticks.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_text(vjust = 0.5, hjust = 0),
                    axis.line = ggplot2::element_blank(),
                    legend.box.just = "left"),
    ggplot2::scale_y_discrete(position = "right"),
    ggplot2::labs(x = NULL, y = NULL),
    ggplot2::scale_size_continuous(range = bubble.size.range))
  # Bubble plot.
  bubble <- ggplot2::ggplot(df, ggplot2::aes(x = COMPARISON, y = NAME)) +
    ggplot2::geom_point(aes(color = NES, size = -log10(FDR.q.val)), shape = 16) +
    ggplot2::geom_point(data = subset(df, !Significance), stroke = 1, shape = 21,
                        ggplot2::aes(size = -log10(FDR.q.val), color = NES,
                                     fill = Significance)) +
    ggplot2::scale_color_gradientn(colours = color.gradient, guide = "colorbar",
                                   values = scales::rescale(unique(c(min.val, 0,
                                                            max.val))),
                                   limits = c(min.val, max.val)) +
    ggplot2::scale_fill_manual(values = "white",
                               labels = paste("FDR.q.val >= ", FDR.threshold)) +
    ggplot2::guides(colour = ggplot2::guide_colorbar(order = 1),
                    size = ggplot2::guide_legend(order = 2),
                    fill = ggplot2::guide_legend(
                      order = 3,
                      override.aes = list(size = max(bubble.size.range)))) +
    bubble.theme
  # Merge plots.
  patchwork.heights <- patchwork::plot_layout(heights = c(1, 5))
  patchwork.theme <- ggplot2::theme(plot.margin = unit(rep(0, 4), "cm"))
  column1 <- patchwork::plot_spacer()/tree.rows +
    patchwork.heights &
    patchwork.theme
  column2 <- tree.cols/bubble +
    patchwork.heights &
    patchwork.theme
  final <- patchwork::wrap_plots(column1, column2, ncol = 2, widths = c(1, 5))
  return(final)
}
