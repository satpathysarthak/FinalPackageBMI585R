#' PCA Lollipop plot
#'
#' Lollipop plot is for visualization of Principal Component loadings
#' @export
#'
#' @param x a dataframe or tibble of numerical data.
#' @return a plot object with lollipop plot as output
#' @examples
#' data = iris[,1:4]
#' pcApprox(data,3)
pcLollipop= function(x){
  xs = scale(x, center = TRUE, scale = TRUE)
  xpca = prcomp(xs)
  df = as.data.frame(xpca$rotation)
  df$col = colnames(as.data.frame(x))
  df_long <- df %>% tidyr::pivot_longer(!col, values_to = "value", names_to = "PC")
  plot = ggplot2::ggplot(data = df_long, ggplot2::aes(x=col, y=value)) +
    ggplot2::geom_segment( ggplot2::aes(x=col, xend=col, y=0, yend=value) , size=1, color="blue" ) +
    ggplot2::geom_point() + ggplot2::facet_wrap(~PC)
  return(plot)
}
