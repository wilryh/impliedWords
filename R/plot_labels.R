#' @rdname get_labels
#' @export
#'
#' @title
#' List labels from \code{scale_text_pcs}
#'
#' @description
#' \code{plot_labels} lists labels for each scaled text dimension.
#'
#' @param scores List from output of \code{scale_text}.
#' @param dimension Integer scalar or vector. How many/which dimensions to
#' print.
#'
#' @examples
#' \dontrun{
#' plot_labels(scores, n_dimensions=1)
#' }
#'
#' @seealso \code{\link{scale_text_pcs}}
#'

plot_labels <- function(
    scores,
    dimension=1
) {

  if (!requireNamespace("ggwordcloud", quietly = TRUE)) {
    stop(
      "Package \"wordcloud\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }


  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop(
      "Package \"gridExtra\" needed for option \"plot_density=TRUE\" to work. Please install it.",
      call. = FALSE
    )
  }


  all_labels <- list()

    ordered_labels <- scores$embedding_labels[[dimension]] |>
      arrange(value)


    ggwordcloud_show_all_grid <- function(words, freqs,
                                          max_words = Inf,
                                          min_size = 1, max_size = 7,
                                          rotate = TRUE,
                                          grid_size = 0.5,
                                          eccentricity = 1,
                                          rm_outside = T,
                                          expand_margin = 0.2 # fraction to expand axis limits
    ) {
      df <- data.frame(word = words, freq = freqs)
      df <- df[order(df$freq, decreasing = TRUE), ]
      if (max_words != Inf) df <- head(df, max_words)

      df$size <- scales::rescale(df$freq, to = c(min_size, max_size))

      if (rotate) {
        set.seed(123)
        df$angle <- sample(c(0, 90), size = nrow(df), replace = TRUE, prob = c(0.9, 0.1))
      } else {
        df$angle <- 0
      }

      # Basic ggplot
      p <- ggplot2::ggplot(df, ggplot2::aes(label = word, size = size, angle = angle)) +
        ggwordcloud::geom_text_wordcloud(rm_outside = rm_outside,
                                         grid_size = grid_size,
                                         eccentricity = eccentricity) +
        ggplot2::scale_size(range = c(min_size, max_size), guide = "none") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))

      # Expand limits slightly to avoid clipping words on edges
      p + ggplot2::coord_cartesian(clip = "off",
                                   xlim = c(-expand_margin, 1 + expand_margin),
                                   ylim = c(-expand_margin, 1 + expand_margin))
    }

    gridExtra::grid.arrange(
      with(
        ordered_labels |>
          filter(value < 0) |>
          arrange(value) |>
          head(100),
        ggwordcloud_show_all_grid(
          words=label,
          freqs=abs(value)
        )
      ),
      with(
        ordered_labels |>
          filter(value > 0) |>
          arrange(desc(value)) |>
          head(100),
        ggwordcloud_show_all_grid(
          words=label,
          freqs=value
        )
      ),
      nrow=1
    )

}
