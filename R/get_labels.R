#' @rdname get_labels
#' @export
#'
#' @title
#' List labels from \code{scale_text_pcs}
#'
#' @description
#' \code{get_labels} lists labels for each scaled text dimension.
#'
#' @param scores List from output of \code{scale_text}.
#' @param n_dimensions Integer scalar or vector. How many/which dimensions to
#' print.
#' @param n_labels Integer scale. How many labels to print.
#' @param capture_output A logical scalar. Whether to return the output as list
#' of data frames rather than print to console.
#'
#' @examples
#' \dontrun{
#' get_labels(scores, n_dimensions=3, n_labels=15)
#' }
#'
#' @seealso \code{\link{scale_text_pcs}}
#'

get_labels <- function(
    scores,
    n_dimensions,
    n_labels = 15,
    capture_output = FALSE
) {

  all_labels <- list()

  for (i in if (length(n_dimensions) == 1) {1:n_dimensions} else {n_dimensions}) {

    ordered_labels <- scores$embedding_labels[[i]] |>
      arrange(value) |>
      pull(label)

    labels <- data.frame(
      head(
        rev(ordered_labels),
        n = n_labels
      ),
      head(
        ordered_labels,
        n = n_labels
      )
    )
    names(labels) <- c("labels (-)","(+) labels")

    if (capture_output) {
      all_labels[[paste0("D", i)]] <- labels
    } else {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        ##
        cat("\nDimension", i, "labels\n\n")
        print(labels, row.names = F)
        cat("\n")
      } else {
        ##
        print(
          knitr::kable(
            labels,
            align = "c",
            format = "pandoc",
            caption = paste("Dimension", i, "labels")
          )
        )
        cat("\n")
      }
    }
  }

  if (capture_output) {
    return(all_labels)
  }
}
