#' @rdname get_keywords
#' @export
#'
#' @title
#' List keywords from \code{scale_text}
#'
#' @description
#' \code{get_keywords} lists keywords for each scaled text dimension.
#'
#' @param scores List from output of \code{scale_text}.
#' @param n_dimensions Integer scalar or vector. How many/which dimensions to
#' print.
#' @param n_words An integer scalar. How many keywords for each dimension.
#' @param multiply_by_freq A logical scalar. Whether to multiply the word scores by
#' the square root of the words' frequencies. Defaults to TRUE.
#' @param capture_output A logical scalar. Whether to return the output as list
#' of data frames rather than print to console.
#'
#' @examples
#' \dontrun{
#' scores <- scale_text_pcs(
#'     meta=out$meta,
#'     dtm=dtm
#'     )
#'
#' get_keywords(scores, n_dimensions=3, n_words=15)
#' }
#'
#' @seealso \code{\link{plot_keywords}},
#' \code{\link{score_documents_pcs}}, \code{\link{doc_to_dtm}}
#'

get_keywords <- function(
    scores,
    n_dimensions,
    n_words = 15,
    capture_output = FALSE,
    multiply_by_freq = TRUE
) {

  all_keywords <- list()

  for (i in if (length(n_dimensions) == 1) {1:n_dimensions} else {n_dimensions}) {

    if (multiply_by_freq) {
      ordered_words <- scores$vocab[
        order(
          scale(scores$word_scores[ ,i+1], scale=F) * sqrt(scores$word_counts),
          decreasing = TRUE
        )
      ]
      ordered_words <- ordered_words[ordered_words %in% scores$vocab[scores$pivots]]
      ##
      keywords <- data.frame(
        head(
          rev(ordered_words),
          n = n_words
        ),
        head(
          ordered_words,
          n = n_words
        )
      )
      names(keywords) <- c("keywords (-)","(+) keywords")
    } else {
      ordered_words_orig <- scores$vocab[
        order(
          scale(scores$word_scores[ ,i+1], scale=F),
          decreasing = TRUE
        )
      ]
      ordered_words_orig <- ordered_words_orig[ordered_words_orig %in% scores$vocab[scores$pivots]]
      #
      keywords <- data.frame(
        head(
          rev(ordered_words_orig),
          n = n_words
        ),
        head(
          ordered_words_orig,
          n = n_words
        )
      )
      names(keywords) <- c("keywords (-)","(+) keywords")
    }

    if (capture_output) {
      all_keywords[[paste0("D", i)]] <- keywords
    } else {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        ##
        cat("\nDimension", i, "keywords\n\n")
        print(keywords, row.names = F)
        cat("\n")
      } else {
        ##
        print(
          knitr::kable(
            keywords,
            align = "c",
            format = "pandoc",
            caption = paste("Dimension", i, "keywords")
          )
        )
        cat("\n")
      }
    }
  }

  if (capture_output) {
    return(all_keywords)
  }
}
