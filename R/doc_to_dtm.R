#' @rdname doc_to_dtm
#' @export
#'
#' @title
#' Convert list of documents from stm package to sparse term-document matrix
#'
#' @description
#' \code{doc_to_dtm} converts list of documents from stm package
#' \code{prepDocuments} function to a sparse term-document matrix.
#'
#' @param out A list of the output from R package \code{stm}'s
#' \code{prepDocuments}.
#' @param binary A logical scalar. If TRUE (default) then only count one
#' occurrence of a word in a document.
#'
#' @examples
#'
#' \dontrun{
#' library(stm)
#'
#' processed <- textProcessor(
#'     input_data$text,
#'     data.frame(input_data),
#'     removestopwords=T, lowercase=T, stem=F
#'     )
#' out <- prepDocuments(
#'     processed$documents, processed$vocab, processed$meta
#'     )
#'
#' ddm <- doc_to_dtm(out)
#' }
#'
#' @seealso \code{\link[stm]{prepDocuments}},
#' \code{\link[stm]{textProcessor}},
#' \code{\link{scale_text_pcs}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{score_documents_pcs}}
#'

doc_to_dtm <- function(
    out,
    binary = TRUE
) {
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop(
      "Package \"reshape2\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  d2 <- reshape2::melt(lapply(out$documents, function(x) x[1, ]))
  d2 <- data.frame(
    reshape2::melt(
      lapply(out$documents, function(x) x[1, ])
    ),
    count = reshape2::melt(
      lapply(out$documents, function(x) x[2, ])
    )[ ,1]
  )

  if (binary) {
    dtm <- Matrix::sparseMatrix(
      as.numeric(d2[ ,2]),
      d2[ ,1],
      x = rep(1, length(d2[ ,3]))
    )
  } else {
    dtm <- Matrix::sparseMatrix(
      as.numeric(d2[ ,2]),
      d2[ ,1],
      x = d2[ ,3])
  }

  dtm <- dtm[Matrix::rowSums(dtm) > 0, ]

  colnames(dtm) <- out$vocab
  rownames(dtm) <- names(out$documents)

  return(dtm)
}
