#' @rdname score_documents
#' @export
#'
#' @title
#' Score documents
#'
#' @description
#' \code{score_documents} uses word scores to assign document scores
#'
#' @param scores A list from output of \code{scale_text}. \code{meta} must be specified
#' in \code{scale_text_pcs} for this function to work.
#' @param max_dimensions An integer scalar. How many dimensions of scaled text to
#' score. Defaults to \code{max_dimensions} specified in \code{scale_text}
#'
#' @examples
#'
#' \dontrun{
#' scores <- scale_text_pcs(
#'     meta = out$meta,
#'     dtm = dtm
#'     )
#'
#' document_scores <- score_documents_pcs(
#'     scores = scores, n_dimensions = 10
#'     )
#' }
#'
#' @seealso \code{\link{scale_text_pcs}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{doc_to_dtm}}

score_documents_pcs <- function(
    scores, max_dimensions = NULL
)
{

  scores$dtm <- scores$dtm_orig[,colnames(scores$dtm_orig) %in%
                                  scores$vocab
  ]
  scores$dtm <- scores$dtm[,
                           match(
                             scores$vocab,
                             colnames(scores$dtm)
                           )]

  if (is.null(max_dimensions)) {
    max_dimensions <- scores$max_dimensions
  }

  ## score documents
  scored <- scores$dtm %*%
    as(scores$word_scores[ ,1:max_dimensions], "dgCMatrix")

  ##
  n_words <- Matrix::rowSums(scores$dtm)

  scored_norm <- Matrix::rowSums(
    scored^2
  )^(1/2)
  scored_norm[scored_norm==0] <- 1
  scored <- sweep(
    scored,
    1,
    scored_norm,
    "/"
  )

  ## convert document scores to data frame
  scored <- data.frame(as.matrix(scored))
  names(scored) <- paste0(
    "X",
    ## rename first dimension to X0
    0:(ncol(scored)-1)
  )

  ## combine meta data and document scores
  scored <- data.frame(
    scores$meta,
    n_words = unname(n_words),
    as.matrix(scored)
  )

  return(scored)
}
