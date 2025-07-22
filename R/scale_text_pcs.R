#' @rdname scale_text_pcs
#' @export
#'
#' @title
#' Scale text using implied words scaling
#'
#' @description
#' \code{scale_text_pcs} runs implied words scaling
#'
#' @param dtm A sparseMatrix. Rows are documents and columns are vocabulary.
#' @param meta data.frame. Must line up with \code{dtm} etc. This is included only to
#' keep track of any accompanying variables. It is unaltered by the function.
#' @param cutoff_exponent An integer scalar. Used to determine the number of
#' 'common words'. Defaults to 2 (as in paper).
#' @param dtm_vocab A character vector. Provide vocabulary for columns of \code{dtm} if
#' missing in column names.
#' @param verbose A logical scalar. Print progress of the function.
#' @param holdout A logical vector, numeric vector, or character scalar. A
#' logical or numeric vector indicates which rows to exclude from training. A
#' character scalar provides the name of a column in \code{meta}.
#' @param max_dimensions An integer scalar. The number of principal components
#' to calculate.
#' @param weights A numeric vector or character scalar. Responses weights.
#' If a numeric vector is provided, it must be equal to the number of rows in
#' \code{dtm}. A character scalar provides the name of a column in \code{meta}.
#' @param project_rare_words A logical scalar. Whether to project rare words not (fully)
#' used in the scaling process.
#' @param embed_dimensions A logical scalar. Whether to embed implied words output.
#' @param embeddings A character scalar. The name of the nested embeddings column in meta.
#' Required if embed_dimensions = TRUE.
#' @param max_embed_dimensions An integer scalar. The number of dimensions to embed.
#' Must be less than or equal to max_dimensions - 1.
#' @param label_embeddings A matrix. The rows of the matrix are labels (with rownames
#' for labels) and the columns are embeddings.
#'
scale_text_pcs <- function(
    dtm,
    meta = NULL,
    dtm_vocab = NULL,
    cutoff_exponent = 2,
    verbose = TRUE,
    holdout = NULL,
    max_dimensions = 4,
    weights = NULL,
    project_rare_words = TRUE,
    embed_dimensions = FALSE,
    embeddings = NULL,
    max_embed_dimensions = NULL,
    label_embeddings = NULL
)
{

  row_standardize_matrix <- function(m) {
    m <- Matrix::t(m)
    ##
    m@x <- sqrt(m@x)
    ##
    row_norm <- sqrt(Matrix::colSums(m^2))
    row_norm[row_norm==0] <- 1
    ##
    m@x <- m@x /
      rep.int(row_norm, diff(m@p))
    ##
    m <- Matrix::t(m)
    return(m)
  }

  weight_matrix <- function(m, w) {
    m <- Matrix::t(m)
    ##
    if (any(is.na(w))) {
      warning("Missing weights converted to 0's.")
      w[is.na(w)] <- 0
    }
    ## w <- w / sum(w)
    m@x <- m@x *
      rep.int(sqrt(w), diff(m@p))
    ##
    m <- Matrix::t(m)
    return(m)
  }


  ## check vocab -------------------------------------------------------------
  if (!inherits(dtm, "generalMatrix")) {
    dtm <- as(dtm, "generalMatrix")
  }

  if (!is.null(dtm_vocab)) {
    colnames(dtm) <- dtm_vocab
  }

  if (is.null(colnames(dtm))) {
    stop("\nPlease supply vocabulary of term-document matrix\n")
  }

  dtm_orig <- dtm

  if (!is.null(weights)) {
    if (inherits(weights, "character") & length(weights)==1 & !is.null(meta)) {
      weights <- meta[[weights]]
    }
  }

  if (!is.null(holdout)) {
    if (inherits(holdout, "character") & length(holdout)==1 & !is.null(meta)) {
      holdout <- meta[[holdout]]
    }
    if (!inherits(holdout, "logical")) {
      holdout <- as.numeric(as.character(holdout))
      holdout <- 1:nrow(dtm) %in% holdout
    }
    dtm <- dtm[!holdout,Matrix::colSums(dtm[!holdout,])>0]
    weights <- weights[!holdout]
  }

  if (is.null(meta)) {
    meta <- NA
  }


  if (!is.null(weights)) {
    if (verbose) cat("Weighting..\n")
    if (length(weights) != nrow(dtm)) {
      stop("\nPlease provide weights of length equal to nrow(dtm).")
    }
    dtm <- weight_matrix(dtm, weights / sum(weights, na.rm=T))
  }

  word_counts <- Matrix::colSums(dtm)

  n_common_words <- sum(
    word_counts^2 > mean(word_counts^cutoff_exponent)
  )
  pivots <- word_counts > sort(word_counts, T)[n_common_words]


  if (verbose) cat("Counting word co-occurrences..\n")

  cooccur <- Matrix::crossprod(dtm)

  standardized_cooccur <- as(cooccur, "generalMatrix")
  ##
  standardized_cooccur <- row_standardize_matrix(standardized_cooccur)


  if (verbose) cat("Transforming document-term matrix..\n")

  dtm_alt <- row_standardize_matrix(
    dtm
  ) %*%
    Matrix::t(standardized_cooccur[pivots,])

  dtm_alt_orig <- row_standardize_matrix(
    dtm_orig[,colnames(dtm)]
  ) %*%
    Matrix::t(standardized_cooccur[pivots,])

  dtm_alt_std <- row_standardize_matrix(
    dtm_alt
  )

  if (!is.null(weights)) {
    dtm_alt_std <- weight_matrix(dtm_alt_std, weights / sum(weights, na.rm=T))
  }

  if (verbose) cat("Scaling..\n")

  pcs <- RSpectra::svds(
    dtm_alt_std,
    k=max_dimensions,
    nu=0
  )

  if (project_rare_words) {
    word_scores <- standardized_cooccur[,pivots] %*% pcs$v
  } else {
    word_scores <- pcs$v
    rownames(word_scores) <- rownames(standardized_cooccur)[pivots]
  }
  colnames(word_scores) <- paste0("X", 0:(ncol(word_scores)-1))
  word_scores <- as.matrix(word_scores)

  scores <- list(
    dtm = dtm,
    dtm_orig = dtm_orig,
    holdout = holdout,
    vocab = rownames(word_scores),
    meta = meta,
    word_scores = word_scores,
    word_counts = if (project_rare_words) {word_counts} else {word_counts[pivots]},
    pivots = pivots,
    full_word_counts = word_counts,
    n_common_words = n_common_words,
    max_dimensions = max_dimensions,
    embeddings_column = embeddings
  )



    if (embed_dimensions) {
      if (verbose) cat("Embedding..\n")
      if (is.null(embeddings)) stop("Provide name of nested embeddings column.")
      if (is.null(meta[,embeddings])) stop("Provide nested embeddings column in meta.")
    if (is.null(label_embeddings)) stop("Provide label embedding matrix.")
    if (is.null(rownames(label_embeddings))) stop("Add labels as rownames to label_embeddings matrix.")
    if (is.null(max_embed_dimensions)) {
      max_embed_dimensions <- max_dimensions - 1
    } else {
      if (max_embed_dimensions < max_dimensions) stop("Provide max_embed_dimensions <= max_dimensions - 1.")
    }

    embedding_scores <- matrix(nrow=max_embed_dimensions, ncol=ncol(meta[,embeddings]))
    embedding_labels <- list()
    for (i in 1:max_embed_dimensions) {
      meta_scored <- score_documents_pcs(
        scores=scores
      )
      embedding_scores[i,] <- embed_dim(
        df = meta_scored,
        dimension = paste0("X",i),
        embedding_cols = embeddings,
        weight_col = "weight"
      )

      embedding_labels[[i]] <- t(as.matrix(cos_sim(
        embedding_scores[i,,drop=F],
        label_embeddings
      )))
      rownames(embedding_labels[[i]]) <- rownames(label_embeddings)
      embedding_labels[[i]] <- embedding_labels[[i]] |>
        data.frame() |>
        rownames_to_column() |>
        setNames(c("label","value"))
    }

    scores[["embedding_scores"]] <- embedding_scores
    scores[["embedding_labels"]] <- embedding_labels

  }

  return(
    scores
  )
}
