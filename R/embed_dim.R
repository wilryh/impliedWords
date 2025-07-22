embed_dim <- function(
    df,
    dimension,
    embedding_cols,
    weight_col=NULL
) {
  matrix(
    apply(
      as.numeric(stdz(
        data.frame(df)[,dimension],
        if (!is.null(weight_col)) {
          data.frame(df)[,weight_col]
        } else {
          rep(1, nrow(df))
        }
      )) *
        as.matrix(df[,embedding_cols]),
      2,
      weighted.mean,
      w=if (!is.null(weight_col)) {
        data.frame(df)[,weight_col]
      } else {
        rep(1, nrow(df))
      },
      na.rm = TRUE
    ),
    nrow=1
  )
}

cos_sim <- function(
    amat, bmat
) {
  row_standardize_matrix2(
    as(amat, "dgCMatrix")
  ) %*% Matrix::t(
    row_standardize_matrix2(
      as(bmat, "dgCMatrix")
    )
  )
}

stdz <- function(x, weight = NULL) {
  # from Hmisc and weights package
  wtd.mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
    if (!length(weights))
      return(mean(x, na.rm = na.rm))
    if (na.rm) {
      s <- !is.na(x + weights)
      x <- x[s]
      weights <- weights[s]
    }
    sum(weights * x)/sum(weights)
  }
  wtd.var <- function(
    x, weights = NULL, normwt = TRUE, na.rm = TRUE,
    method = c("unbiased", "ML")
  )  {
    method <- match.arg(method)
    if (!length(weights)) {
      if (na.rm)
        x <- x[!is.na(x)]
      return(var(x))
    }
    if (na.rm) {
      s <- !is.na(x + weights)
      x <- x[s]
      weights <- weights[s]
    }
    if (normwt)
      weights <- weights * length(x)/sum(weights)
    if (normwt || method == "ML")
      return(
        as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov)
      )
    sw <- sum(weights)
    if (sw <= 1)
      warning("only one effective observation; variance estimate undefined")
    xbar <- sum(weights * x)/sw
    sum(weights * ((x - xbar)^2))/(sw - 1)
  }
  if (is.null(weight)) {
    weight <- rep(1, length(x))
  }
  x <- x - wtd.mean(x, weight, na.rm = TRUE)
  x <- x/sqrt(wtd.var(x, weight, na.rm = TRUE))
  x
}

row_standardize_matrix2 <- function(m) {
  m <- Matrix::t(m)
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
