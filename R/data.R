#' ANES likes/dislikes document-term matrix
#'
#' A document-term matrix of open-ended survey responses to the question
#' "Is there anything in particular that you (like/dislike) about the
#' (Democratic/Republican) party? What is that?". As pre-processed for the paper
#' "Categorizing topics versus inferring attitudes: a theory and method for
#' analyzing open-ended survey responses."
#'
#' @format ## `likes_dislikes_dtm`
#' A sparseMatrix with 62,798 rows and 12,121 columns:
#' \describe{
#'   \item{rows}{Documents (a single open-ended response)}
#'   \item{columns}{Terms (whether a given word was used in a response)}
#' }
"likes_dislikes_dtm"

#' ANES likes/dislikes meta data
#'
#' Meta data for the open-ended survey responses to the question
#' "Is there anything in particular that you (like/dislike) about the
#' (Democratic/Republican) party? What is that?". As formatted for the paper
#' "Categorizing topics versus inferring attitudes: a theory and method for
#' analyzing open-ended survey responses."
#'
#' @format ## `likes_dislikes_meta`
#' A data frame with 62,798 rows and 5 columns:
#' \describe{
#'   \item{survey_year}{Survey year}
#'   \item{respondent_id}{Respondent ID}
#'   \item{question}{
#'   Indicates if a response explains like or dislike for the Democratic
#'   or Republican party.
#'   }
#'   \item{weight}{ANES survey weight, normalized by survey year}
#'   \item{holdout}{Whether the open-ended response was in the holdout set for the article.}
#'   ...
#' }
"likes_dislikes_meta"
