
## Example

``` r
library(devtools)
install_github("wilryh/impliedWords", dependencies=TRUE)

library(stm)
library(impliedWords)

processed <- textProcessor(
    input_data$text,
    data.frame(input_data),
    removestopwords=T, lowercase=T, stem=F
    )
out <- prepDocuments(
    processed$documents, processed$vocab, processed$meta
    )

dtm <- doc_to_dtm(out)

scores <- scale_text_pcs(
    meta=out$meta,
    dtm=dtm
)

document_scores <- score_documents_pcs(
  scores=scores
)

get_keywords(
  scores, 
  n_dimensions=2
)

plot_keywords(
  scores, x_dimension=1, y_dimension=2, q_cutoff=0.9
)
```
