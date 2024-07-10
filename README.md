


## Example

### install package from github with `devtools`

``` r
library(devtools)
install_github("wilryh/impliedWords", dependencies=TRUE)
```

### load package

``` r
library(impliedWords)
```

### text pre-processing with `stm` package

``` r
library(stm)

processed <- textProcessor(
  input_data$text,
  data.frame(input_data),
  removestopwords=T, lowercase=T, stem=F
)
out <- prepDocuments(
  processed$documents, processed$vocab, processed$meta
)

dtm <- doc_to_dtm(out)
```

### analyze party likes/dislikes responses from the ANES

``` r
data(likes_dislikes_dtm) # included in impliedWords package
data(likes_dislikes_meta)

scores <- scale_text_pcs(
  dtm=likes_dislikes_dtm,
  meta=likes_dislikes_meta,
  weights="weight", # weight and holdout are columns in likes_dislikes_meta
  holdout="holdout",
  max_dimensions = 10
)
#> Loading required package: Matrix
#> Weighting..
#> Counting word co-occurrences..
#> Transforming document-term matrix..
#> Scaling..
```

### list keywords

``` r
get_keywords(
  scores,
  n_dimensions=1,
  n_words = 10
)
```

| keywords (-) | (+) keywords |
|:------------:|:------------:|
|   abortion   |    people    |
|    rights    |     rich     |
|    stance    |     poor     |
|     gun      |    class     |
|     pro      |   working    |
|    views     |     get      |
|    issues    |     help     |
| conservative |    always    |
|   marriage   |     man      |
|     gay      |    middle    |

Dimension 1 keywords

### plot word scores

``` r
plot_keywords(
  scores,
  x_dimension=1, y_dimension=2,
  q_cutoff=0.9,
  color = T
)
```

<img src="man/figures/README-likes_dislikes_plot_keywords-1.png" width="100%" />

### score documents

``` r
document_scores <- score_documents_pcs(
  scores=scores
)
```

### label dimensions with AI labels + embeddings

Label embedding scoring for implied word dimensions will be implemented
soon. Please reach out if you hope to use it sooner than later.

<figure>
<img
src="man/figures/theory_of_openended_responses_article_figure_6_top.png"
alt="Article Figure 6 (top panels)" />
<figcaption aria-hidden="true">Article Figure 6 (top
panels)</figcaption>
</figure>
