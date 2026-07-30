# textclasstutorial

`textclasstutorial` is an installable R package for learning and teaching
reproducible text classification. It modernizes the code accompanying:

> Kobayashi, V. B., Mol, S. T., Berkers, H. A., Kismihók, G., & Den Hartog,
> D. N. (2018). Text classification for organizational researchers: A
> tutorial. *Organizational Research Methods, 21*(3), 766–799.

The package turns the original sequence-dependent scripts into documented,
testable functions while retaining those scripts in `inst/legacy-code/`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("vkobayashi/textclassificationtutorial")
```

## Quick start

```r
library(textclasstutorial)

documents <- c(
  "Analyze customer data and build statistical models.",
  "Create dashboards and communicate analytical findings.",
  "Provide nursing care and support patients.",
  "Coordinate treatment with physicians and nurses."
)

labels <- c("data", "data", "care", "care")

clean <- preprocess_text(
  documents,
  stopwords = c("and", "with"),
  min_token_length = 2
)

dtm <- document_term_matrix(clean)
tf_idf(dtm)
extract_keywords(dtm, n = 2)

model <- fit_naive_bayes(dtm, labels)
predictions <- predict(model, dtm)
classification_metrics(labels, predictions, positive = "data")
```

## What the package covers

- HTML extraction from a file, string, or directory
- Lightweight sentence segmentation
- Transparent text normalization and stopword removal
- Document-term and TF-IDF matrices
- Keyword extraction and cosine similarity
- Stratified repeated cross-validation folds
- Binary classification evaluation
- Multinomial Naive Bayes without a heavy modelling dependency

Read the tutorials with:

```r
vignette("getting-started", package = "textclasstutorial")
vignette("model-evaluation", package = "textclasstutorial")
```

## Design principles

Package functions never install dependencies, change global warning settings,
open browsers, delete output directories, or depend on objects produced by a
previous script. File-writing decisions remain with the user.

The lightweight sentence splitter and classifier are deliberately transparent
for teaching. For production NLP, use language-specific tokenizers, carefully
validated preprocessing, and models appropriate to the research design.

## Development

```r
install.packages(c("devtools", "testthat", "knitr", "rmarkdown"))
devtools::document()
devtools::test()
devtools::check()
```
