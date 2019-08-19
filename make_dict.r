# generate dictionary information (codebook) from haven labelled SPSS datasets
# this works well for an individual SPSS data frame, or map()'d over a list of data frames, and is much easier to use than looking at str() or attributes() output
make_dict <- function(data) {
  require(tibble)
  require(dplyr)
  require(purrr)
  require(haven)

  # other control features?
  stopifnot(
    any(map(data, is.labelled)),
    is.data.frame(data)
    )

  tibble(
    variable_name = colnames(data),
    variable_label = map(data, attributes) %>%
      map(pluck, "label") %>%
      as.character(),
    variable_format = map(data, attributes) %>%
      map(pluck, "format.spss") %>%
      as.character(),
    value_labels = map(data, attributes) %>%
      map(pluck, "labels") %>%
      as.character()
  )
}
