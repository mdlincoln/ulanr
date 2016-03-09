library(ulanr)
library(dplyr, warn.conflicts = FALSE)
context("SPARQL data results")

na_df <- function(name) {
  data_frame(
    name = name,
    id = c(NA),
    pref_name = c(NA),
    birth_year = c(NA),
    death_year = c(NA),
    gender = c(NA),
    nationality = c(NA),
    score = c(NA))
}

val_df <- data_frame(
  name = character(1),
  id = integer(1),
  pref_name = character(1),
  birth_year = integer(1),
  death_year = integer(1),
  gender = character(1),
  nationality = character(1),
  score = double(1))

test_that("no matching results returns NA", {
  expect_equivalent(lapply(ulan_match("asfjk", method = "local"), is.na), list(is.na(na_df("asfjk"))))
  expect_warning(ulan_match("asfjk", method = "local"))
  expect_equivalent(lapply(ulan_match(c("Rembrandt van Rijn", NA), method = "local", max_results = 1), is.na), lapply(list(val_df, na_df(NA)), is.na))
  expect_equivalent(lapply(ulan_match("", method = "local"), is.na), lapply(list(na_df("")), is.na))
})