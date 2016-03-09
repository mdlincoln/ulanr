library(ulanr)

context("Validate input values")

test_that("NA or empty character in names returns an error", {
  expect_error(ulan_match(c("Rembrandt", NA)))
  expect_error(ulan_match(c("Rembrandt", "")))
  expect_error(ulan_id(c("Rembrandt", NA)))
  expect_error(ulan_id(c("Rembrandt", "")))
})

test_that("incorrect early_year and late_year pairings raise errors", {
  expect_error(ulan_match("Rembrandt", early_year = c(1500, 1700), late_year = c(1500, 1800)))
  expect_error(ulan_match("Rembrandt", late_year = c(1500, 1800)))
  expect_error(ulan_match("Rembrandt", early_year = c("a", "b")))
  expect_error(ulan_id("Rembrandt", early_year = c(1500, 1700), late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", early_year = c("a", "b")))
})

test_that("NAs in early_year or late_year are coerced", {
  expect_warning(ulan_match(c("Rembrandt", "Hendrick Hondius (I)"), early_year = c(NA, 1500), late_year = c(1700, NA)))
})

na_df <- dplyr::data_frame(
  id = NA_integer_,
  pref_name = NA_character_,
  birth_year = NA_integer_,
  death_year = NA_integer_,
  gender = NA_character_,
  nationality = NA_character_,
  score = NA_real_)

val_df <- dplyr::data_frame(
  id = integer(1),
  pref_name = character(1),
  birth_year = integer(1),
  death_year = integer(1),
  gender = character(1),
  nationality = character(1),
  score = numeric(1))

context("SPARQL data results")

test_that("no matching results returns NA", {
  expect_equivalent(lapply(ulan_match("asfjk", method = "sparql"), is.na), list("asfjk" = is.na(na_df)))
  expect_warning(ulan_match("asfjk", method = "sparql"), regexp = "asfjk")
  expect_equivalent(lapply(ulan_match(c("Rembrandt van Rijn"), method = "sparql", max_results = 1), is.na), lapply(list("Rembrandt van Rijn" = val_df), is.na))
})

context("local data results")

test_that("no matching results returns NA", {
  expect_equivalent(lapply(ulan_match("asfjk", method = "local"), is.na), list("asfjk" = is.na(na_df)))
  expect_warning(ulan_match("asfjk", method = "local"), regexp = "asfjk")
  expect_equivalent(lapply(ulan_match(c("Rembrandt van Rijn"), method = "local", max_results = 1), is.na), lapply(list("Rembrandt van Rijn" = val_df), is.na))
})

context("ulan_id wrapper")

test_that("returns integer vector", {
  expect_equivalent(ulan_id(c("Rembrandt van Rijn", "Michelangelo Buonarotti"), method = "sparql"), c(500011051, 500010654))
  expect_equivalent(is.na(ulan_id(c("Rembrandt van Rijn", "asdasdfasd"), method = "sparql")), is.na(c(500011051, NA)))
  expect_equivalent(ulan_id(c("Rembrandt van Rijn", "Michelangelo Buonarotti"), method = "local"), c(500011051, 500010654))
  expect_equivalent(is.na(ulan_id(c("Rembrandt van Rijn", "asdasdfasd"), method = "local")), is.na(c(500011051, NA)))
})
