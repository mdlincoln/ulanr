library(ulanr)
context("Validate input values")

test_that("NULL or NA names return NA", {
  expect_error(ulan_match(NA))
  expect_error(ulan_match(NULL))
})

test_that("incorrect early_year and late_year pairings raise errors", {
  expect_error(ulan_match("Rembrandt", early_year = c(1500, 1700), late_year = c(1500, 1800)))
  expect_error(ulan_match("Rembrandt", late_year = c(1500, 1800)))
  expect_error(ulan_match("Rembrandt", early_year = c("a", "b")))
})

test_that("NAs in early_year or late_year are coerced", {
  expect_warning(ulan_match(c("Rembrandt", "Hendrick Hondius (I)"), early_year = c(NA, 1500), late_year = c(1700, NA)))
})

