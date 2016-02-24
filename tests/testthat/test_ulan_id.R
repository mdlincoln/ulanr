library(ulanr)
context("ulan_id")

test_that("NULL or NA names return NULL", {
  expect_error(ulan_id(NA))
  expect_error(ulan_id(NULL))
})

test_that("incorrect early_year and late_year pairings raise errors", {
  expect_error(ulan_id("Rembrandt", early_year = c(1500, 1700), late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", early_year = c("a", "b")))
})

test_that("NAs in early_year or late_year are coerced", {
  expect_warning(ulan_id(c("Rembrandt", "Hendrick Hondius (I)"), early_year = c(NA, 1500), late_year = c(1700, NA)))
})
