library(ulanr)
context("SPARQL results")

test_that("no matching results returns NA", {
  expect_equal(ulan_id("asfjk"), c(NA))
  expect_warning(ulan_id("asfjk"))
  expect_equal(ulan_id(c("Rembrandt", NA)), c(500011051, NA))
  expect_equal(ulan_id(""), c(NA))
})

test_that("NULL or NA names return NULL", {
  expect_error(ulan_id(NA))
  expect_error(ulan_id(NULL))
})

test_that("ulan_id handles a vector of names", {
  expect_equal(ulan_id(c("Rembrandt", "Hendrik Hondius")), c(500011051, 500006788))
})

test_that("ulan_id returns correct name", {
  expect_equal(ulan_id("Rembrandt"), 500011051)
  expect_equal(ulan_id("Hendrik Hondius"), 500006788)
})

test_that("incorrect early_year and late_year pairings raise errors", {
  expect_error(ulan_id("Rembrandt", early_year = c(1500, 1700), late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", late_year = c(1500, 1800)))
  expect_error(ulan_id("Rembrandt", early_year = c("a", "b")))
})

test_that("ulan_id date restrictions work", {
  expect_equal(ulan_id("Rembrandt", early_year = 1600, late_year = 1700), 500011051)
  expect_equal(ulan_id("Rembrandt", early_year = 1700, late_year = 1800), 500019719)
  expect_equal(ulan_id("Rembrandt", early_year = 1890, late_year = 2000), 500006691)
})

