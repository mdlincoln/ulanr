library(ulanr)
context("stringdist results")

test_that("no matching results returns NA", {
  expect_equal(ulan_id("asfjk", method = "stringdist"), c(NA))
  expect_warning(ulan_id("asfjk"), method = "stringdist")
  expect_equal(ulan_id(c("Rembrandt", NA), method = "stringdist"), c(500011051, NA))
  expect_equal(ulan_id("", method = "stringdist"), c(NA))
})

test_that("ulan_id handles a vector of names", {
  expect_equal(ulan_id(c("Rembrandt", "Hendrick Hondius (I)"), method = "stringdist"), c(500011051, 500006788))
})

test_that("multiple names can be queried using one year range", {
  expect_equal(ulan_id(c("Rembrandt", "Hendrick Hondius (I)"), early_year = 1500, late_year = 1700, method = "stringdist"), c(500011051, 500006788))
})

test_that("ulan_id returns correct name", {
  expect_equal(ulan_id("Rembrandt", method = "stringdist"), 500011051)
  expect_equal(ulan_id("Hendrik Hondius (I)", method = "stringdist"), 500006788)
})

test_that("ulan_id date restrictions work", {
  expect_equal(ulan_id("Rembrandt", early_year = 1600, late_year = 1700, method = "stringdist"), 500011051)
  expect_equal(ulan_id("Rembrandt", early_year = 1700, late_year = 1800, method = "stringdist"), 500019719)
  expect_equal(ulan_id("Rembrandt", early_year = 1890, late_year = 2000, method = "stringdist"), 500006691)
})
