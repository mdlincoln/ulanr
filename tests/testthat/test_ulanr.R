library(ulanr)
context("SPARQL results")

test_that("no matching results returns NULL", {
  expect_null(ulan_id("asfjk"))
  expect_null(ulan_id(NA))
  expect_null(ulan_id(NULL))
  expect_null(ulan_id(""))
})

test_that("ulan_id returns correct name", {
  expect_equal(ulan_id("Rembrandt"), 500011051)
  expect_equal(ulan_id("Hendrik Hondius"), 500006788)
})

test_that("ulan_id date restrictions work", {
  expect_equal(ulan_id("Rembrandt", years = c(1600, 1700)), 500011051)
  expect_equal(ulan_id("Rembrandt", years = c(1700, 1800)), 500019719)
  expect_equal(ulan_id("Rembrandt", years = c(1890, 2000)), 500006691)
})

