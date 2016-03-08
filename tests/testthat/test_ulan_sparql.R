library(ulanr)
context("SPARQL ID results")

test_that("no matching results returns NA", {
  expect_equal(is.na(ulan_id("asfjk", method = "sparql")), is.na(NA))
  expect_warning(ulan_id("asfjk", method = "sparql"))
  expect_equal(is.na(ulan_id(c(""), method = "sparql")), is.na(NA))
})

test_that("ulan_id handles a vector of names", {
  expect_equal(ulan_id(c("Rembrandt van Rijn", "Hendrick Hondius (I)"), method = "sparql"), c(500011051, 500006788))
})

test_that("multiple names can be queried using one year range", {
  expect_equal(ulan_id(c("Rembrandt van Rijn", "Hendrick Hondius (I)"), early_year = 1500, late_year = 1700), c(500011051, 500006788), method = "sparql")
})

test_that("ulan_id returns correct name", {
  expect_equal(ulan_id("Rembrandt van Rijn", method = "sparql"), 500011051)
  expect_equal(ulan_id("Hendrik Hondius (I)", method = "sparql"), 500006788)
})

test_that("ulan_id date restrictions work", {
  expect_equal(ulan_id("Rembrandt", early_year = 1600, late_year = 1670, method = "sparql"), 500011051)
  expect_equal(ulan_id("Rembrandt", early_year = 1770, late_year = 1860, method = "sparql"), 500019719)
  expect_equal(ulan_id("Rembrandt", early_year = 1880, late_year = 1930, method = "sparql"), 500006691)
})

context("SPARQL data results")

test_that("no matching results returns NA", {
  expect_equivalent(is.na(ulan_data("asfjk", method = "sparql")), is.na(data.frame(
    name = c("asfjk"),
    id = c(NA),
    pref_name = c(NA),
    birth_year = c(NA),
    death_year = c(NA),
    gender = c(NA),
    nationality = c(NA),
    stringsAsFactors = FALSE)))
  expect_warning(ulan_data("asfjk", method = "sparql"))
  expect_equivalent(is.na(ulan_data(c("Rembrandt van Rijn", NA), method = "sparql")), is.na(data.frame(
    name = c("Rembrandt van Rijn", NA),
    id = c(500011051, NA),
    pref_name = c("Rembrandt van Rijn", NA),
    birth_year = c(1606, NA),
    death_year = c(1669, NA),
    gender = c("male", NA),
    nationality = c("Dutch", NA),
    stringsAsFactors = FALSE)))
  expect_equivalent(is.na(ulan_data("", method = "sparql")), is.na(data.frame(
    name = c(""),
    id = c(NA),
    pref_name = c(NA),
    birth_year = c(NA),
    death_year = c(NA),
    gender = c(NA),
    nationality = c(NA),
    stringsAsFactors = FALSE)))
})

test_that("ulan_data handles a vector of names", {
  expect_equivalent(ulan_data(c("Rembrandt van Rijn", "Hendrick Hondius (I)")), data.frame(
    name = c("Rembrandt van Rijn", "Hendrick Hondius (I)"),
    id = c(500011051, 500006788),
    pref_name = c("Rembrandt van Rijn", "Hondius, Hendrik, I"),
    birth_year = c(1606, 1573),
    death_year = c(1669, 1650),
    gender = c("male", "male"),
    nationality = c("Dutch", "Dutch"),
    stringsAsFactors = FALSE))
})

test_that("multiple names can be queried using one year range", {
  expect_equivalent(ulan_data(c("Rembrandt van Rijn", "Hendrick Hondius (I)"), early_year = 1500, late_year = 1700), data.frame(
    name = c("Rembrandt van Rijn", "Hendrick Hondius (I)"),
    id = c(500011051, 500006788),
    pref_name = c("Rembrandt van Rijn", "Hondius, Hendrik, I"),
    birth_year = c(1606, 1573),
    death_year = c(1669, 1650),
    gender = c("male", "male"),
    nationality = c("Dutch", "Dutch"),
    stringsAsFactors = FALSE))
})

test_that("ulan_data returns correct name", {
  expect_equivalent(ulan_data("Rembrandt van Rijn"), data.frame(
    name = c("Rembrandt van Rijn"),
    id = c(500011051),
    pref_name = c("Rembrandt van Rijn"),
    birth_year = c(1606),
    death_year = c(1669),
    gender = c("male"),
    nationality = c("Dutch"),
    stringsAsFactors = FALSE))
  expect_equivalent(ulan_data("Hendrik Hondius (I)"), data.frame(
    name = c("Hendrik Hondius (I)"),
    id = c(500006788),
    pref_name = c("Hondius, Hendrik, I"),
    birth_year = c(1573),
    death_year = c(1650),
    gender = c("male"),
    nationality = c("Dutch"),
    stringsAsFactors = FALSE))
})

test_that("ulan_data date restrictions work", {
  expect_equivalent(ulan_data("Rembrandt", early_year = 1600, late_year = 1670), data.frame(
    name = c("Rembrandt"),
    id = c(500011051),
    pref_name = c("Rembrandt van Rijn"),
    birth_year = c(1606),
    death_year = c(1669),
    gender = c("male"),
    nationality = c("Dutch"),
    stringsAsFactors = FALSE))
  expect_equivalent(ulan_data("Rembrandt", early_year = 1770, late_year = 1860), data.frame(
    name = c("Rembrandt"),
    id = c(500019719),
    pref_name = c("Peale, Rembrandt"),
    birth_year = c(1778),
    death_year = c(1860),
    gender = c("male"),
    nationality = c("American"),
    stringsAsFactors = FALSE))
  expect_equivalent(ulan_data("Rembrandt", early_year = 1880, late_year = 1930), data.frame(
    name = c("Rembrandt"),
    id = c(500006691),
    pref_name = c("Bugatti, Rembrandt"),
    birth_year = c(1884),
    death_year = c(1916),
    gender = c("male"),
    nationality = c("Italian"),
    stringsAsFactors = FALSE))
})
