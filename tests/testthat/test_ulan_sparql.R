library(ulanr)
context("SPARQL ID results")

test_that("no matching results returns NA", {
  expect_equal(ulan_id("asfjk"), c(NA))
  expect_warning(ulan_id("asfjk"))
  expect_equal(ulan_id(c("Rembrandt", NA)), c(500011051, NA))
  expect_equal(ulan_id(""), c(NA))
})

test_that("ulan_id handles a vector of names", {
  expect_equal(ulan_id(c("Rembrandt", "Hendrick Hondius (I)")), c(500011051, 500006788))
})

test_that("multiple names can be queried using one year range", {
  expect_equal(ulan_id(c("Rembrandt", "Hendrick Hondius (I)"), early_year = 1500, late_year = 1700), c(500011051, 500006788))
})

test_that("ulan_id returns correct name", {
  expect_equal(ulan_id("Rembrandt"), 500011051)
  expect_equal(ulan_id("Hendrik Hondius (I)"), 500006788)
})

test_that("ulan_id date restrictions work", {
  expect_equal(ulan_id("Rembrandt", early_year = 1600, late_year = 1670), 500011051)
  expect_equal(ulan_id("Rembrandt", early_year = 1770, late_year = 1860), 500019719)
  expect_equal(ulan_id("Rembrandt", early_year = 1880, late_year = 1930), 500006691)
})

context("SPARQL data results")

test_that("no matching results returns NA", {
  expect_equal(ulan_data("asfjk"), data.frame(
    name = c("asfjk"),
    id = c(NA),
    pref_name = c("Rembrandt van Rijn")
    birth_year = c(NA),
    death_year = c(NA),
    gender = c(NA),
    nationality = c(NA)))
  expect_warning(ulan_data("asfjk"))
  expect_equal(ulan_data(c("Rembrandt", NA)), data.frame(
    name = c("Rembrandt", NA),
    id = c(500011051, NA),
    pref_name = c("Rembrandt van Rijn", NA)
    birth_year = c(1606, NA),
    death_year = c(1669, NA),
    gender = c("male", NA),
    nationality = c("Dutch", NA)))
  expect_equal(ulan_data(""), data.frame(
    name = c("asfjk"),
    id = c(NA),
    pref_name = c(NA),
    birth_year = c(NA),
    death_year = c(NA),
    gender = c(NA),
    nationality = c(NA)))
})

test_that("ulan_data handles a vector of names", {
  expect_equal(ulan_data(c("Rembrandt", "Hendrick Hondius (I)")), data.frame(
    name = c("Rembrandt", "Hendrick Hondius (I)"),
    id = c(500011051, 500006788),
    pref_name = c("Rembrandt van Rijn", "Hondius, Hendrik, I")
    birth_year = c(1606, 1573),
    death_year = c(1669, 1650),
    gender = c("male", "male"),
    nationality = c("Dutch", "Dutch")))
})

test_that("multiple names can be queried using one year range", {
  expect_equal(ulan_data(c("Rembrandt", "Hendrick Hondius (I)"), early_year = 1500, late_year = 1700), data.frame(
    name = c("Rembrandt", "Hendrick Hondius (I)"),
    id = c(500011051, 500006788),
    pref_name = c("Rembrandt van Rijn", "Hondius, Hendrik, I"),
    birth_year = c(1606, 1573),
    death_year = c(1669, 1650),
    gender = c("male", "male"),
    nationality = c("Dutch", "Dutch")))
})

test_that("ulan_data returns correct name", {
  expect_equal(ulan_data("Rembrandt"), data.frame(
    name = c("Rembrandt"),
    id = c(500011051),
    pref_name = c("Rembrandt van Rijn")
    birth_year = c(1606),
    death_year = c(1669),
    gender = c("male"),
    nationality = c("Dutch")))
  expect_equal(ulan_data("Hendrik Hondius (I)"), data.frame(
    name = c("Hendrick Hondius (I)"),
    id = c(500006788),
    pref_name = c("Hondius, Hendrik, I")
    birth_year = c(1573),
    death_year = c(1650),
    gender = c("male"),
    nationality = c("Dutch")))
})

test_that("ulan_data date restrictions work", {
  expect_equal(ulan_data("Rembrandt", early_year = 1600, late_year = 1670), data.frame(
    name = c("Rembrandt"),
    id = c(500011051),
    pref_name = c("Rembrandt van Rijn"),
    birth_year = c(1606),
    death_year = c(1669),
    gender = c("male"),
    nationality = c("Dutch")))
  expect_equal(ulan_data("Rembrandt", early_year = 1770, late_year = 1860), data.frame(
    name = c("Rembrandt"),
    id = c(500019719),
    pref_name = c("Peale, Rembrandt"),
    birth_year = c(1778),
    death_year = c(1860),
    gender = c("male"),
    nationality = c("American")))
  expect_equal(ulan_data("Rembrandt", early_year = 1880, late_year = 1930), data.frame(
    name = c("Rembrandt"),
    id = c(500006691),
    pref_name = c("Bugatti, Rembrandt"),
    birth_year = c(1884),
    death_year = c(1916),
    gender = c("male"),
    nationality = c("Italian")))
})
