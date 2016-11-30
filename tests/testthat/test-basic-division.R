library(testthat)
library(cconvention)
context("basic-division")

library(maptools)
data(wrld_simpl)
test_that("in-built data set is available", {
  expect_that(division(), is_a("SpatialPolygonsDataFrame"))
  expect_that(division("58.4.2", subset(wrld_simpl, NAME == "Antarctica")), is_a("SpatialPolygonsDataFrame"))
  
  expect_error(division("apoly"))
  expect_that(division(intsct = wrld_simpl), is_a("SpatialPolygonsDataFrame"))
})
