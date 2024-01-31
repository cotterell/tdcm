################################################################################
# data.tdcm01
################################################################################

test_that("data.tdcm01 can be loaded with no errors", {
  expect_no_error(data("data.tdcm01", package = "TDCM"))
}) # test_that

test_that("data.tdcm01$data is 1000-by-40", {
  data("data.tdcm01", package = "TDCM")
  dims <- dim(data.tdcm01$data)
  expect_equal(dims[1], 1000)
  expect_equal(dims[2], 40)
}) # test_that

test_that("data.tdcm01$qmatrix is 20-by-4", {
  data("data.tdcm01", package = "TDCM")
  dims <- dim(data.tdcm01$qmatrix)
  expect_equal(dims[1], 20)
  expect_equal(dims[2], 4)
}) # test_that

################################################################################
# data.tdcm02
################################################################################

test_that("data.tdcm02 can be loaded with no errors", {
  expect_no_error(data("data.tdcm02", package = "TDCM"))
}) # test_that

test_that("data.tdcm02$data is 2500-by-30", {
  data("data.tdcm02", package = "TDCM")
  dims <- dim(data.tdcm02$data)
  expect_equal(dims[1], 2500)
  expect_equal(dims[2], 30)
}) # test_that

test_that("data.tdcm02$qmatrix is 10-by-2", {
  data("data.tdcm02", package = "TDCM")
  dims <- dim(data.tdcm02$qmatrix)
  expect_equal(dims[1], 10)
  expect_equal(dims[2], 2)
}) # test_that

################################################################################
# data.tdcm03
################################################################################

test_that("data.tdcm03 can be loaded with no errors", {
  expect_no_error(data("data.tdcm03", package = "TDCM"))
}) # test_that

test_that("data.tdcm03$data is 1500-by-30", {
  data("data.tdcm03", package = "TDCM")
  dims <- dim(data.tdcm03$data)
  expect_equal(dims[1], 1500)
  expect_equal(dims[2], 30)
}) # test_that

test_that("data.tdcm03$qmatrix is 30-by-2", {
  data("data.tdcm03", package = "TDCM")
  dims <- dim(data.tdcm03$qmatrix)
  expect_equal(dims[1], 30)
  expect_equal(dims[2], 2)
}) # test_that

################################################################################
# data.tdcm04
################################################################################

test_that("data.tdcm04 can be loaded with no errors", {
  expect_no_error(data("data.tdcm04", package = "TDCM"))
}) # test_that

test_that("data.tdcm04$data is 1700-by-40", {
  data("data.tdcm04", package = "TDCM")
  dims <- dim(data.tdcm04$data)
  expect_equal(dims[1], 1700)
  expect_equal(dims[2], 40)
}) # test_that

test_that("data.tdcm04$qmatrix is 20-by-4", {
  data("data.tdcm04", package = "TDCM")
  dims <- dim(data.tdcm04$qmatrix)
  expect_equal(dims[1], 20)
  expect_equal(dims[2], 4)
}) # test_that

################################################################################
# data.tdcm05
################################################################################

test_that("data.tdcm05 can be loaded with no errors", {
  expect_no_error(data("data.tdcm05", package = "TDCM"))
}) # test_that

test_that("data.tdcm05$data is 750-by-20", {
  data("data.tdcm05", package = "TDCM")
  dims <- dim(data.tdcm05$data)
  expect_equal(dims[1], 750)
  expect_equal(dims[2], 20)
}) # test_that

test_that("data.tdcm05$qmatrix is 20-by-4", {
  data("data.tdcm05", package = "TDCM")
  dims <- dim(data.tdcm05$qmatrix)
  expect_equal(dims[1], 20)
  expect_equal(dims[2], 4)
}) # test_that
