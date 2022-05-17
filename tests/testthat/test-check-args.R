test_that("Allowance of NULL is handled correctly", {
  j <- function(a){
    check_arg(a)
  }

  j2 <- function(a){
    check_arg(a, allow_null = TRUE)
  }
  expect_error(j(NULL))
  expect_true(j2(NULL))
  expect_true(j(1))
  expect_true(j2(1))
})

test_that("Class is handled correctly", {
  j3 <- function(a){
    check_arg(a, chk_class = "tbl_df")
  }
  j4 <- function(a){
    check_arg(a, chk_class = "data.frame")
  }
  j5 <- function(a){
    check_arg(a, chk_class = "tbl")
  }
  j6 <- function(a){
    check_arg(a, chk_class = c("tbl_df", "data.frame", "tbl1"))
  }
  j7 <- function(a){
    check_arg(a, chk_class = c("tbl_df", "data.frame"))
  }
  d <- tibble::tibble(col1 = c(1, 2, 3), col2 = c(4, 5, 6))
  expect_true(j3(d))
  expect_true(j4(d))
  expect_true(j5(d))
  expect_error(j6(d))
  expect_true(j7(d))
  d1 <- matrix(1:9, nrow = 3)
  expect_error(j3(d1))
  expect_error(j4(d1))
  expect_error(j5(d1))
  d2 <- list(1, 2, 3)
  expect_error(j5(d2))
})

test_that("Dimension is handled correctly", {
  j6 <- function(a){
    check_arg(a, chk_dim = c(3, 3, 3))
  }
  j7 <- function(a){
    check_arg(a, chk_dim = list(3, 3, 3))
  }
  j8 <- function(a){
    check_arg(a, chk_dim = c(3, 3))
  }
  d <- array(1:27, dim = c(3, 3, 3))
  expect_true(j6(d))
  expect_true(j7(d))
  expect_error(j8(d))
})

test_that("Length is handled correctly", {
  j9 <- function(a){
    check_arg(a, chk_len = 10)
  }
  expect_true(j9(1:10))
  expect_true(j9(91:100))
  expect_error(j9(91:101))
  expect_error(j9(-1))
  expect_error(j9(1:9))
  expect_error(j9(1:9))
})

test_that("Contains is handled correctly", {
  j10 <- function(a){
    check_arg(a, chk_is_in = 1:100)
  }
  expect_true(j10(1))
  expect_true(j10(50))
  expect_true(j10(100))
  expect_error(j10(-1))
  expect_error(j10(0))
  expect_error(j10(101))
  expect_true(j10(c(1, 2, 3, 4, 100)))
  expect_error(j10(c(1, 2, 3, 4, 101)))
  expect_error(j10("a"))
  expect_error(j10(c("a", "b")))
  expect_error(j10(data.frame(a = 1:10, b = 2:11, d = 3:12)))
  expect_error(j10(list(a = 1:10, b = 2:11, d = 3:12)))

  j11 <- function(a, tmp, b){
    check_arg(a, chk_is_in = 1:100)
    check_arg(tmp, chk_is_in = -10:10)
    check_arg(b, chk_is_in = 0:5)
  }
  expect_error(j11(1, -11, 1))
  expect_error(j11(1, -10, 6))
  expect_error(j11(0, -11, 6))

  j12 <- function(a){
    check_arg(a, chk_is_in = c(0, 1))
  }
})
