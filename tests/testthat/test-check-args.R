test_that("Allowance of NULL is handled correctly", {
  j <- function(a){check_arg(a)}
  j2 <- function(a){check_arg(a, allow_null = TRUE)}
  expect_error(j(NULL))
  expect_true(j2(NULL))
  expect_true(j(1))
  expect_true(j2(1))
})

test_that("Class is handled correctly", {
  k1 <- function(a){check_arg(a, chk_class = "tbl_df")}
  k2 <- function(a){check_arg(a, chk_class = "data.frame")}
  k3 <- function(a){check_arg(a, chk_class = "tbl")}
  k4 <- function(a){check_arg(a, chk_class = c("tbl_df", "data.frame", "tbl1"))}
  k5 <- function(a){check_arg(a, chk_class = c("tbl_df", "data.frame"))}
  k6 <- function(a){check_arg(a, chk_class = c("tbl_df", "data.frame"), chk_len = 1e6)}
  k7 <- function(a){check_arg(a, chk_len = 1e6)}
  d <- tibble::tibble(col1 = c(1, 2, 3), col2 = c(4, 5, 6))
  expect_true(k1(d))
  expect_true(k2(d))
  expect_true(k3(d))
  expect_error(k4(d))
  expect_true(k5(d))
  expect_true(k6(d))
  expect_true(k7(d))

  d1 <- matrix(1:9, nrow = 3)
  expect_error(k1(d1))
  expect_error(k2(d1))
  expect_error(k3(d1))
  d2 <- list(1, 2, 3)
  expect_error(k4(d2))
})

test_that("Dimension check is handled correctly", {
  l1 <- function(a){check_arg(a, chk_dim = c(3, 3, 3))}
  l2 <- function(a){check_arg(a, chk_dim = list(3, 3, 3))}
  l3 <- function(a){check_arg(a, chk_dim = c(3, 3))}
  l4 <- function(a){check_arg(a, chk_dim = NULL)}
  l5 <- function(a){check_arg(a, chk_dim = c(0, 0))}

  d1 <- array(1:27, dim = c(3, 3, 3))
  d2 <- matrix(1:9, nrow = 3)
  d3 <- data.frame()
  expect_true(l1(d1))
  expect_true(l2(d1))
  expect_error(l3(d1))
  expect_true(l3(d2))
  expect_true(l4(1:10))
  expect_true(l4("a"))
  expect_true(l4(list(1, 2, 3)))
  expect_true(l5(d3))
})

test_that("Length check is handled correctly", {
  m1 <- function(a){check_arg(a, chk_len = 10)}
  expect_true(m1(1:10))
  expect_true(m1(91:100))
  expect_error(m1(91:101))
  expect_error(m1(-1))
  expect_error(m1(1:9))
  expect_error(m1(1:9))
})

test_that("Containment check is handled correctly", {
  n1 <- function(a){check_arg(a, chk_is_in = 1:100)}
  expect_true(n1(1))
  expect_true(n1(50))
  expect_true(n1(100))
  expect_error(n1(-1))
  expect_error(n1(0))
  expect_error(n1(101))
  expect_true(n1(c(1, 2, 3, 4, 100)))
  expect_error(n1(c(1, 2, 3, 4, 101)))
  expect_error(n1("a"))
  expect_error(n1(c("a", "b")))
  expect_error(n1(data.frame(a = 1:10, b = 2:11, d = 3:12)))
  expect_error(n1(list(a = 1:10, b = 2:11, d = 3:12)))

  n2 <- function(a, tmp, b){
    check_arg(a, chk_is_in = 1:100)
    check_arg(tmp, chk_is_in = -10:10)
    check_arg(b, chk_is_in = 0:5)
  }
  expect_error(n2(1, -11, 1))
  expect_error(n2(1, -10, 6))
  expect_error(n2(0, -11, 6))

  n3 <- function(a){check_arg(a, chk_is_in = c(0, 1))}
  expect_true(n3(0))
  expect_true(n3(1))
  expect_true(n3(0.1))
  expect_true(n3(0.9))
  expect_error(n3(1.0001))
  expect_error(n3(-0.0001))
})
