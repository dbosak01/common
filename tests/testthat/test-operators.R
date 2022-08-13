





test_that("common5: eq function works as expected.", {

  expect_equal(mtcars %eq% mtcars, TRUE)
  expect_equal(mtcars %eq% iris, FALSE)
  expect_equal(mtcars %eq% mtcars[1:10, ], FALSE)
  expect_equal(mtcars %eq% mtcars[, 1:5], FALSE)
  d1 <- mtcars
  d1[1, 1] <- 2
  expect_equal(mtcars  %eq%  d1, FALSE)

  v1 <- mtcars[[1]]
  v2 <- mtcars[[1]]

  expect_equal(v1 %eq% v2, TRUE)
  expect_equal(v1 %eq% mtcars[[2]], FALSE)
  expect_equal(v1 %eq% v2[1:5], FALSE)
  v2[5] <-2
  expect_equal(v1  %eq% v2, FALSE)

  v1 <- c(1, 2, NA, NA)
  v2 <- c(NA, NA, 1, NA, 3)
  expect_equal(v1 %eq% v2, FALSE)

})

test_that("common6: strong_eq function works as expected.", {

  v1 <- c(1, 2, 3, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), TRUE)

  v1 <- c(1, 2, 2, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), FALSE)

  v1 <- c(1, NA, 3, 4)
  v2 <- c(1, 2, 3, 4)
  expect_equal(all(strong_eq(v1, v2)), FALSE)

  v1 <- c(NA, NA, NA, NA)
  v2 <- c(NA, NA, NA, NA)
  expect_equal(all(strong_eq(v1, v2)), TRUE)

  v1 <- c(NA, NA, NA, NA)
  v2 <- c(NA, NA, 1, NA)
  expect_equal(all(strong_eq(v1, v2)), FALSE)


})


test_that("common7: equality operators are working as expected.", {


  expect_equal(NULL %eq% NULL, TRUE)
  expect_equal(NULL %eq% "fork", FALSE)
  expect_equal(1 %eq% "fork", FALSE)


  expect_equal(data.frame(A = 1,
                          stringsAsFactors = FALSE) %eq% data.frame(B = 1,
                                                                    stringsAsFactors = FALSE), FALSE)

  expect_equal(strong_eq(1 , "fork"), FALSE)


})
