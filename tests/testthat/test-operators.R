





test_that("operators1: eq function works as expected.", {

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

test_that("operators2: strong_eq function works as expected.", {

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


test_that("operators3: equality operators are working as expected.", {


  expect_equal(NULL %eq% NULL, TRUE)
  expect_equal(NULL %eq% "fork", FALSE)
  expect_equal(1 %eq% "fork", FALSE)


  expect_equal(data.frame(A = 1,
                          stringsAsFactors = FALSE) %eq% data.frame(B = 1,
                                                                    stringsAsFactors = FALSE), FALSE)

  expect_equal(strong_eq(1 , "fork"), FALSE)


})


test_that("operators4: inequality operator is working as expected.", {


  expect_equal(NULL %ne% NULL, FALSE)
  expect_equal(NULL %ne% "fork", TRUE)
  expect_equal(1 %ne% "fork", TRUE)

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 3)
  v3 <- c(1, 2, 4)

  expect_equal(v1 %ne% v2, FALSE)
  expect_equal(v1 %ne% v3, TRUE)
  expect_equal(v1 %ne% NA, TRUE)
  expect_equal(v1 %ne% NULL, TRUE)

  d1 <- data.frame(A = 1,
             stringsAsFactors = FALSE)

  d2 <- data.frame(B = 1,
                   stringsAsFactors = FALSE)

  d3 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)


  expect_equal(d1 %ne% d2, TRUE)
  expect_equal(d1 %ne% d3, FALSE)



})

test_that("operators5: paste operator is working as expected.", {


  res <- "test" %p% "me"

  expect_equal(res, "testme")


})


test_that("operators6: %le% operator is working as expected.", {


  expect_equal(NULL %le% NULL, TRUE)
  expect_equal(NULL %le% "fork", FALSE)
  expect_equal(1 %le% "fork", FALSE)
  expect_equal(1 %le% 2, TRUE)
  expect_equal(2 %le% 1, FALSE)

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 3)
  v3 <- c(1, 2, 4)
  v4 <- c(1, 2, 2)

  expect_equal(v1 %le% v2, TRUE)
  expect_equal(v1 %le% v3, TRUE)
  expect_equal(v1 %le% v4, FALSE)
  expect_equal(v1 %le% NA, FALSE)
  expect_equal(v1 %le% NULL, FALSE)

  d1 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d2 <- data.frame(B = 1,
                   stringsAsFactors = FALSE)

  d3 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d4 <- data.frame(A = 2,
                   stringsAsFactors = FALSE)


  expect_equal(d1 %le% d2, FALSE)
  expect_equal(d1 %le% d3, TRUE)
  expect_equal(d1 %le% d4, TRUE)
  expect_equal(d4 %le% d1, FALSE)



})


test_that("operators7: %ge% operator is working as expected.", {


  expect_equal(NULL %ge% NULL, TRUE)
  expect_equal(NULL %ge% "fork", FALSE)
  expect_equal(1 %ge% "fork", FALSE)
  expect_equal(1 %ge% 2, FALSE)
  expect_equal(2 %ge% 1, TRUE)

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 3)
  v3 <- c(1, 2, 4)
  v4 <- c(1, 2, 2)

  expect_equal(v1 %ge% v2, TRUE)
  expect_equal(v1 %ge% v3, FALSE)
  expect_equal(v1 %ge% v4, TRUE)
  expect_equal(v1 %ge% NA, FALSE)
  expect_equal(v1 %ge% NULL, FALSE)

  d1 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d2 <- data.frame(B = 1,
                   stringsAsFactors = FALSE)

  d3 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d4 <- data.frame(A = 2,
                   stringsAsFactors = FALSE)


  expect_equal(d1 %ge% d2, FALSE)
  expect_equal(d1 %ge% d3, TRUE)
  expect_equal(d1 %ge% d4, FALSE)
  expect_equal(d4 %ge% d1, TRUE)



})


test_that("operators8: %lt% operator is working as expected.", {


  expect_equal(NULL %lt% NULL, FALSE)
  expect_equal(NULL %lt% "fork", FALSE)
  expect_equal(1 %lt% "fork", FALSE)
  expect_equal(1 %lt% 2, TRUE)
  expect_equal(2 %lt% 1, FALSE)

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 4)
  v3 <- c(2, 3, 4)

  expect_equal(v1 %lt% v1, FALSE)
  expect_equal(v1 %lt% v2, FALSE)
  expect_equal(v1 %lt% v3, TRUE)
  expect_equal(v1 %lt% NA, FALSE)
  expect_equal(v1 %lt% NULL, FALSE)

  d1 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d2 <- data.frame(B = 1,
                   stringsAsFactors = FALSE)

  d3 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d4 <- data.frame(A = 2,
                   stringsAsFactors = FALSE)


  expect_equal(d1 %lt% d2, FALSE)
  expect_equal(d1 %lt% d3, FALSE)
  expect_equal(d1 %lt% d4, TRUE)
  expect_equal(d4 %lt% d1, FALSE)



})


test_that("operators9: %gt% operator is working as expected.", {


  expect_equal(NULL %gt% NULL, FALSE)
  expect_equal(NULL %gt% "fork", FALSE)
  expect_equal(1 %gt% "fork", FALSE)
  expect_equal(1 %gt% 2, FALSE)
  expect_equal(2 %gt% 1, TRUE)
  expect_equal(1 %gt% 1, FALSE)

  v1 <- c(1, 2, 3)
  v2 <- c(1, 2, 3)
  v3 <- c(1, 2, 4)
  v4 <- c(2, 3, 4)

  expect_equal(v1 %gt% v2, FALSE)
  expect_equal(v1 %gt% v3, FALSE)
  expect_equal(v1 %gt% v4, FALSE)
  expect_equal(v4 %gt% v1, TRUE)
  expect_equal(v1 %gt% NA, FALSE)
  expect_equal(v1 %gt% NULL, FALSE)

  d1 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d2 <- data.frame(B = 1,
                   stringsAsFactors = FALSE)

  d3 <- data.frame(A = 1,
                   stringsAsFactors = FALSE)

  d4 <- data.frame(A = 2,
                   stringsAsFactors = FALSE)


  expect_equal(d1 %gt% d2, FALSE)
  expect_equal(d1 %gt% d3, FALSE)
  expect_equal(d1 %gt% d4, FALSE)
  expect_equal(d4 %gt% d1, TRUE)



})

