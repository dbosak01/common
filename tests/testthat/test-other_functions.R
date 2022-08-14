

test_that("other1: v() function works", {

  res <- v(a, b, c)


  expect_equal(res, c("a", "b", "c"))

  res1 <- v( c * d, a, b)

  res1

  expect_equal(res1, c("c * d", "a", "b"))


  res2 <- v(c & d, a, b)

  res2

  expect_equal(res2, c("c & d", "a", "b"))

})

test_that("other2: roundup() function works as expected.", {

  x <- seq(0.5,9.5,by=1)

  res <- roundup(x, 0)

  res



  expect_equal(res, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


  vct0 <- c(-2.5, -1.5, -.5, 1.5, 2.5)
  vct1 <- c(8.75, 8.85, 8.95, 9.05, 9.15, 9.25)

  res0 <- roundup(vct0)

  sasres0 <- c(-3, -2, -1, 2,3)
  expect_equal(all(res0 == sasres0), TRUE)

  rres0 <- round(vct0)
  expect_equal(all(res0 == rres0), FALSE)

  res1 <- roundup(vct1, 1)

  sasres1 <- c(8.8, 8.9, 9.0, 9.1, 9.2, 9.3)
  expect_equal(all(res1 == sasres1), TRUE)

  rres1 <- round(vct1)
  expect_equal(all(res1 == rres1), FALSE)

  vct3 <- c(-2.5, -1.5, NA, 1.5, 2.5)
  rres2 <- roundup(vct3)

  expect_equal(is.na(rres2[3]), TRUE)
  expect_equal(rres2[4], 2)

  expect_error(roundup("1"))
  expect_error(roundup(NULL))
})


test_that("other3: find.names() works as expected.", {

  res <- find.names(mtcars)

  res

  expect_equal(length(res), 11)

  res <- find.names(mtcars, "d*")

  res

  expect_equal(length(res), 2)

  res <- find.names(mtcars, c("c*", "d*"))

  res

  expect_equal(length(res), 4)

  res <- find.names(mtcars, c("c*", "d*"), exclude = "disp")

  res

  expect_equal(length(res), 3)


  res <- find.names(mtcars, c("c*", "d*"), exclude = "Disp")

  res

  expect_equal(length(res), 3)


  res <- find.names(mtcars, c("c*", "d*"), exclude = "Disp", ignore.case = FALSE)

  res

  expect_equal(length(res), 4)

  res <- find.names(mtcars, c("c*", "d*"),  start = 3, end = 10)

  res

  expect_equal(length(res), 2)

  nms <- names(mtcars)
  res <- find.names(nms, c("c*", "d*"),  start = 3, end = 10)

  res

  expect_equal(length(res), 2)


})


test_that("other4: Test that copy.attributes parameter checks work.", {

  lst <- list()
  df <- data.frame()

  expect_error(copy.attributes(NULL, mtcars))
  expect_error(copy.attributes(mtcars, NULL))
  expect_error(copy.attributes(df, mtcars))
  expect_error(copy.attributes(mtcars, df))
  expect_error(copy.attributes(lst, mtcars))
  expect_error(copy.attributes(mtcars, lst))

})


test_that("other5: copy.attributes works as expected.", {

  dat1 <- mtcars[1:10, c("mpg", "cyl", "disp")]
  dat2 <- mtcars[1:10, c("mpg", "cyl", "disp")]


  labels(dat1) <- list(mpg = "Fork1",
                       cyl = "Fork2")

  labels(dat2) <- list(disp = "fork3")

  attr(dat1$cyl, "format") <- "myfmt"
  attr(dat2$disp, "format") <- "myfmt2"
  attr(dat2$cyl, "description") <- "mydesc"

  res <- copy.attributes(dat1, dat2)

  attributes(res$mpg)
  attributes(res$cyl)
  attributes(res$disp)


  expect_equal(attributes(res$mpg)$label, "Fork1")
  expect_equal(attributes(res$cyl)$label, "Fork2")
  expect_equal(attributes(res$cyl)$format, "myfmt")
  expect_equal(attributes(res$cyl)$description, "mydesc")
  expect_equal(attributes(res$disp)$label, "fork3")
  expect_equal(attributes(res$disp)$format, "myfmt2")


})
