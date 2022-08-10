
prt <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         no    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23')



test_that("common1: sort.data.frame works as expected.", {


  # Sample data frame to sort
  dt <- mtcars[1:10, 1:3]
  dt

  # Sorts all columns
  dt1 <- sort.data.frame(dt[ ,c("cyl", "disp")])
  dt1
  expect_equal(ncol(dt1), 2)
  expect_equal(dt1[1, 1], 4)
  expect_equal(dt1[10, 1], 8)

  # Generic works
  dt2 <-  sort(dt[ ,c("cyl", "disp")])
  dt2
  expect_equal(ncol(dt2), 2)
  expect_equal(dt2[1, 1], 4)
  expect_equal(dt2[10, 1], 8)

  # By works
  dt3 <-  sort(dt, by = c("cyl", "disp"))
  dt3

  dt4 <-  sort(dt,
               by = c("cyl", "disp"),
               ascending = c(FALSE, TRUE))
  dt4
  expect_equal(ncol(dt4), 3)
  expect_equal(dt4[1, 2], 8)
  expect_equal(dt4[10, 2], 4)

  # index.return works
  ord1 <-  sort(dt, by = c("mpg"), index.return = TRUE)
  ord1
  expect_equal(is.vector(ord1), TRUE)

  dt5 <- dt[ord1, ]
  dt5
  expect_equal(ncol(dt5), 3)
  expect_equal(dt5[1, 1], 14.3)
  expect_equal(dt5[10, 1], 24.4)

  # Single by works and returns data frame
  dt6 <-  sort(dt, by = c( "disp"))
  dt6
  expect_equal("data.frame" %in% class(dt6),  TRUE)
  expect_equal(ncol(dt6), 3)
  expect_equal(dt6[1, 3], 108)
  expect_equal(dt6[10, 3], 360)

  # Test with NA value
  dt_mod <- dt
  dt_mod[2, 3] <- NA
  dt_mod$names <- rownames(dt_mod)
  dt_mod[5, 4] <- NA


  dt7 <-  sort(dt_mod, by = c( "disp"), na.last = TRUE)
  dt7

  expect_equal(is.na(dt7[1, 3]), FALSE)
  expect_equal(is.na(dt7[10, 3]), TRUE)

  dt8 <-  sort(dt_mod, by = c( "disp"), na.last = FALSE)
  dt8


  expect_equal(is.na(dt8[1, 3]), TRUE)
  expect_equal(is.na(dt8[10, 3]), FALSE)


  dt9 <-  sort(dt_mod, by = c( "names"), na.last = TRUE)
  dt9

  expect_equal(is.na(dt9[1, 4]), FALSE)
  expect_equal(is.na(dt9[10, 4]), TRUE)


  dt10 <-  sort(dt_mod, by = c( "names"), na.last = FALSE)
  dt10


  expect_equal(is.na(dt10[1, 4]), TRUE)
  expect_equal(is.na(dt10[10, 4]), FALSE)

  dt11 <-  sort(dt_mod, by = c("disp", "names"), na.last = TRUE)
  dt11


  expect_equal(is.na(dt11[9, 4]), TRUE)
  expect_equal(is.na(dt11[10, 3]), TRUE)

  expect_error(sort(dt_mod, by = "fork"))

})


test_that("common2: v() function works", {

  res <- v(a, b, c)


  expect_equal(res, c("a", "b", "c"))

  res1 <- v( c * d, a, b)

  res1

  expect_equal(res1, c("c * d", "a", "b"))


  res2 <- v(c & d, a, b)

  res2

  expect_equal(res2, c("c & d", "a", "b"))

})

test_that("common3: roundup() function works as expected.", {

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



test_that("common4: labels() function works as expected.", {

  # Basic functioning
  df1 <- mtcars[1:10, c("mpg", "cyl") ]

  df1

  # Assign widths
  labels(df1) <- list(mpg = "Miles Per Gallon",
                      cyl = "Cylinders")


  # Extract format list
  lst <- labels.data.frame(df1)

  expect_equal(length(lst), 2)
  expect_equal(lst, list(mpg = "Miles Per Gallon", cyl = "Cylinders"))


  # Clearning labels works
  labels(df1)

  labels(df1) <- NULL
  expect_equal(length(labels(df1)), 0)


  # Parameter Check
  s <- "hello"

  expect_error(labels(s) <- list(a = "One"))

  dat <- mtcars

  names(dat) <- NULL

  # No error generated now
  labels(dat) <- list(a = "One")


})






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


test_that("common8: file.find() base works as expected.", {

  res <- file.find(".", up = 0, down = 0)
  res

  expect_equal(length(res) > 0, TRUE)


  # Comment out from here down
  # res1 <- file.find(".", "*.md", up = 0, down = 0)
  #
  # res1
  # expect_equal(length(res1) > 0, TRUE)
  #
  #
  # res2 <- file.find(".", "README.md", up = 0, down = 0)
  #
  # res2
  # expect_equal(length(res2) > 0, TRUE)
  #
  # res3 <- file.find(".", "n*", up = 0, down = 0)
  #
  # res3
  # expect_equal(length(res3) > 0, TRUE)
  #
  #
  # res4 <- file.find(".", "*n*", up = 0, down = 0)
  #
  # res4
  # expect_equal(length(res4) > 0, TRUE)
  #
  #
  # res5 <- file.find(".", "*n*", up = -1, down = 0)
  #
  # res5
  # expect_equal(is.null(res5), TRUE)




})


test_that("common9: get_dir_up works as expected.", {

  pth <- "c:/mypath/another/andother/sourcedir"
  res <- get_dir_up(pth, 1)

  expect_equal(res, "c:/mypath/another/andother")

  res <- get_dir_up(pth, 2)

  expect_equal(res, "c:/mypath/another")


  res <- get_dir_up(pth, 3)

  expect_equal(res, "c:/mypath")


})


test_that("common8: file.find() offset works as expected.", {

  res <- file.find(".", "*.R*", up = 1, down = 2)

  res
  expect_equal(is.null(res), FALSE)
  expect_equal(length(res) > 0, TRUE)



})


# test_that("common9: get_dirs_down() works as expected.", {
#
#
#   ret1 <- get_dirs_down(".", 1)
#
#   ret1
#
#   expect_equal(length(ret1) > 1, TRUE)
#
#
#   ret2 <- get_dirs_down(".", 3)
#
#   ret2
#
#   expect_equal(length(ret2) > 1, TRUE)
#
#
# })
