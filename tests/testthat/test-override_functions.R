
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



test_that("override1: sort.data.frame() works as expected.", {


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



test_that("override2: labels() function works as expected.", {

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

