
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


test_that("override3: Sort retains df attributes", {

  # Sample data frame to sort
  dt <- mtcars[1:10, 1:3]
  dt

  labels(dt) <- list(mpg = "Miles Per Gallon",
                     cyl = "Cylinders",
                     disp = "Displacement")


  # Sorts all columns
  dt1 <- sort.data.frame(dt, by = c("cyl"))
  dt1

  res <- labels(dt1)

  res

  expect_equal(length(res), 3)
})


test_that("override4: Sort of two factors works as expected.", {

  prtsp <- prt

  prtsp$enrollment[4] <- NA
  prtsp$sex[2] <- NA
  prtsp$sex <- factor(prtsp$sex, levels = c("girls", "boys"))
  prtsp$enrollment <- factor(prtsp$enrollment, levels = c("yes", "no"))


  res <- sort(prtsp, by = c("sex", "enrollment"))

  res

  expect_equal(unique(as.character(res$sex)), c("girls", "boys", NA))
  expect_equal(unique(as.character(res$enrollment)), c("yes", "no", NA))

})


test_that("override5: Sort descending and na.last works as expected.", {

  # Sample data frame to sort
  dt <- mtcars[1:10, 1:3]
  dt

  # Test with NA value
  dt_mod <- dt
  dt_mod[2, 3] <- NA
  dt_mod$names <- rownames(dt_mod)
  dt_mod[5, 4] <- NA

  # Sort descending and NA last
  dt12 <-  sort(dt_mod, by = c( "disp"), ascending = FALSE, na.last = TRUE)
  dt12

  expect_equal(is.na(dt12[10, 3]), TRUE)
  expect_equal(is.na(dt12[1, 3]), FALSE)


  # Sort descending and NA first
  dt13 <-  sort(dt_mod, by = c( "disp"), ascending = FALSE, na.last = FALSE)
  dt13

  expect_equal(is.na(dt13[1, 3]), TRUE)
  expect_equal(is.na(dt13[10, 3]), FALSE)

  # Multiple variables sort descending NA last
  dt14 <-  sort(dt_mod, by = c("disp", "names"), ascending = FALSE, na.last = TRUE)
  dt14

  expect_equal(is.na(dt14[10, 3]), TRUE)


  # Multiple variables sort descending NA first
  dt14 <-  sort(dt_mod, by = c("disp", "names"), ascending = FALSE, na.last = FALSE)
  dt14

  expect_equal(is.na(dt14[1, 3]), TRUE)

})

# All below are good and match SAS
test_that("override6: Multiple na.last values work as expected.", {

  # Only 1 NA per column
  df <- data.frame(
    id = c(1,2,3,4),
    a  = c(NA, 2, 2, 1),
    b  = c(1,  NA,  1, 2)
  )

  # Multiple variables sort descending NA  - OK with SAS
  res <-  sort(df, by = c("a", "b"), ascending = c(FALSE, TRUE), na.last = c(TRUE, FALSE))
  res

  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[1]), TRUE)


  # Multiple variables sort descending NA - OK with SAS
  res <-  sort(df, by = c("a", "b"), ascending = c(FALSE, FALSE), na.last = c(TRUE, TRUE))
  res

  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[2]), TRUE)


  # Multiple variables sort descending NA - OK with SAS
  res <-  sort(df, by = c("a", "b"), ascending = c(TRUE, FALSE), na.last = c(FALSE, TRUE))
  res

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$b[4]), TRUE)


  # Multiple variables sort descending NA - OK with SAS
  res <-  sort(df, by = c("a", "b"), ascending = c(TRUE, TRUE), na.last = c(FALSE, FALSE))
  res

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$b[3]), TRUE)




})

# Sorting within multiple NAs - ** Trouble here originally. Working now **
test_that("override7: Sorting with multiple NAs and parameter combinations.", {

  df <- data.frame(
    id = c(1,2,3,4),
    a  = c(NA, 2, NA, 1),
    b  = c(2,  1,  NA, 2)
  )

  # Multiple variables sort with NA - Got it
  res <-  sort(df, by = c("a", "b"), ascending = c(TRUE, TRUE), na.last = c(FALSE, FALSE))
  res

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)
  expect_equal(is.na(res$b[1]), TRUE)

  # Multiple variables sort with NA - Yes
  res <-  sort(df, by = c("a", "b"), ascending = c(FALSE, FALSE), na.last = c(TRUE, TRUE))
  res

  expect_equal(is.na(res$a[3]), TRUE)
  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[4]), TRUE)

  # Multiple variables sort with NA - This actually correct
  res <-  sort(df, by = c("a", "b"), ascending = c(FALSE, TRUE), na.last = c(TRUE, FALSE))
  res

  expect_equal(is.na(res$a[3]), TRUE)
  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[3]), TRUE)

  # Multiple variables sort with NA - This actually correct
  res <-  sort(df, by = c("a", "b"), ascending = c(TRUE, FALSE), na.last = c(FALSE, TRUE))
  res

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)
  expect_equal(is.na(res$b[2]), TRUE)


})


