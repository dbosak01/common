



dev <- FALSE

test_that("file1: file.find() base works as expected.", {

  res <- file.find(".", up = 0, down = 0)
  res

  expect_equal(length(res) > 0, TRUE)


  if (dev) {
    res1 <- file.find(".", "*.md", up = 0, down = 0)

    res1
    expect_equal(length(res1) > 0, TRUE)


    res2 <- file.find(".", "README.md", up = 0, down = 0)

    res2
    expect_equal(length(res2) > 0, TRUE)

    res3 <- file.find(".", "n*", up = 0, down = 0)

    res3
    expect_equal(length(res3) > 0, TRUE)


    res4 <- file.find(".", "*n*", up = 0, down = 0)

    res4
    expect_equal(length(res4) > 0, TRUE)


    res5 <- file.find(".", "*n*", up = -1, down = 0)

    res5
    expect_equal(is.null(res5), TRUE)

  }


})


test_that("file2: get_dir_up works as expected.", {

  pth <- "c:/mypath/another/andother/sourcedir"
  res <- get_dir_up(pth, 1)

  expect_equal(res, "c:/mypath/another/andother")

  res <- get_dir_up(pth, 2)

  expect_equal(res, "c:/mypath/another")


  res <- get_dir_up(pth, 3)

  expect_equal(res, "c:/mypath")


})


test_that("file3: file.find() offset works as expected.", {

  res <- file.find(".", "*.R*", up = 1, down = 2)

  res
  expect_equal(is.null(res), FALSE)
  expect_equal(length(res) > 0, TRUE)



})


test_that("file4: get_dirs_down() works as expected.", {

  if (dev) {

    ret1 <- get_dirs_down(".", 1)

    ret1

    expect_equal(length(ret1) > 1, TRUE)


    ret2 <- get_dirs_down(".", 3)

    ret2

    expect_equal(length(ret2) > 1, TRUE)

  } else {

    expect_equal(1, 1)
  }


})


test_that("file5: get_matching_dirs() works as expected.", {

  dirs <- c("c:/mypath/mydir",
            "c:/myotherpath/mydir2",
            "c:/mypath/kitten",
            "c:/myotherpath/mydir3")

  res <- get_matching_dirs(dirs,  glob2rx("kitten"))

  res

  expect_equal(length(res), 1)
  expect_equal(res[1], "c:/mypath/kitten")


  res <- get_matching_dirs(dirs, glob2rx("my*"))

  res

  expect_equal(length(res), 3)


  res <- get_matching_dirs(dirs, glob2rx("mydir"))

  res

  expect_equal(length(res), 1)


  res <- get_matching_dirs(dirs, glob2rx("my"))

  res

  expect_equal(length(res), 0)

})

test_that("file6: list_dirs() works as expect.", {

  res <- list_dirs(".", "fork")

  res

  expect_equal(length(res) > 0, FALSE)

  if (dev) {

    res <- list_dirs(".")

    res

    expect_equal(length(res) > 0, TRUE)

    res <- list_dirs(".", "tests")

    res

    expect_equal(length(res) > 0, FALSE)


    res <- list_dirs(".", glob2rx("t*"))

    res

    expect_equal(length(res) > 0, TRUE)

  }

})


test_that("file7: dir.find() works as expect.", {

  res <- dir.find()

  res
  expect_equal(length(res) > 0, TRUE)


  res <- dir.find(pattern = "fork")

  res
  expect_equal(length(res) > 0, FALSE)

  if (dev) {

    res <- dir.find(pattern = "articles")

    res
    expect_equal(length(res) > 0, TRUE)


    res <- dir.find(pattern = "packages")

    res
    expect_equal(length(res) > 0, TRUE)



    res <- dir.find(pattern = ".g*")

    res
    expect_equal(length(res) > 0, TRUE)


    res <- dir.find(pattern = "t*")

    res
    expect_equal(length(res) > 0, TRUE)

    res <- dir.find(pattern = "t*", down = 1)

    res
    expect_equal(length(res) == 1, TRUE)


    res <- dir.find(pattern = "c*", down = 3)

    res
    expect_equal(length(res) > 1, TRUE)


    res <- dir.find(pattern = "c*", down = 0)

    res
    expect_equal(length(res) > 1, TRUE)


    res <- dir.find(pattern = "c*", down = -1)

    res
    expect_equal(length(res) == 1, TRUE)

    res <- dir.find(pattern = "c*", up = 0, down = 0)

    res
    expect_equal(length(res) == 1, TRUE)

  }

})

