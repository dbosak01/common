


test_that("super1: Superscript lower case letters work as expected.", {


  ret <- supsc("abcd")

  ret

  expect_equal(ret, "\U1D43\U1D47\U1D9C\U1D48")



  mystr <- "Asian\U1D43\U1D47\U1D9C\U00001D48Asian"

  mystr


  ret <- supsc("abcdefghijklmnopqrstuvwxyz")

  ret

  expect_equal(nchar(ret), 26)

})


test_that("super2: Superscript upper case letters work as expected.", {


  ret <- supsc("ABCD")

  ret

  expect_equal(ret, "\U1D2C\U1D2E\U1D04\U1D30")


  ret <- supsc("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  ret

  expect_equal(nchar(ret), 26)

})

test_that("super3: Superscript other characters work as expected.", {


  ret <- supsc("1234")

  ret

  expect_equal(ret, "\U00B9\U00B2\U00B3\U2074")

  ret <- supsc("1234567890+-/ *(=)")

  ret

  expect_equal(nchar(ret), 18)

})


test_that("super4: Non-character arguments turned to character without error.", {

  ret2 <- supsc(1)

  expect_equal(TRUE, TRUE)

})


test_that("sub1: Subscript lower case letters work as expected.", {


  ret <- subsc("aehp")

  ret

  expect_equal(ret, "\U2090\U2091\U2095\U209A")



  mystr <- "Asian\U1D43\U1D47\U1D9C\U00001D48Asian"

  mystr


  ret <- subsc("abcdefghijklmnopqrstuvwxyz")

  ret

  expect_equal(nchar(ret), 26)

})


test_that("sub2: Subscript upper case letters work as expected.", {


  ret <- subsc("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  ret

  expect_equal(nchar(ret), 26)

})

test_that("sub3: Subscript other characters work as expected.", {


  ret <- subsc("1234")

  ret

  expect_equal(ret, "\U2081\U2082\U2083\U2084")

  ret <- subsc("1234567890+-/ (=)")

  ret

  expect_equal(nchar(ret), 17)

})


test_that("sub4: Non-character arguments turned to character without error.", {

  ret <- subsc(1)


  expect_equal(TRUE, TRUE)

})

