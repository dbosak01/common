


test_that("super1: Superscript lower case letters work as expected.", {


  ret <- supsc("abcd")

  ret

  expect_equal(ret, "\U1D43\U1D47\U1D9C\U1D48")

  ret <- supsc("1234")

  ret

 # expect_equal(ret, "\U1D43\U1D47\U1D9C\U1D48")

  mystr <- "Asian\U1D43\U1D47\U1D9C\U00001D48Asian"

  mystr

  #ret <- paste0("Asian", supuni("a1b2c3d4"))

  #ret

  #expect_equal(ret, "\U1D43\U1D47\U1D9C\U1D48")

# super(
#
#   subsc(


  ret <- supsc("efghijklmnopqrstuvwxyz")

  ret


})
#
# "c:/mypath/myfile.sas7bosak"
#
# myvar <- "1.0"
#
# glue("Table {supsc('a')}", myvar = "Hello")
# gluedf( )
#
# rbind()

test_that("super1: Superscript upper case letters work as expected.", {


  ret <- supsc("ABCD")

  ret

  #expect_equal(ret, "\U1D43\U1D47\U1D9C\U1D48")


  ret <- supsc("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

  ret

  expect_equal(nchar(ret), 26)

})
