




myfunc <- function() {

  ret <- mean(mtcars$mpg)

  return(ret)
}

mm <- myfunc()

print(paste0("Program1: mean of cars is ", mm))
