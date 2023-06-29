



Sys.path.internal <- function() {

  ppth <- NULL

  tryCatch({

    # if (utils::packageVersion("this.path")  >= "2.0.0")
    #   ppth <- this.path::sys.path()
    # else {
      ppth <- this.path::this.path()

    # }

  }, error = function(e) { ppth <- NULL})


  return(ppth)

}
