# common 1.0.2

* Fixed bug on `v()` function when first item had more than one part.
* Added `supsc()` and `subsc()` functions to get UTF-8 superscript and subscript
codes from normal characters.
* Added `file.find()` function to look for files on the file system.
* Added `dir.find()` function to look for directories on the file system.
* Added `names.find()` function to look for variable names on a data frame.
* Restructured test and code files
* Improved documentation

# common 1.0.1

* Published to CRAN.

# common 0.0.9001

* Add news.
* Add readme.
* Complete documentation.
* Build pkgdown site.

# common 0.0.9000

* Initial version with following functions: 
- `labels.data.frame()`: Adds, views and removes labels on a data frame.
- `sort.data.frame()`: Sorts a data frame.
- `%p%`: An infix operator for the `paste0()` function.
- `%eq%`: An infix operator to determine equality between two objects.
- `roundup()`: A function that rounds 5 away from zero.
- `Sys.path()`: A function to get the path of the currently executing program.
- `v()`: A quoting function for non-standard evaluation.
