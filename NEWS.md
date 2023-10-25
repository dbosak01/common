# common 1.1.0

* Added `changed()` function to identify grouping boundaries. 

# common 1.0.9

* Fix for CRAN.
* Added infinity symbol "infin" keyword to `symbol()` function.
* Fixed bug on sort that was causing factors to be ignored in some circumstances.

# common 1.0.8

* Remove dependency on this.path(). 

# common 1.0.7

* Changes to prevent breakage of this.path().

# common 1.0.6

* Allow `roundup()` to accept a data.frame and round all numeric columns.

# common 1.0.5

* CRAN fixes.

# common 1.0.4

* Added `spaces()` function to create a string of blank spaces.
* Added `symbol()` function to get UTF-8 symbols.  
* Fixed `copy.attributes()` so it won't break factors.
* Added names to `v()` function output values.

# common 1.0.3

* Added `file.find()` function to look for files on the file system.
* Added `dir.find()` function to look for directories on the file system.
* Added `find.names()` function to look for variable names on a data frame.
* Added `copy.attributes()` function to copy column attributes from 
one data frame to another.
* Added `supsc()` and `subsc()` functions to get UTF-8 superscript and subscript
codes from normal characters.
* Fixed bug on `v()` function when first item had more than one part.
* Fixed bug on `sort.data.frame()` that was causing it to lose column attributes.
* Restructured test and code files.
* Improved documentation.

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
