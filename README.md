<!-- badges: start -->

[![common version](https://www.r-pkg.org/badges/version/common)](https://cran.r-project.org/package=common)
[![common lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://cran.r-project.org/package=common)
[![common downloads](https://cranlogs.r-pkg.org/badges/common)](https://cran.r-project.org/package=common)
[![common total downloads](https://cranlogs.r-pkg.org/badges/grand-total/common)](https://cran.r-project.org/package=common)
[![R-CMD-check](https://github.com/dbosak01/common/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dbosak01/common/actions/workflows/R-CMD-check.yaml)


<!-- badges: end -->

# Introduction to **common**
<img src='man/images/common1.png' align="left" height="138" style="margin-right:10px"/>

When working in Base R, there are some situations where you stop and wonder:
"Why isn't there a function to do that?" For example:

- Why isn't there a function in Base R to sort a data frame by multiple columns?
- Why isn't there a function in Base R to get the path of the current program?
- In Base R, why is there no infix operator for concatenation?
- Why isn't there a function in Base R to add/modify data frame labels?

The answer to all of the above questions is that there should be.
The purpose of the **common** package is to encapsulate these types of functions,
and provide them to R users in a lightweight package.  These functions
are particularly useful for package developers, who might want to add
these capabilities to their package without creating dependencies on 
**tidyverse**. 

If you have ideas for more **common** functions, please submit
your suggestion to the github 
[issue list](https://github.com/dbosak01/common/issues).


### Installation

The easiest way to install the **common** package is to run the following 
command from your R console:

    install.packages("common")


Then put the following line at the top of your script:

    library(common)
    
For examples and usage 
information, please visit the **common** documentation site 
at common.r-sassy.org/articles/common.html

### Getting Help

If you need help, the first place 
to turn to is the  web site at common.r-sassy.org

If you want to look at the code for the **common** package, visit the
github page [here](https://github.com/dbosak01/common).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/common/issues).



