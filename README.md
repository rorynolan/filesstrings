filesstrings
================

An R package for string and file manipulation inspired by struggles with microscopy filenames.

[![Travis-CI Build Status](https://travis-ci.org/rorynolan/filesstrings.svg?branch=master)](https://travis-ci.org/rorynolan/filesstrings) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/filesstrings?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/filesstrings) [![Coverage Status](https://img.shields.io/codecov/c/github/rorynolan/filesstrings/master.svg)](https://codecov.io/github/rorynolan/filesstrings?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/filesstrings)](https://cran.r-project.org/package=filesstrings) ![RStudio CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/filesstrings)

Installation
------------

In R, enter

``` r
install.packages("filesstrings")
```

and you're done!

Files
=====

Please Read the Manual
----------------------

This package is one for which the functions are largely simple enough such that the function names well describe their purpose, so the manual is an excellent way to acquaint yourself with the package. But I made a couple of vignettes anyway.

First let's load the library:

``` r
library(filesstrings)
```

    #> Loading required package: stringr

Here are some file operations that I wished were easier in R.

Move files around
-----------------

I find it bizarre that base R has no `file.move`. To move a file, you have to cleverly rename it. Well, no more.

``` r
setwd(tempdir())
dir.create("tmp_00")
file.create("tmp000.txt")
```

    #> [1] TRUE

``` r
list.files()
```

    #> [1] "tmp_00"     "tmp000.txt"

``` r
PutFilesInDir("tmp000.txt", "tmp_00")
```

    #> tmp000.txt 
    #>       TRUE

``` r
list.files()
```

    #> [1] "tmp_00"

``` r
list.files("tmp_00")
```

    #> [1] "tmp000.txt"

``` r
unlink("tmp_00", recursive = TRUE)
```

Delete Directories
------------------

That `unlink` above with `recursive = TRUE` was a cryptic way to delete a directory right? I give you `RemoveDirs()`.

``` r
setwd(tempdir())
dir.create("tmp_00")
list.files()
```

    #> [1] "tmp_00"

``` r
RemoveDirs("tmp_00")
```

    #> tmp_00 
    #>   TRUE

``` r
list.files()
```

    #> character(0)

Remove spaces from file names
-----------------------------

Surely I don't have to convince anyone that spaces in file names are a bad idea? Let's get rid of some!

``` r
setwd(tempdir())
file.create(c("file 1.txt", "file 2.txt"))
```

    #> [1] TRUE TRUE

``` r
list.files()
```

    #> [1] "file 1.txt" "file 2.txt"

``` r
RemoveFileNameSpaces(replace.with = "_")
```

    #> [1] TRUE TRUE

``` r
list.files()
```

    #> [1] "file_1.txt" "file_2.txt"

``` r
file.remove(list.files())
```

    #> [1] TRUE TRUE

Strings
=======

Here are some string operations that I wished were easier in R.

The *n*<sup>th</sup> number in a string
---------------------------------------

I often want to get the first, last or *n*<sup>th</sup> number in a string.

``` r
request <- "I want the $35 scarf."
NthNumber(request, 1)
```

    #> [1] 35

``` r
NthNumber("20 people want the $12 scarf.", -1)  # last number
```

    #> [1] 12

``` r
GetCurrency(request)
```

    #> [1] "$"

Messed up file numbering
------------------------

The microscope I use numbers files with 3 numbers by default, i.e. `file001.tif`, `file002.tif` and so on. This is a problem when the automatic numbering passes 1000, whereby we have `file999.tif`, `file1000.tif`. What's the problem with this? Well, sometimes you need alphabetical order to reflect the true order of your files. These file numbers don't satisfy this requirement:

``` r
file.names <- c("file999.tif", "file1000.tif")
sort(file.names)
```

    #> [1] "file1000.tif" "file999.tif"

so `file1000.tif` comes before `file999.tif` in alphabetical order. We want them to be like

``` r
NiceNums(file.names)
```

    #> [1] "file0999.tif" "file1000.tif"

The function `NiceFileNums` renames all the files in an entire directory to be as we would like. It wraps `NiceNums`.

Could that be interpreted as numeric?
-------------------------------------

Sometimes we don't want to know is something *is* numeric, we want to know if it could be considered to be numeric (or could be coerced to numeric).

``` r
is.numeric(23)
```

    #> [1] TRUE

``` r
is.numeric("23")
```

    #> [1] FALSE

``` r
CanBeNumeric(23)
```

    #> [1] TRUE

``` r
CanBeNumeric("23")
```

    #> [1] TRUE

``` r
CanBeNumeric("23a")
```

    #> [1] FALSE

``` r
StrSplitByNums("23a")
```

    #> [[1]]
    #> [1] "23" "a"

``` r
CanBeNumeric(StrSplitByNums("23a")[[1]])
```

    #> [1]  TRUE FALSE

The name of a file without the extension
----------------------------------------

``` r
BeforeLastDot("spreadsheet_92.csv")
```

    #> spreadsheet_92.csv 
    #>   "spreadsheet_92"

Get the *n*<sup>th</sup> element of a string
--------------------------------------------

``` r
StrElem("abc", 2)
```

    #> [1] "b"

``` r
StrElem("abcdefz", -1)
```

    #> [1] "z"
