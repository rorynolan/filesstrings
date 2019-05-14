## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>")

## ----load----------------------------------------------------------------
library(filesstrings)

## ---- remove_filename_spaces---------------------------------------------
file.create(c("file 1.txt", "file 2.txt"))
remove_filename_spaces(pattern = "txt$", replacement = "_")
list.files(pattern = "txt$")
file.remove(list.files(pattern = "txt$"))  # clean up

## ----nice_nums setup-----------------------------------------------------
file.names <- c("file999.tif", "file1000.tif")
sort(file.names)

## ----nice_nums-----------------------------------------------------------
nice_nums(file.names)

## ----before_last_dot-----------------------------------------------------
before_last_dot("spreadsheet_92.csv")

## ----add file extension 1------------------------------------------------
give_ext("xyz", "csv")

## ----add file extension 2------------------------------------------------
give_ext("xyz.csv", "csv")  

## ----change file extension-----------------------------------------------
give_ext("abc.csv", "txt")  # tack the new extension onto the end
give_ext("abc.csv", "txt", replace = TRUE)  # replace the current extension

