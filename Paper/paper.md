---
title: 'filesstrings: An R package for file and string manipulation'
tags:
  - R
  - file
  - string
authors:
  - name: Rory Nolan
    orcid: 0000-0002-5239-4043
    affiliation: 1
  - name: Sergi Padilla-Parra
    orcid: 0000-0002-8010-9481
    affiliation: 1, 2
affiliations:
  - name: Wellcome Trust Centre for Human Genetics, University of Oxford
    index: 1
  - name: Department of Structural Biology, University of Oxford
    index: 2
date: 11 April 2017
bibliography: paper.bib
nocite: | 
  @R, @RStudio, @readr, @tibble, @Rcpp, @magrittr, @ore, @dplyr, @matrixStats, @stringr, @stringi
---

# Summary
`filesstrings` is an R package providing convenient functions for moving files, deleting directories, and a variety of string operations that facilitate manipulating file names and extracting information from strings. For example, R has no `file.move()`, just `file.copy()` and `file.rename()`, so the best way to move a file was to unintuitively rename it. `filesstrings` provides `file.move()`. The package's string operations mostly pertain to dealing with numbers contained within strings. It has a function `NiceFileNums()` for fixing file names such that their numbering is consistent with alphabetical order. For example, 'file10.txt' comes before 'file9.txt' in alphabetical order, so `NiceFileNums()` recognises this and renames them to 'file10.txt' and 'file09.txt' respectively. See documentation at  https://cran.r-project.org/package=filesstrings.

# References
