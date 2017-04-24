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
---

# Summary

This is an R package to fill in some functionality that is not provided in either 'base', 'stringr' or 'stringi'. For example, R has no 'file.move', just 'file.copy' and 'file.rename' so the best way to move a file was to cleverly rename it. 'filesstrings' provides the easier 'MoveFiles'. Perhaps the best feature of the package is its ability to extract numbers from strings. Making further use of this functionality, it also has a convenient function for fixing file names such that their numbering is consistent with alphabetical order. For example, 'file10.txt' comes before 'file9.txt' in alphabetical order, so the 'NiceFileNums' function recognises this and renames them to 'file10.txt' and 'file09.txt' respectively. See https://github.com/rorynolan/filesstrings or https://cran.r-project.org/package=filesstrings.

# References
