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
---

# Summary
`filesstrings` is an R package providing convenient functions for moving files, deleting directories, and a variety of string operations that facilitate manipulating file names and extracting information from strings. For example, R has no `file.move()`, just `file.copy()` and `file.rename()`, so the best way to move a file was to unintuitively rename it. `filesstrings` provides `file.move()`. The package's string operations mostly pertain to dealing with numbers contained within strings. It has a function `NiceFileNums()` for fixing file names such that their numbering is consistent with alphabetical order. For example, 'file10.txt' comes before 'file9.txt' in alphabetical order, so `NiceFileNums()` recognises this and renames them to 'file10.txt' and 'file09.txt' respectively. See documentation at  https://cran.r-project.org/package=filesstrings.

# References
[1] S. M. Bache and H. Wickham. _magrittr: A Forward-Pipe Operator for R_. R package
version 1.5. 2014. URL:
[https://CRAN.R-project.org/package=magrittr](https://CRAN.R-project.org/package=magrittr).

[2] H. Bengtsson. _matrixStats: Functions that Apply to Rows and Columns of Matrices
(and to Vectors)_. R package version 0.52.1. 2017. URL:
[https://CRAN.R-project.org/package=matrixStats](https://CRAN.R-project.org/package=matrixStats).

[3] J. Clayden, based on Onigmo and K. Takata. _ore: An R Interface to the Oniguruma
Regular Expression Library_. R package version 1.5.0. 2016. URL:
[https://CRAN.R-project.org/package=ore](https://CRAN.R-project.org/package=ore).

[4] D. Eddelbuettel and R. Francois. “Rcpp: Seamless R and C++ Integration”. In:
_Journal of Statistical Software_ 40.1 (2011), pp. 1-18. ISSN: 1548-7660. DOI:
[10.18637/jss.v040.i08](http://dx.doi.org/10.18637/jss.v040.i08). URL:
[https://www.jstatsoft.org/index.php/jss/article/view/v040i08](https://www.jstatsoft.org/index.php/jss/article/view/v040i08).

[5] M. Gagolewski and B. Tartanus. _R package stringi: Character string processing
facilities_. 2016. URL:
[http://www.gagolewski.com/software/stringi/](http://www.gagolewski.com/software/stringi/).

[6] R Core Team. _R: A Language and Environment for Statistical Computing_. R
Foundation for Statistical Computing. Vienna, Austria, 2017. URL:
[https://www.R-project.org/](https://www.R-project.org/).

[7] RStudio Team. _RStudio: Integrated Development Environment for R_. RStudio,
Inc.. Boston, MA, 2016. URL: [http://www.rstudio.com/](http://www.rstudio.com/).

[8] H. Wickham. _stringr: Simple, Consistent Wrappers for Common String Operations_.
R package version 1.2.0. 2017. URL:
[https://CRAN.R-project.org/package=stringr](https://CRAN.R-project.org/package=stringr).

[9] H. Wickham and R. Francois. _dplyr: A Grammar of Data Manipulation_. R package
version 0.5.0. 2016. URL:
[https://CRAN.R-project.org/package=dplyr](https://CRAN.R-project.org/package=dplyr).

[10] H. Wickham, R. Francois and K. Müller. _tibble: Simple Data Frames_. R package
version 1.3.0. 2017. URL:
[https://CRAN.R-project.org/package=tibble](https://CRAN.R-project.org/package=tibble).

[11] H. Wickham, J. Hester and R. Francois. _readr: Read Rectangular Text Data_. R
package version 1.1.0. 2017. URL:
[https://CRAN.R-project.org/package=readr](https://CRAN.R-project.org/package=readr).
