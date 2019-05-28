AsNumeric <- function(arg) suppressWarnings(as.numeric(arg))
AfterLast <- function(strings, last) {
  # Chops of the leading part of the string up to and including the last instance of the charachter (or series of characters) specified in the last argument
  instancess <- gregexpr(last, strings)
  last.instances <- sapply(instancess, Last)
  chops <- mapply(substr, strings, last.instances + 1, nchar(strings))
}
AllButFirstNChar <- function(string, n) {
  nc <- nchar(string)
  if (nc < 2) stop("string must be more than 1 character")
  abfn <- substr(string, n + 1, nc)
  return(abfn)
}
CharVec <- function(string) strsplit(string, NULL)[[1]]
FirstNumber <- function(string, leave.as.string = F) {
  char.vec <- sapply(1:nchar(string), StrElem, string = string)
  first.numeric <- match(F, is.na(AsNumeric(char.vec)))
  non.numeric.start.gone <- substr(string, first.numeric, nchar(string))
  first.number <- LeadingNumber(non.numeric.start.gone, leave.as.string)
  return(first.number)
}
LastNumber <- function(string, leave.as.string = F) {
  string.reversed <- StrReverse(string)
  last.num.backwards.string <- FirstNumber(string.reversed, leave.as.string = T)
  last.num.string <- StrReverse(last.num.backwards.string)
  if (leave.as.string) {
    return(last.num.string)
  } else {
    last.num <- as.numeric(last.num.string)
    return(last.num)
  }
}
Last <- function(listorvec) {
  l <- length(listorvec)
  if (is.list(listorvec)) {
    return(listorvec[[l]])
  } else {
    return(listorvec[l])
  }
}
LeadingNumber <- function(string, leave.as.string = F) {
  if (!is.na(suppressWarnings(as.numeric(string)))) {
    if (leave.as.string) {
      return(toString(string))
    } else {
      return(as.numeric(string))
    }
  } else if (is.na(suppressWarnings(as.numeric(StrElem(string, 1))))) {
    stop("string does not begin with a numeric character")
  } else {
    char.vec <- sapply(1:nchar(string), StrElem, string = string)
    suppressWarnings(numeric.or.not <- sapply(char.vec, as.numeric))
    last.numeric <- match(NA, numeric.or.not) - 1
    number.as.string <- substr(string, 1, last.numeric)
    if (leave.as.string) {
      return(number.as.string)
    } else {
      number <- as.numeric(number.as.string)
      return(number)
    }
  }
}
MyAllEqual <- function(a, b = NA, cn = F) {
  if (is.na(b)) {
    return(length(unique(a)) == 1)
  } else {
    return(isTRUE(all.equal(a, b, check.names = cn)))
  }
}
Mat2RowList <- function(mat) {
  # convert a matrix into a list where each list element is a row of that matrix
  RowList <- split(t(mat), rep(1:ncol(t(mat)), each=nrow(t(mat))))
  return(RowList)
}
Mat2ColList <- function(mat) {
  # convert a matrix into a list where each list element is a column of that matrix
  RowList <- split(mat, rep(1:ncol(mat), each=nrow(mat)))
  return(RowList)
}
NumNameFiles <- function() {
  lf <- list.files()
  extensions <- AfterLast(lf, "\\.")
  new.names <- paste0(1:length(extensions), ".", extensions)
  if (any(new.names %in% lf)) stop("some o")
  file.rename(lf, new.names)
}
OccurrenceNumber <- function(m, vec) {
  sum(vec[1:m] == vec[m])  
  # element m is which occurrence of that value in the vector vec
  # so OccurrenceNumber(4, c(1, 3, 2, 3, 3)) = 2.
}
OccurrenceNumberAll <- function(vec) {
  sapply(1:length(vec), OccurrenceNumber, vec)
}
OrderOnAthenB <- function(a, b) {
  # a and b should be numeric
  l <- length(a)
  if (l != length(b)) stop("arguments must be of same length")
  if (any(b < 0)) b <- b - min(b)  # mak sure a is non-negative
  a.diffs <- apply(AllPairs(a), 1, function(x) abs(x[1] - x[2]))  # differences between elements of a
  min.g0.ad <- min(a.diffs[a.diffs > 0])  # the minimum greated than zero a difference
  b.tata <- b * (min.g0.ad / max(b)) / 2  # B_ToAdd_ToA. This is a scaled version of b whose max element will be max(b) * (min.g0.ad / max(b)) / 2 = min.g0.ad / 2 which is less than the minimum greater than zero a difference. This gives that a[i] < a[j] => a[i] + b.tata[i] < a[j] + b.tata[j], so adding b.tata to a preserves order of unequal elements. Also we have b[i] < b[j] => b.tata[i] < b.tata[j]; since b.tata is just a scaled version of b, order is preserved. However for a[i] = a[j], we have b[i] < b[j] => b.tata[i] < b.tata[j] => a[i] + b.tata[i] < a[j] + b.tata[j]. So we have that the addition of b.tata to a preserves the order of unequal elements and orders equal elements according to b, which is what we want to order on a then b (then we just have to take order(a + b.tata)).
  to <- a + b.tata  # ToOrder
  return(order(to))
}
StrElem <- function(string, element.index) {
  elem <- substr(string, element.index, element.index)
  return(elem)
}
StringsWithPatterns <- function(strings, patterns, ic = F) {
  for (p in patterns) strings <- strings[grepl(p, strings, ignore.case = ic)]
  return(strings)
}
StrReverse <- function(string) {
  char.vec <- CharVec(string)
  char.vec.reversed <- rev(char.vec)
  string.reversed <- paste(char.vec.reversed, collapse = "")
  return(string.reversed)
}
UpToLast <- function(strings, last) {
  # Chops off the last bit of the string which starts with the charachter (or series of characters) specified in the last argument
  instancess <- gregexpr(last, strings)
  last.instances <- sapply(instancess, Last)
  chopped.offs <- mapply(substr, strings, 1, last.instances - 1)
}
