require(stringr)

ArgName <- function(x) deparse(substitute(x))
AsNumeric <- function(arg) suppressWarnings(as.numeric(arg))
CanBeNumeric <- function(string) !is.na(AsNumeric(string))
CharVec <- function(string) strsplit(string, NULL)[[1]]
DirCreateIfNotThere <- function(dir.name) if (!file.exists(dir.name)) dir.create(dir.name)
DirRemove <- function(dir) unlink(dir, recursive = T)
DirMove <- function(dir, destination) {
  file.copy(dir, destination, recursive = T, overwrite = T)
  DirRemove(dir)
}
DirsMove <- function(dirs, destinations) {
  if(length(destinations) == length(dirs)) {
    mapply(DirMove, dirs, destinations)
  } else if (length(destinations == 1)) {
    sapply(dirs, DirMove, destinations)
  } else {
    stop("the number of destinations must be equal to 1 or equal to the number of directories to be moved")
  }
}
ExtractMatFrom3DArray <- function(array3D, slice) array3D[, , slice]
FirstNumber <- function(string, leave.as.string = F) {
  char.vec <- sapply(1:nchar(string), StrElem, string = string)
  first.numeric <- match(F, is.na(AsNumeric(char.vec)))
  non.numeric.start.gone <- substr(string, first.numeric, nchar(string))
  first.number <- LeadingNumber(non.numeric.start.gone, leave.as.string)
  return(first.number)
}
GoToDirAndGetImList <- function(dir.fromhere, pat = "\\.tif", z = 0) {
  initial.dir <- getwd()
  setwd(goto.dir)
  im.list <- ImMatListFromNamePattern(pat, z)
  names(rim.list) <- sapply(names(rim.list), RemoveDotTif)
  setwd(initial.dir)
  return(rim.list)
}
Grouper <- function(vai, max.gap = 1){  # VectorofAscendingIndices
  # groups together bright rows of the matrix which represent the same "line"
  lv <- length(vai)
  if (lv == 0) stop("no lines of desired length found")
  if (lv == 1) {
    return(list(vai))
  } else {
    gaps <- vai[2:lv] - vai[1:(lv-1)]
    big.gaps <- gaps > max.gap
    nbgaps <- sum(big.gaps)  # number of big (>10) gaps
    if (!nbgaps) {
      return(list(vai))
    } else {
      ends <- which(big.gaps)  # vertical end of lines
      group1 <- vai[1:ends[1]]
      lg <- list(group1)
      if (nbgaps == 1){
        lg[[2]] <- vai[(ends[1] + 1):lv]
      } else {
        for (i in 2:nbgaps){
          lg[[i]] <- vai[(ends[i - 1] + 1):ends[i]]
          ikeep <- i
        }
        lg[[ikeep + 1]] <- vai[(ends[nbgaps] + 1):lv]
      }
      return(lg)
    }
  }
}
ImMatListFromNamePattern <- function(name.patterns, z = 0, first.last = 0) {
  # Read in a list of image matrices as in RIM where only images with names containing the strings specified in the string vector name.patterns are read into the list
  name.matches <- StringsWithPatterns(list.files(), name.patterns)
  if (first.last != 0) {
    if (!first.last %in% c("first", "f", "last", "l")) {
      stop('if first.last is set, it must be either "first", "f", "last" or "l"')
    } else {
      if (first.last %in% c("first", "f")) nums <- sapply(name.matches, FirstNumber)
      if (first.last %in% c("last", "l")) nums <- sapply(name.matches, LastNumber)
      name.matches <- name.matches[order(nums)]
    }
  }
  img.mat.list <- lapply(name.matches, RIM, z, fix4to3)
  names(img.mat.list) <- name.matches
  return(img.mat.list)
}
ImStack2GrayscaleList <- function(img.stack) {
  z.slices <- 1:(dim(img.stack)[3])
  gray.list <- lapply(z.slices, ExtractMatFrom3DArray, array3D = img.stack)
}
Interleave <- function(vec1, vec2) {
  l1 <- length(vec1)
  l2 <- length(vec2)
  if (! (l1 - l2) %in% 0:1) stop("vec1 must be either the same length as vec2 or one longer than it")
  ans <- 1:(l1 + l2)
  ans[seq(1, 2 * l1 - 1, 2)] <- vec1
  ans[seq(2, 2 * l2, 2)] <- vec2
  return(ans)
}
ListDirs <- function(pattern = NA, ord = F) {
  dirs <- setdiff(list.files(), list.files(pattern = "\\."))
  if (ord) {
    fns <- sapply(dirs, FirstNumber)
    lns <- sapply(dirs, LastNumber)
    ordr <- OrderOnAThenB(fns, lns)
    ordered.dirs <- dirs[ordr]
    dirs <- ordered.dirs
  }
  if (!is.na(pattern)) {
    dirs <- dirs[grepl(pattern, dirs)]
  }
  return(dirs)
}
LFNO <- function(pattern = -1) {  # List_Files_Number_Order
  if (pattern == -1) {
    lf <- list.files()
  } else {
    lf <- list.files(pattern = pattern)
  }
  ord <- order(sapply(lf, FirstNumber))
  return(lf[ord])
}
LastNChars <- function(string, n) {
  char.vec <- CharVec(string)
  l <- length(char.vec)
  start <- l - n + 1
  char.vec <- char.vec[start:l]
  last.n <- paste(char.vec, collapse = "")
  return(last.n)
}
LastNumber <- function(string, leave.as.string = F) {
  # this function doesn't work for strings with decimal numbers
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
LeadingNumber <- function(string, leave.as.string = F) {
  # this function doesn't work for strings with decimal numbers
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
MergeCSVs <- function(out.name = "Merged_CSVs", header = T, saf = F) {
  ext <- ".csv"
  lcsvs <- list.files(pattern = ext)
  if (LastNChars(out.name, 4) != ext) out.name <- paste0(out.name, ext)
  tables <- lapply(lcsvs, ReadCSV, header = header, saf = saf)
  ncs <- sapply(tables, ncol)
  if (!MyAllEqual(ncs)) stop("The csvs have different numbers of columns.")
  merged <- Reduce(rbind, tables)
  write.csv(merged, file = out.name)
}
MyAllEqual <- function(a, b = NA, cn = F) {
  if (is.na(b[1])) {
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
NiceFileNums <- function(patt = NA) {
  if (is.na(patt)) {
    lf <- list.files()
  } else {
    lf <- list.files(pattern = patt)
  }
  file.nums <- lapply(lf, NumsSepByStrings)
  if (!MyAllEqual(sapply(file.nums, length))) stop("some of the file names contain different numbers of numbers")
  file.nums <- simplify2array(file.nums)
  if (is.vector(file.nums)) file.nums <- t(as.matrix(file.nums))
  max.lengths <- apply(file.nums, 1, function(x) max(nchar(x)))
  for (i in 1:nrow(file.nums)) {
    for (j in 1:ncol(file.nums)) {
      while(nchar(file.nums[i, j]) < max.lengths[i]) {
        file.nums[i, j] <- paste0(0, file.nums[i, j])
      }
    }
  }
  num.first <- sapply(lf, function(x) CanBeNumeric(StrElem(x, 1)))
  if (!MyAllEqual(num.first)) stop("some file names start with numbers and some don't")
  numbers <- Mat2ColList(file.nums)
  strings <- lapply(lf, StringsSepByNums)
  if (num.first[1]) {
    interleaves <- mapply(Interleave, numbers, strings)
  } else {
    interleaves <- mapply(Interleave, strings, numbers)
  }
  new.names <- apply(interleaves, 2, function(x) paste(x, collapse = ""))
  file.rename(lf, new.names)
}
NthNumber <- function(string, n) {
  # this function doesn't work for strings with decimal numbers
  for (i in 1:(n - 1)) {
    fn <- FirstNumber(string, T)
    string <- str_replace(string, fn, "")
  }
  return(FirstNumber(string))
}
NumsSepByStrings <- function(string) {
  # this function doesn't work for strings with decimal numbers
  numeric.chars <- which(CanBeNumeric(CharVec(string)))
  numeric.groups <- Grouper(numeric.chars)
  numerics <- sapply(numeric.groups, StrElemsPasted, string = string)  # these will not be of numeric type, just of the type that will come out of as.numeric as numbers and not NAs
  return(numerics)
}
PutFilesInDir <- function(file.names, dir.name) {
  DirCreateIfNotThere(dir.name)
  there <- paste0(getwd(), "/", dir.name)
  new.names <- str_replace_all(paste0(there, "/", file.names), " ", "")
  file.rename(file.names, new.names)
}
ReadCSV <- function(file, header = T, saf = F) {
  if (LastNChars(file, 4) != ".csv") file <- paste0(file, ".csv")
  x <- CharVec(readLines(file, 1))[1:3]
  if (all(x == c("\"", "\"", ","))) {
    return(read.csv(file, header = header, row.names = 1, stringsAsFactors = saf))
  } else {
    return(read.csv(file, header = header, stringsAsFactors = saf))
  }
}
RemoveSpacesOfFilesInDir <- function(patt = "", replace.with = "") {
  lf <- list.files(pattern = patt)
  new.names <- str_replace_all(lf, " ", replace.with)
  file.rename(lf, new.names)
}
RemoveAdjacentSameElements <- function(vec) {
  # given a vector, wherever it happens that element i+1 is equal to element i, remove element i+1 and do this recursively until no two adjacent elements are the same (so c(3, 4, 4, 5, 8, 8, 8, 3) becomes c(3, 4, 5, 8, 3))
  l <- length(vec)
  vec1 <- vec[1:(l - 1)]
  vec2 <- vec[2:l]
  to.remove <- which(vec1 == vec2) + 1
  vec <- vec[-to.remove]
  return(vec)
}
RenameWithNums <- function(file.type, dir = ".") {
  init.dir <- getwd()
  setwd(dir)
  ext <- ifelse(CharVec(file.type)[1] == ".", file.type, paste0(".", file.type))
  lf <- list.files(pattern = ext)
  l <- length(lf)
  new.names <- paste0(1:l, ext)
  if (any(new.names %in% lf)) stop("Some of the names are already in the desired format, unable to proceed as renaming may result in deletion.")
  file.rename(lf, new.names)
  NiceFileNums(ext)
  setwd(init.dir)
}
RIM <- function(file.name, z = 0) {  # ReadImageMatrix
  # Reads the image matrix in as I like it, i.e. a transposed version of the default. The default reads in the matrix where matrix entry [i, j] refers to the pixel x=1, y=j but for a traditional matrix the idea is the other way around namely [i, j] refers to the entry in row i, column j. This is how I want it to be, because I want to treat it how I treat every other matrix.
  image.data <- imageData(suppressWarnings(readImage(file.name)))
  dims.initial <- dim(image.data)
  if (z == "all" | z == "a") {
    stack.as.list <- ImgStack2GrayscaleList(image.data)
    transpositions <- lapply(stack.as.list, t)
    back.to.stack <- do.call(abind, c(transpositions, along=3))
    return(back.to.stack)
  } else if (z) {
    return(t(image.data[, , z]))
  } else {
    return(t(image.data))
  }
}
StrElem <- function(string, element.index) substr(string, element.index, element.index)
StrElemsPasted <- function(string, elem.indices) {
  elems <- sapply(elem.indices, StrElem, string = string)
  pasted <- paste(elems, collapse = "")
  return(pasted)
}
StringsWithPatterns <- function(strings, patterns, ic = f) {
  for (p in patterns) strings <- strings[grepl(p, strings, ignore.case = ic)]
  return(strings)
}
StringsSepByNums <- function(string) {
  # this function doesn't work for strings with decimal numbers
  non.numeric.chars <- which(!CanBeNumeric(CharVec(string)))
  string.groups <- Grouper(non.numeric.chars)
  strings <- sapply(string.groups, StrElemsPasted, string = string)
  return(strings)
}
StrReverse <- function(string) {
  char.vec <- CharVec(string)
  char.vec.reversed <- rev(char.vec)
  string.reversed <- paste(char.vec.reversed, collapse = "")
  return(string.reversed)
}
T2Cols <- function(df) {
  cns <- colnames(df)
  last.nums <- sapply(cns, LastNumber)
  if (!MyAllEqual(paste0("t", last.nums), cns)) stop("columns must be t numbered (i.e. t1, t2, t3 or something like that)")
  no.nas <- sapply(df, na.omit)
  ls <- sapply(no.nas, length)
  ts <- rep(last.nums, ls)
  xs <- Reduce(c, no.nas)
  ts.xs <- cbind(ts, xs)
}
Vec2csv <- function(vec, csv.name, vec.name = "x", rn = F) {
  # vec: Vector, rn: RowNames
  d.f <- data.frame(vec)
  colnames(d.f) <- vec.name
  ext <- ".csv"
  if (RightChars(csv.name, 4) != ext) csv.name <- paste0(csv.name, ext)
  write.csv(d.f, csv.name, row.names = rn)
}


