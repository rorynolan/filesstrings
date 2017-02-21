#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::string PasteCollapse(CharacterVector strings, std::string collapse) {
  std::string out = as<std::string>(strings[0]);
  for (int i = 1; i < strings.size(); i++) {
    out += collapse;
    out += strings[i];
  }
  return out;
}
//' Apply paste collapse to each element of a list.
//'
//' This is the same as doing
//' \code{sapply(char.list, paste, collapse = collapse)}, it's just faster.
//'
//' @param char_list A list of character vectors.
//' @param collapse See \code{\link{paste}}.
//'
//' @return A list of character vectors.
//'
//' @examples
//' PasteCollapseListElems(list(1:3, c("a", 5, "rory")), collapse = "R")
//' @export
// [[Rcpp::export]]
CharacterVector PasteCollapseListElems(List char_list, std::string collapse = "") {
  int list_len = char_list.size();
  CharacterVector pasted(list_len);
  for (int i = 0; i < list_len; i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    pasted[i] = PasteCollapse(strings, collapse);
  }
  return(pasted);
}

//' Remove empty strings from a character list.
//'
//' @param char_list A list of character vectors.
//'
//' @return A list of character vectors.
//'
//' @examples
//' StrListRemoveEmpties(list(c("a", "", "b"), "gg", c("", 1, "")))
//' @export
// [[Rcpp::export]]
List StrListRemoveEmpties(List char_list) {
  List no_empties = clone(char_list);
  for (int i = 0; i < char_list.length(); i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    int j = 0;
    while (j < strings.size()) {
      if (strings[j] == "")
        strings.erase(j);
      else
        j++;
    }
    no_empties[i] = strings;
  }
  return(no_empties);
}

//' Get the nth element of each vector in a list.
//'
//' @param char_list A list of character vectors.
//' @param n The index of the element that you want from each vector.
//'
//' @return A list.
//'
//' @examples
//' CharListElemsNthElem(list(c("a", "b", "c"), c("d", "f", "a")), 2)
//' NumListElemsNthElem(list(1:5, 0:2), 4)
//' @export
// [[Rcpp::export]]
CharacterVector CharListElemsNthElem(List char_list, int n) {
  int sls = char_list.size();
  CharacterVector nths(sls);
  for (int i = 0; i < sls; i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    if (n < 0)
      n = strings.size() + 1 + n;
    nths[i] = (((n > strings.size()) | (n <= 0)) ? NA_STRING : strings[n - 1]);
  }
  return(nths);
}

//' @rdname CharListElemsNthElem
//' @param num_list A list of numeric vectors.
//' @export
// [[Rcpp::export]]
NumericVector NumListElemsNthElem(List num_list, int n) {
  int sls = num_list.size();
  NumericVector nths(sls);
  for (int i = 0; i < sls; i++) {
    NumericVector nums = as<NumericVector>(num_list[i]);
    nths[i] = ((n > nums.size()) ? NA_REAL : nums[n - 1]);
  }
  return(nths);
}

// [[Rcpp::export]]
CharacterVector InterleaveStrings(CharacterVector strings1,
                                  CharacterVector strings2) {
  int s1l = strings1.size();
  int s2l = strings2.size();
  int length_diff = s1l - s2l;
  if (abs(length_diff) > 1)
    return(NA_STRING);
  else {
    int l = s1l + s2l;
    CharacterVector interleaved(l);
    int i = 0;
    if (length_diff >= 0) {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings1[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings2[i / 2];
          i++;
        }
      }
    } else {
      while (i < l) {
        if (i % 2 == 0) {
          interleaved[i] = strings2[i / 2];
          i++;
        } else if (i < l) {
          interleaved[i] = strings1[i / 2];
          i++;
        }
      }
    }
  return(interleaved);
  }
}

// [[Rcpp::export]]
bool IsPrefix(std::string whole, std::string pre)
{
  return (std::strncmp(whole.c_str(), pre.c_str(), pre.size()) == 0);
}

// [[Rcpp::export]]
CharacterVector CorrectInterleave0(std::string orig,
                                  CharacterVector strings1,
                                  CharacterVector strings2) {
  CharacterVector interleave = NA_STRING;
  if (strings1.size() == 0)
    interleave = strings2;
  else if (strings2.size() == 0)
    interleave = strings1;
  else {
    CharacterVector onetwo = InterleaveStrings(strings1, strings2);
    if (PasteCollapse(onetwo, "") == orig)
      interleave = onetwo;
    else {
      CharacterVector twoone = InterleaveStrings(strings2, strings1);
      if (PasteCollapse(twoone, "") == orig)
        interleave = twoone;
    }
  }
  return(interleave);
}

// [[Rcpp::export]]
List CorrectInterleave(CharacterVector orig, List strings1, List strings2) {
  int l = orig.size();
  List interleaved(l);
  if (strings1.size() != l || strings2.size() != l) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CorrectInterleave0(as<std::string>(orig[i]),
                                          strings1[i], strings2[i]);
    }
  }
  return(interleaved);
}

// [[Rcpp::export]]
List InterleaveStringList(List strings1, List strings2) {
  int l = strings1.size();
  List interleaved(l);
  if (l != strings2.size()) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = InterleaveStrings(as<CharacterVector>(strings1[i]),
                                         as<CharacterVector>(strings2[i]));
    }
  }
  return(interleaved);
}

