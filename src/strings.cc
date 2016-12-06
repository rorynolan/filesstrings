// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/algorithm/string/predicate.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
String PasteCollapse(CharacterVector strings, std::string collapse) {
  std::string out;
  out = strings[0];
  for (int i = 1; i < strings.size(); i++) {
    out += collapse;
    out += strings[i];
  }
  return out;
}

//' @export
// [[Rcpp::export]]
CharacterVector PasteListElems(List string_list, std::string collapse = "") {
  int list_len = string_list.size();
  CharacterVector pasted(list_len);
  for (int i = 0; i < list_len; i++) {
    CharacterVector strings = as<CharacterVector>(string_list[i]);
    pasted[i] = PasteCollapse(strings, collapse);
  }
  return(pasted);
}

//' @export
// [[Rcpp::export]]
List StrListRemoveEmpties(List string_list) {
  List no_empties = clone(string_list);
  for (int i = 0; i < string_list.length(); i++) {
    CharacterVector strings = as<CharacterVector>(string_list[i]);
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

//' @export
// [[Rcpp::export]]
CharacterVector CharListElemsNthElem(List string_list, int n) {
  int sls = string_list.size();
  CharacterVector nths(sls);
  for (int i = 0; i < sls; i++) {
    CharacterVector strings = as<CharacterVector>(string_list[i]);
    nths[i] = ((n > strings.size()) ? NA_STRING : strings[n - 1]);
  }
  return(nths);
}

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
bool IsPrefix(std::string pre, std::string whole) {
  return(boost::starts_with(whole, pre));
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
CharacterVector CorrectInterleave0(std::string orig,
                                  CharacterVector strings1,
                                  CharacterVector strings2) {
  CharacterVector interleave;
  if (IsPrefix(as<std::string>(strings1[0]), orig))
    interleave = InterleaveStrings(strings1, strings2);
  else if (IsPrefix(as<std::string>(strings2[0]), orig))
    interleave = InterleaveStrings(strings2, strings1);
  else
    return(NA_STRING);
  if (PasteCollapse(interleave, "") == orig)
    return(interleave);
  else
    return(NA_STRING);
}

// [[Rcpp::export]]
List CorrectInterleave(CharacterVector orig, List strings1, List strings2) {
  int l = orig.size();
  List interleaved(l);
  if (strings1.size() != l || strings2.size() != l) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = NA_STRING;
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
      interleaved[i] = NA_STRING;
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
