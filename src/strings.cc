#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
String PasteCollapse(CharacterVector strings, std::string collapse) {
  std::string ossOut;
  ossOut = strings[0];
  for (int i = 1; i < strings.size(); i++) {
    ossOut += collapse;
    ossOut += strings[i];
  }
  return ossOut;
}

//' @export
// [[Rcpp::export]]
CharacterVector PasteListElems(List string_list, std::string collapse) {
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
