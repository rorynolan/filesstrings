#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::string paste_collapse(CharacterVector strings, std::string collapse) {
  std::string out = as<std::string>(strings[0]);
  for (int i = 1; i < strings.size(); i++) {
    out += collapse;
    out += strings[i];
  }
  return out;
}

// Apply paste collapse to each element of a list.
//
// This is the same as doing
// \code{sapply(char.list, paste, collapse = collapse)}, it's just faster.
//
// @param char_list A list of character vectors.
// @param collapse See \code{\link{paste}}.
//
// @return A list of character vectors.
//
// @examples
// paste_collapse_list_elems(list(1:3, c("a", 5, "rory")), collapse = "R")
// @export
// [[Rcpp::export]]
CharacterVector paste_collapse_list_elems(List char_list,
                                          std::string collapse = "") {
  int list_len = char_list.size();
  CharacterVector pasted(list_len);
  for (int i = 0; i < list_len; i++) {
    CharacterVector strings = as<CharacterVector>(char_list[i]);
    pasted[i] = paste_collapse(strings, collapse);
  }
  return(pasted);
}

// Remove empty strings from a character list.
//
// @param char_list A list of character vectors.
//
// @return A list of character vectors.
//
// @examples
// str_list_remove_empties(list(c("a", "", "b"), "gg", c("", 1, "")))
// @export
// [[Rcpp::export]]
List str_list_remove_empties(List char_list) {
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

// Get the nth element of each vector in a list of numeric or character
// vectors.
//
// These are faster implementations of procedures that could very easily be
// done with [base::sapply].
//
// @param char_list A list of character vectors.
// @param n The index of the element that you want from each vector.
//
// @return A list.
//
// @examples
// str_list_nth_elems(list(c("a", "b", "c"), c("d", "f", "a")), 2)
// num_list_nth_elems(list(1:5, 0:2), 4)
// @export
// [[Rcpp::export]]
CharacterVector str_list_nth_elems(List char_list, int n) {
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

// @rdname str_list_nth_elems
// @param num_list A list of numeric vectors.
// @export
// [[Rcpp::export]]
NumericVector num_list_nth_elems(List num_list, int n) {
  int sls = num_list.size();
  NumericVector nths(sls);
  for (int i = 0; i < sls; i++) {
    NumericVector nums = as<NumericVector>(num_list[i]);
    nths[i] = ((n > nums.size()) ? NA_REAL : nums[n - 1]);
  }
  return(nths);
}

// [[Rcpp::export]]
CharacterVector interleave_strings(CharacterVector strings1,
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
CharacterVector correct_interleave_helper(std::string orig,
                                  CharacterVector strings1,
                                  CharacterVector strings2) {
  CharacterVector interleave = NA_STRING;
  if (strings1.size() == 0)
    interleave = strings2;
  else if (strings2.size() == 0)
    interleave = strings1;
  else {
    CharacterVector onetwo = interleave_strings(strings1, strings2);
    if (paste_collapse(onetwo, "") == orig)
      interleave = onetwo;
    else {
      CharacterVector twoone = interleave_strings(strings2, strings1);
      if (paste_collapse(twoone, "") == orig)
        interleave = twoone;
    }
  }
  return(interleave);
}

// [[Rcpp::export]]
List correct_interleave(CharacterVector orig, List strings1, List strings2) {
  int l = orig.size();
  List interleaved(l);
  if (strings1.size() != l || strings2.size() != l) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = correct_interleave_helper(as<std::string>(orig[i]),
                                          strings1[i], strings2[i]);
    }
  }
  return(interleaved);
}

// [[Rcpp::export]]
List interleave_char_lists(List strings1, List strings2) {
  int l = strings1.size();
  List interleaved(l);
  if (l != strings2.size()) {
    for (int i = 0; i < l; i++) {
      interleaved[i] = CharacterVector::create(NA_STRING);
    }
  }
  else {
    for (int i = 0; i < l; i++) {
      interleaved[i] = interleave_strings(as<CharacterVector>(strings1[i]),
                                          as<CharacterVector>(strings2[i]));
    }
  }
  return(interleaved);
}

// [[Rcpp::export]]
IntegerVector intmat_list_nrows(List intmat_list) {
  const std::size_t intmat_list_size = intmat_list.size();
  IntegerVector nrows(intmat_list_size);
  for (std::size_t i = 0; i != intmat_list_size; ++i) {
    IntegerMatrix mat_i = as<IntegerMatrix>(intmat_list[i]);
    nrows[i] = mat_i.nrow();
  }
  return nrows;
}

// [[Rcpp::export]]
IntegerVector intmat_list_bind_nth_rows(List intmat_list, IntegerVector n) {
  IntegerMatrix mat1 = as<IntegerMatrix>(intmat_list[0]);
  std::size_t nc = mat1.ncol();
  std::size_t intmat_list_size = intmat_list.size();
  IntegerMatrix out(intmat_list_size, nc);
  std::copy(mat1.row(n[0]).begin(), mat1.row(n[0]).end(), out.row(0).begin());
  if (intmat_list_size > 1) {
    for (std::size_t i = 1; i != intmat_list_size; ++i) {
      IntegerMatrix mat_i = as<IntegerMatrix>(intmat_list[i]);
      std::copy(mat_i.row(n[i]).begin(), mat_i.row(n[i]).end(),
                out.row(i).begin());
    }
  }
  return out;
}

bool is_prefix(const std::string& full, const std::string& prefix) {
  if (prefix.length() > full.length()) {
    return false;
  } else {
    return std::equal(prefix.begin(), prefix.end(), full.begin());
  }
}

int count_if(LogicalVector x, int& first_true) {
  int counter = 0;
  first_true = -1;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      if (counter == 0) first_true = i;
      counter++;
    }
  }
  return counter;
}

int match_arg_index1(std::string arg, CharacterVector choices) {
  std::size_t n_choices = choices.size();
  LogicalVector is_pre(n_choices);
  int first_true = -1;
  for (std::size_t i = 0; i != n_choices; ++i)
    is_pre[i] = is_prefix(std::string(choices[i]), arg);
  int n_matches = count_if(is_pre, first_true);
  if (n_matches == 0) {  // no match
    return -1;
  } else if (n_matches > 1) {  // ambiguity
    std::size_t arg_len = arg.length();
    for (std::size_t i = 0; i != n_choices; ++i) {
      std::string choice(choices[i]);
      if (arg_len == choice.length()) {
        if (std::equal(arg.begin(), arg.end(), choice.begin()))
          return i;
      }
    }
    return -2;
  } else {
    return first_true;
  }
}

// [[Rcpp::export]]
IntegerVector match_arg_index(CharacterVector arg, CharacterVector choices) {
  int arg_sz = arg.size();
  IntegerVector out(arg_sz);
  for (int i = 0; i != arg_sz; ++i)
    out[i] = match_arg_index1(std::string(arg[i]), choices);
  return out;
}
