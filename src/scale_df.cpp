#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame scale_df(DataFrame data, LogicalVector is_numeric) {
  // Rcpp::DataFrame allows for mixed data types
  // LogicalVector is_numeric: logical vector indicates which columns are numeric
  // (TRUE means the column is numeric).

  int n_cols = data.size();
  int n_rows = data.nrows();

  List scaled_data(n_cols);  // list to store scaled or original columns
  CharacterVector col_names = data.names();  // keep column names

  // Loop through each column
  for (int j = 0; j < n_cols; ++j) {
    if (is_numeric[j]) {
      // if the column is numeric (is_numeric[j] == TRUE) scale it
      // otherwise leave the column unchanged

      NumericVector col = data[j];
      double col_min = Rcpp::min(col); // get minimum value in the column
      double col_max = Rcpp::max(col); // get maximum value in the column

      // scale numeric column
      NumericVector scaled_col(n_rows);
      for (int i = 0; i < n_rows; ++i) {
        scaled_col[i] = (col[i] - col_min) / (col_max - col_min);
      }
      scaled_data[j] = scaled_col;
    } else {
      // non-numeric column remains unchanged
      scaled_data[j] = data[j];
    }
  }
  // Set the names of the columns
  scaled_data.attr("names") = col_names;
  // return the scaled data as a DataFrame
  return DataFrame(scaled_data);
}

