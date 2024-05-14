#include <Rcpp.h>
using namespace Rcpp;

//devides input into sequences of fixed length
//first add zero padding, length depends on the number of token the target var consists of (#tok_t)
//the model input shape is then ncol of train embedding + lenth of zero padding
//per input string/line, there are #tok_t sequences produces, the first shows the first token of
//the target var, the second the first and second token of target var
//len_target: number of tokens in target variable (e.g. categoric vars-> len_target=1)
//train_tokenized: matrix containing the token ids for each token in a row


// [[Rcpp::export]]
NumericMatrix training_seq_cpp(int len_target, NumericMatrix train_tokenized) {
  int nrows = train_tokenized.nrow();
  int orig_train_len = train_tokenized.ncol();
  int padded_len = orig_train_len + len_target - 1;
  
  // Create padded matrix
  NumericMatrix padded_train(nrows, padded_len);
  for (int i = 0; i < nrows; i++) {
    for (int j = len_target - 1; j < padded_len; j++) {
      padded_train(i, j) = train_tokenized(i, j - len_target + 1);
    }
  }
  
  // Create sequences
  NumericMatrix seqs(nrows * len_target, orig_train_len);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < len_target; j++) {
      for (int k = 0; k < orig_train_len; k++) {
        seqs(i + j * nrows, k) = padded_train(i, j + k);
      }
    }
  }
  
  return seqs;
}