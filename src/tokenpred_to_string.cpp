#include <Rcpp.h>
using namespace Rcpp;

// find next token - not used anymore

// [[Rcpp::export]]
List tokenpred_to_string_cpp(NumericVector probs, DataFrame target_tok, bool sample_tok_probs) {
  IntegerVector TOKEN = target_tok["TOKEN"];
  CharacterVector Word_ngram = target_tok["Word_ngram"];
  int n = target_tok.nrows();
  if (n == 0 || probs.size() != n) stop("Probability/token dimensions do not match");
  for (int i = 0; i < n; ++i) {
    if (!R_finite(probs[i]) || probs[i] < 0) stop("Invalid token probability");
  }
  if (sum(probs) <= 0) stop("Token probabilities must have positive mass");
  int token_idx;
  
  if (sample_tok_probs) {
    // Next token sampled with probabilities of probs
    NumericVector prob_cum = cumsum(probs);
    double u = as<double>(runif(1)) * prob_cum[n - 1];
    token_idx = std::lower_bound(prob_cum.begin(), prob_cum.end(), u) - prob_cum.begin();
  } else {
    // Next token is the token with highest probability
    token_idx = which_max(probs);
  }
  
  List result = List::create(
    _["token_id"] = TOKEN[token_idx],
                         _["token"] = Rcpp::as<std::string>(Word_ngram[token_idx])
  );
  
  return result;
}

// [[Rcpp::export]]
List parallel_tokenpred_to_string(NumericMatrix probs, DataFrame target_tok, bool sample_tok_probs) {
  int n = probs.nrow();
  List results(n);
  
// Rcpp allocation and R RNG calls must remain on the R main thread.
  for (int i = 0; i < n; i++) {
    results[i] = tokenpred_to_string_cpp(probs(i, _), target_tok, sample_tok_probs);
  }
  
  return results;
}