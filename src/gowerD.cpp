#include <Rcpp.h>
#include <string>
#include <cmath>
#include <climits>
#include <algorithm>
#include <vector>
#include <utility>
#ifdef _OPENMP
#include <omp.h>
#endif

// Number of OpenMP threads for the parallel regions below. The R side passes
// vim_ncores(): getOption("VIM.ncores"), else 2 under R CMD check (CRAN allows
// at most two cores), else 0 = OpenMP's own default. Always 1 without OpenMP.
static int vim_omp_threads(int n) {
#ifdef _OPENMP
  if (n == NA_INTEGER || n < 1) n = omp_get_max_threads();
  return n < 1 ? 1 : n;
#else
  (void)n;
  return 1;
#endif
}

void R_init_VIM(DllInfo* info) {
	R_registerRoutines(info, NULL, NULL, NULL, NULL);
	R_useDynamicSymbols(info, TRUE);
}

using namespace std;
using namespace Rcpp;
static double validate_gower_inputs(const NumericMatrix& x, const NumericMatrix& y,
                                 const NumericVector& weights,
                                 const NumericVector& counts,
                                 const NumericVector& levels,
                                 const NumericVector& constants) {
  if (x.ncol() != y.ncol() || x.ncol() == 0)
    stop("Distance matrices must have the same positive number of columns");
  if (counts.size() != 4)
    stop("Type counts must contain four nonnegative integers");
  double total = 0;
  for (double count : counts) {
    if (!R_FINITE(count) || count < 0 || count != std::floor(count))
      stop("Type counts must contain four nonnegative integers");
    total += count;
  }
  if (total != x.ncol())
    stop("Type counts must sum to the number of matrix columns");
  if (weights.size() != x.ncol())
    stop("Weights must contain one value per matrix column");
  double weight_sum = 0;
  for (double weight : weights) {
    if (!R_FINITE(weight) || weight < 0)
      stop("Weights must be finite and nonnegative with a positive finite total");
    weight_sum += weight;
  }
  if (!R_FINITE(weight_sum) || weight_sum <= 0)
    stop("Weights must be finite and nonnegative with a positive finite total");
  if (levels.size() != counts[2])
    stop("Ordinal level counts must match the number of ordinal columns");
  for (double level : levels)
    if (!R_FINITE(level) || level < 1 || level != std::floor(level))
      stop("Ordinal level counts must be positive finite integers");
  if (constants.size() != counts[3])
    stop("Mixed constants must match the number of mixed columns");
  for (double constant : constants)
    if (!R_FINITE(constant)) stop("Mixed constants must be finite");
  for (double value : x)
    if (!R_FINITE(value)) stop("Distance matrices must contain finite values");
  for (double value : y)
    if (!R_FINITE(value)) stop("Distance matrices must contain finite values");
  return weight_sum;
}

static int neighbor_count(SEXP value, R_xlen_t available) {
  if ((TYPEOF(value) != INTSXP && TYPEOF(value) != REALSXP) ||
      Rf_xlength(value) != 1)
    stop("Neighbour count must be a single positive integer");
  const double n = as<double>(value);
  if (!R_FINITE(n) || n < 1 || n != std::floor(n) || n > INT_MAX)
    stop("Neighbour count must be a single positive integer");
  if (n > available)
    stop("Neighbour count exceeds the number of eligible donors");
  return static_cast<int>(n);
}

// Prepared once per call, then shared read-only across all pairs and workers.
class GowerKernel {
  const double* const weights;
  const double* const mixed_constants;
  const double weight_sum;
  const int numeric_end, categorical_end, ordered_end, mixed_end;
  std::vector<double> ordered_denominators;

public:
  GowerKernel(const NumericVector& weight, const NumericVector& counts,
              const NumericVector& levels, const NumericVector& constants,
              double sum)
    : weights(weight.begin()), mixed_constants(constants.begin()), weight_sum(sum),
      numeric_end(counts[0]), categorical_end(numeric_end + counts[1]),
      ordered_end(categorical_end + counts[2]), mixed_end(ordered_end + counts[3]),
      ordered_denominators(levels.size()) {
    for (R_xlen_t i = 0; i < levels.size(); ++i)
      ordered_denominators[i] = levels[i] > 1 ? levels[i] - 1 : 1;
  }

  // Strides are one for packed rows and the row count for R matrices.
  double distance(const double* x, const double* y,
                  R_xlen_t x_stride, R_xlen_t y_stride) const {
    double out = 0;
    int col = 0;
    // Preserve contribution order and the order of arithmetic within each
    // contribution so rounding cannot change ties or donor rankings.
    for (; col < numeric_end; ++col)
      out += weights[col] * std::abs(x[col * x_stride] - y[col * y_stride]) / weight_sum;
    for (; col < categorical_end; ++col)
      if (x[col * x_stride] != y[col * y_stride])
        out += weights[col] / weight_sum;
    for (; col < ordered_end; ++col)
      out += std::abs(x[col * x_stride] - y[col * y_stride]) /
        ordered_denominators[col - categorical_end] * weights[col] / weight_sum;
    for (; col < mixed_end; ++col) {
      const double a = x[col * x_stride], b = y[col * y_stride];
      const double point_mass = mixed_constants[col - ordered_end];
      const bool a_at_mass = a == point_mass, b_at_mass = b == point_mass;
      if (a_at_mass != b_at_mass)
        out += weights[col] / weight_sum;
      else if (!a_at_mass)
        out += weights[col] * std::abs(a - b) / weight_sum;
    }
    return out;
  }
};

static std::vector<double> pack_gower_rows(const NumericMatrix& matrix) {
  const R_xlen_t nr = matrix.nrow(), nc = matrix.ncol();
  const double* const input = matrix.begin();
  std::vector<double> rows(matrix.size());
  for (R_xlen_t row = 0; row < nr; ++row)
    for (R_xlen_t col = 0; col < nc; ++col)
      rows[row * nc + col] = input[col * nr + row];
  return rows;
}

void whichminN_impl(std::vector<double>& x, int n, double* out, double* outMin=nullptr) {
  std::vector<double>::iterator it = min_element(x.begin(), x.end());
  out[0] = static_cast<double>(it - x.begin() + 1);
  if (outMin != nullptr) {
    outMin[0] = x[it - x.begin()];
  }
  x[it - x.begin()] = R_PosInf;
  for (int i=2; i<=n; i++) {
    it = min_element(x.begin(), x.end());
    out[i-1] = static_cast<double>(it - x.begin() + 1);
    if (outMin != nullptr) {
      outMin[i-1] = x[it - x.begin()];
    }
    x[it - x.begin()] = R_PosInf;
  }
}
// [[Rcpp::export]]
RcppExport SEXP gowerd(SEXP dataX, SEXP dataY,SEXP weights,SEXP ncolNUMFAC,
                       SEXP levOrders,SEXP mixedConstants, int nthreads = 0) {
  BEGIN_RCPP
  NumericMatrix xMat(dataX);	// creates Rcpp matrix from SEXP
  NumericMatrix yMat(dataY);	// creates Rcpp matrix from SEXP
  NumericVector ncolVAR(ncolNUMFAC);	// creates Rcpp matrix from SEXP
  NumericVector weight(weights);	// creates Rcpp matrix from SEXP
  NumericVector levOrder(levOrders);  // creates Rcpp matrix from SEXP
  NumericVector mixedConstant(mixedConstants);
  const double sum = validate_gower_inputs(xMat, yMat, weight, ncolVAR, levOrder,
                                           mixedConstant);
  const GowerKernel kernel(weight, ncolVAR, levOrder, mixedConstant, sum);
  const R_xlen_t nx = xMat.nrow(), ny = yMat.nrow();
  NumericMatrix delta(xMat.nrow(), yMat.nrow());
  const double* const x = xMat.begin();
  const double* const y = yMat.begin();
  double* const out = delta.begin();
  const int nth = vim_omp_threads(nthreads);
  (void)nth;  // only used by the OpenMP pragma
#pragma omp parallel for collapse(2) if (nth > 1 && (nx > 1 || ny > 1)) num_threads(nth)
  for (R_xlen_t j = 0; j < ny; ++j) {
    for (R_xlen_t i = 0; i < nx; ++i) {
      out[j * nx + i] = kernel.distance(x + i, y + j, nx, ny);
    }
  }

  return List::create(
      Named( "delta" ) = delta
  );

  END_RCPP
}

// Find the index of nR minimal values per column of a matrix xR
// [[Rcpp::export]]
RcppExport SEXP whichminN(SEXP xR, SEXP nR, int returnValue) {
  BEGIN_RCPP
  NumericVector x(xR);  // creates Rcpp matrix from SEXP
  int n = neighbor_count(nR, x.size());
  for (double value : x)
    if (!R_FINITE(value)) stop("Distances must contain finite values");
  NumericVector out(n);
  std::vector<double> xCopy(x.begin(), x.end());
  if(returnValue==1){
    NumericVector outMin(n);
    whichminN_impl(xCopy, n, out.begin(), outMin.begin());
    return List::create(
        Named( "which" ) = out,
        Named( "mins" ) = outMin
    ) ;
  }else{
    whichminN_impl(xCopy, n, out.begin());
    return List::create(
        Named( "which" ) = out
    ) ;
  }
  END_RCPP
}

// [[Rcpp::export]]
RcppExport SEXP gowerDind(SEXP dataX, SEXP dataY,SEXP weights,SEXP ncolNUMFAC,SEXP levOrders,
  SEXP mixedConstants,SEXP nR,SEXP returnMinR, int nthreads = 0){
  BEGIN_RCPP
  const NumericMatrix donors(dataX);
  const NumericMatrix recipients(dataY);
  const NumericVector weight(weights), counts(ncolNUMFAC);
  const NumericVector levels(levOrders), constants(mixedConstants);
  const int nx = donors.nrow();
  const int ny = recipients.nrow();
  const int n = neighbor_count(nR, nx);
  const double sum = validate_gower_inputs(donors, recipients, weight, counts,
                                           levels, constants);
  const GowerKernel kernel(weight, counts, levels, constants, sum);

  const bool returnMin = as<int>(returnMinR) != 0;
  NumericMatrix inds(n, ny);
  NumericMatrix mins;
  if (returnMin) mins = NumericMatrix(n, ny);
  double* const index_out = inds.begin();
  double* const min_out = returnMin ? mins.begin() : nullptr;

  // Each worker reuses its own heap. Lexicographic order breaks distance ties
  // by original donor row, so the root is always the worst retained candidate.
  using Candidate = std::pair<double, int>;
  const int nth = std::min(vim_omp_threads(nthreads), std::max(1, ny));
  std::vector<std::vector<Candidate>> scratch;
  if (n > 1 && ny > 0)
    scratch.resize(nth, std::vector<Candidate>(n));

  // Pack once so the streaming search can read both rows contiguously.
  const R_xlen_t columns = donors.ncol();
  const auto donor_rows = ny > 0 ? pack_gower_rows(donors) : std::vector<double>();
  const auto recipient_rows = pack_gower_rows(recipients);

  int invalid_distance = 0;
#pragma omp parallel for if (nth > 1) num_threads(nth) reduction(|:invalid_distance)
  for (int j = 0; j < ny; ++j) {
    const R_xlen_t offset = static_cast<R_xlen_t>(j) * n;
    const double* const recipient = recipient_rows.data() + j * columns;
    const auto distance_to = [&](int i) {
      return kernel.distance(donor_rows.data() + i * columns, recipient, 1, 1);
    };
    if (n == 1) {
      Candidate best(R_PosInf, 0);
      for (int i = 0; i < nx; ++i) {
        const double distance = distance_to(i);
        if (!R_FINITE(distance)) invalid_distance = 1;
        else if (distance < best.first) best = Candidate(distance, i);
      }
      index_out[offset] = best.second + 1;
      if (min_out) min_out[offset] = best.first;
      continue;
    }

    int worker = 0;
#ifdef _OPENMP
    worker = omp_get_thread_num();
#endif
    Candidate* const heap = scratch[worker].data();
    int size = 0;
    for (int i = 0; i < nx; ++i) {
      const double distance = distance_to(i);
      if (!R_FINITE(distance)) {
        invalid_distance = 1;
        continue;
      }
      const Candidate candidate(distance, i);
      if (size < n) {
        heap[size++] = candidate;
        if (size == n) std::make_heap(heap, heap + n);
      } else if (candidate < heap[0]) {
        std::pop_heap(heap, heap + n);
        heap[n - 1] = candidate;
        std::push_heap(heap, heap + n);
      }
    }
    if (size < n) continue;  // A non-finite distance will trigger an error.
    std::sort_heap(heap, heap + n);
    for (int k = 0; k < n; ++k) {
      index_out[offset + k] = heap[k].second + 1;
      if (min_out) min_out[offset + k] = heap[k].first;
    }
  }
  if (invalid_distance) stop("Distances must contain finite values");
  if (returnMin) return List::create(Named("ind") = inds, Named("min") = mins);
  return List::create(Named("ind") = inds);
  END_RCPP
}
