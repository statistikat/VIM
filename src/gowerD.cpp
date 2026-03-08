#include <Rcpp.h>
#include <string>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif

void R_init_VIM(DllInfo* info) {
	R_registerRoutines(info, NULL, NULL, NULL, NULL);
	R_useDynamicSymbols(info, TRUE);
}

using namespace std;
using namespace Rcpp;
double distW(double x,double y, int type, double weight=1, double weightsum=1,double levOrder=1,
  double mixedConstant=0){
  double out=0;
  if(type==0){ //NUMERIC
    out = weight*abs(x-y)/weightsum;
  }else if(type==1){ //Categorical
    if(x!=y){
      out=weight/weightsum;
    }
  }else if(type==2){  //Ordered
    out=abs(x-y)/(levOrder-1)*weight/weightsum;
  }else if(type==3){  //Semi-Continous
    if(
      ((x==mixedConstant)&(y!=mixedConstant))|
        ((x!=mixedConstant)&(y==mixedConstant))
    ){
      out=weight/weightsum;
    }else if((x!=mixedConstant)&(y!=mixedConstant)){
      out=weight*abs(x-y)/weightsum;
    }
  }
//  Rprintf("xval %f, yval %f, dist %f \n",x,y,out);
  return out;
}
double distW1(const NumericMatrix& xMat, const NumericMatrix& yMat, int i, int j,
              const NumericVector& weight, const NumericVector& levOrder,
              double ncolMAX, const NumericVector& ncolVAR, const NumericVector& mixedConstant){
  double out=0;
  double weightsum = accumulate( weight.begin(), weight.end(), 0.0 );
  // compute the distance contribution of each variable
  for (int k=0; k<ncolMAX; k++) {
    if(k<ncolVAR(0)){ //NUMERIC
      out+=distW(xMat(i, k), yMat(j, k), 0, weight(k), weightsum);
    }else if(k<(ncolVAR(0)+ncolVAR(1))){ //Categorical
      out+=distW(xMat(i, k), yMat(j, k), 1, weight(k), weightsum);
    }else if(k<(ncolVAR(0)+ncolVAR(1)+ncolVAR(2))){  //Ordered
      out+=distW(xMat(i, k), yMat(j, k), 2, weight(k), weightsum,levOrder(k-(ncolVAR(0)+ncolVAR(1))));
    }else if(k<(ncolVAR(0)+ncolVAR(1)+ncolVAR(2)+ncolVAR(3))){  //Semi-Continous
      out+=distW(xMat(i, k), yMat(j, k), 3, weight(k), weightsum,1,mixedConstant(k-(ncolVAR(0)+ncolVAR(1)+ncolVAR(2))));
    }
  }
  return out;
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
                       SEXP levOrders,SEXP mixedConstants) {
  BEGIN_RCPP
  NumericMatrix xMat(dataX);	// creates Rcpp matrix from SEXP
  NumericMatrix yMat(dataY);	// creates Rcpp matrix from SEXP
  NumericVector ncolVAR(ncolNUMFAC);	// creates Rcpp matrix from SEXP
  NumericVector weight(weights);	// creates Rcpp matrix from SEXP
  NumericVector levOrder(levOrders);  // creates Rcpp matrix from SEXP
  NumericVector mixedConstant(mixedConstants);
  int nx = xMat.nrow();
  int ny = yMat.nrow();
  NumericMatrix delta(nx,ny);
  double ncolMAX = ncolVAR(0)+ncolVAR(1)+ncolVAR(2)+ncolVAR(3);
#pragma omp parallel for collapse(2) if (nx * ny > 1)
  for (int i=0; i<nx; i++) {
    for (int j=0; j<ny; j++) {
        delta(i,j)=distW1(xMat, yMat, i, j, weight, levOrder,
              ncolMAX, ncolVAR, mixedConstant);
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
  int n = as<int>(nR);
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
  SEXP mixedConstants,SEXP nR,SEXP returnMinR){
  BEGIN_RCPP
  List dist = gowerd( dataX,  dataY, weights, ncolNUMFAC, levOrders, mixedConstants);
  NumericMatrix delta = as<NumericMatrix>(dist["delta"]);
  int nc=delta.cols();
  int n = as<int>(nR);
  int returnMin = as<int>(returnMinR);
  NumericMatrix inds(n,nc);
  if(returnMin==0){
#pragma omp parallel for if (nc > 1)
    for (int i=0; i<nc; i++) {
      std::vector<double> column(delta.rows());
      std::copy(delta.begin() + static_cast<R_xlen_t>(i) * delta.rows(),
                delta.begin() + static_cast<R_xlen_t>(i + 1) * delta.rows(),
                column.begin());
      whichminN_impl(column, n, inds.begin() + static_cast<R_xlen_t>(i) * n);
    }
    return List::create(
        Named( "ind" ) = inds
    );
  }else{
    NumericMatrix mins(n,nc);
#pragma omp parallel for if (nc > 1)
    for (int i=0; i<nc; i++) {
      std::vector<double> column(delta.rows());
      std::copy(delta.begin() + static_cast<R_xlen_t>(i) * delta.rows(),
                delta.begin() + static_cast<R_xlen_t>(i + 1) * delta.rows(),
                column.begin());
      whichminN_impl(column, n,
                     inds.begin() + static_cast<R_xlen_t>(i) * n,
                     mins.begin() + static_cast<R_xlen_t>(i) * n);
    }
    return List::create(
        Named( "ind" ) = inds,
        Named( "min" ) = mins
    );
  }
  END_RCPP
}
