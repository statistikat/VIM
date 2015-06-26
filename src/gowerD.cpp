#include <Rcpp.h>
#include <string>
#include <cmath>

using namespace std;

RcppExport SEXP gowerD(SEXP dataX, SEXP dataY,SEXP weights,SEXP ncolNUMFAC,SEXP levOrders,SEXP mixedConstants) {
  BEGIN_RCPP
  Rcpp::NumericMatrix xMat(dataX);	// creates Rcpp matrix from SEXP
  Rcpp::NumericMatrix yMat(dataY);	// creates Rcpp matrix from SEXP
  Rcpp::NumericVector ncolVAR(ncolNUMFAC);	// creates Rcpp matrix from SEXP
  Rcpp::NumericVector weight(weights);	// creates Rcpp matrix from SEXP
  Rcpp::NumericVector levOrder(levOrders);  // creates Rcpp matrix from SEXP
  Rcpp::NumericVector mixedConstant(mixedConstants);
  int nx = xMat.rows();
  int ny = yMat.rows();
  Rcpp::NumericMatrix delta(nx,ny);
  double weightsum = std::accumulate( weight.begin(), weight.end(), 0.0 );
  double ncolMAX = max(ncolVAR(0),ncolVAR(1)); //should be rewritten with std::max_element!?!?
  ncolMAX=max(ncolMAX,ncolVAR(2));
  ncolMAX=max(ncolMAX,ncolVAR(3));
  for (int i=0; i<nx; i++) {
    for (int j=0; j<ny; j++) {
      delta(i,j)=0.0;
      for (int k=0; k<ncolMAX; k++) {
        if(k<ncolVAR(0)){ //NUMERIC
          double a=abs(xMat(i,k)-yMat(j,k))/weightsum;
          delta(i,j)=delta(i,j)+weight(k)*a;
        }
        if(k<ncolVAR(1)){ //Categorical
          if(xMat(i,k+ncolVAR(0))!=yMat(j,k+ncolVAR(0))){
            delta(i,j)=delta(i,j)+weight(k+ncolVAR(0))/weightsum;
          }
        }
        if(k<ncolVAR(2)){  //Ordered
          double a=abs(xMat(i,k+ncolVAR(0)+ncolVAR(1))-yMat(j,k+ncolVAR(0)+ncolVAR(1)))/(levOrder(k)-1);
//          if(i<50){
//            Rprintf("k %d levorder %f x %f y %f DIST %f \n",k,levOrder(k),xMat(i,k+ncolVAR(0)+ncolVAR(1)),yMat(j,k+ncolVAR(0)+ncolVAR(1)),a);
//          }
          delta(i,j)=delta(i,j)+a*weight(k+ncolVAR(0)+ncolVAR(1))/weightsum;
        }
        if(k<ncolVAR(3)){  //Semi-Continous
          if(
            ((xMat(i,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))==mixedConstant(k))&(yMat(j,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))!=mixedConstant(k)))|
            ((xMat(i,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))!=mixedConstant(k))&(yMat(j,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))==mixedConstant(k)))
            ){
              delta(i,j)=delta(i,j)+weight(k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))/weightsum;
          }else if((xMat(i,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))!=mixedConstant(k))&(yMat(j,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))!=mixedConstant(k))){
            double a=abs(xMat(i,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2))-yMat(j,k+ncolVAR(0)+ncolVAR(1)+ncolVAR(2)))/weightsum;
            delta(i,j)=delta(i,j)+weight(k)*a;
          }
        }
      }
    }
  }
  //  for (int i=0; i<nx; i++) {
  //    for (int j=0; j<ny; j++) {
  //delta(i,j)=delta(i,j)/weightsum;
  //}
  //}

  return Rcpp::List::create(
      Rcpp::Named( "delta" ) = delta
  ) ;

  END_RCPP
}
