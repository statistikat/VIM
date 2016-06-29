#include <Rcpp.h>
#include <string>
#include <cmath>

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
double distW1(NumericVector xV,NumericVector yV, NumericVector weight,
              NumericVector levOrder,
              double ncolMAX,NumericVector ncolVAR,NumericVector mixedConstant){
  double out=0;
  double weightsum = accumulate( weight.begin(), weight.end(), 0.0 );
  // compute the distance contribution of each variable
  for (int k=0; k<ncolMAX; k++) {
    if(k<ncolVAR(0)){ //NUMERIC
      out+=distW(xV(k),yV(k), 0, weight(k), weightsum);
    }
    if(k<ncolVAR(1)){ //Categorical
      out+=distW(xV(k),yV(k), 1, weight(k), weightsum);
    }
    if(k<ncolVAR(2)){  //Ordered
      out+=distW(xV(k),yV(k), 1, weight(k), weightsum,levOrder(k));
    }
    if(k<ncolVAR(3)){  //Semi-Continous
      out+=distW(xV(k),yV(k), 1, weight(k), weightsum,1,mixedConstant(k));
    }
  }
  return out;
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
  double ncolMAX = max(ncolVAR(0),ncolVAR(1)); //should be rewritten with std::max_element!?!?
  ncolMAX=max(ncolMAX,ncolVAR(2));
  ncolMAX=max(ncolMAX,ncolVAR(3));
  for (int i=0; i<nx; i++) {
    for (int j=0; j<ny; j++) {
      // if(i==j){
      //   // set the diagonal to Inf
      //   delta(i,j)=R_PosInf;
      // }else{
        delta(i,j)=distW1(xMat(i,_),yMat(j,_),weight,levOrder,
              ncolMAX,ncolVAR,mixedConstant);
      // }
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
  NumericVector::iterator it = min_element(x.begin(), x.end());  // STL algo // iterator type
  out[0]=it - x.begin()+1;
  if(returnValue==1){
    NumericVector outMin(n);
    outMin[0]=x[it - x.begin()];
    x[it - x.begin()]=R_PosInf;
    for (int i=2; i<=n; i++) {
      it = min_element(x.begin(), x.end());
      out[i-1]=it - x.begin()+1;
      outMin[i-1]=x[it - x.begin()];
      x[it - x.begin()]=R_PosInf;
    }
    return List::create(
        Named( "which" ) = out,
        Named( "mins" ) = outMin
    ) ;
  }else{
    x[it - x.begin()]=R_PosInf;
    for (int i=2; i<=n; i++) {
      it = min_element(x.begin(), x.end());
      out[i-1]=it - x.begin()+1;
      x[it - x.begin()]=R_PosInf;
    }
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
    for (int i=0; i<nc; i++) {
      NumericVector zz1 = delta( Rcpp::_, i);
      List resultList(whichminN(zz1,nR,returnMin));
      inds(_,i)=as<NumericVector>(resultList["which"]);
    }
    return List::create(
        Named( "ind" ) = inds
    );
  }else{
    NumericMatrix mins(n,nc);
    for (int i=0; i<nc; i++) {
      NumericVector zz1 = delta(_, i);
      List resultList(whichminN(zz1,nR,returnMin));
      inds(_,i)=as<NumericVector>(resultList["which"]);
      mins(_,i)=as<NumericVector>(resultList["mins"]);
    }
    return List::create(
        Named( "ind" ) = inds,
        Named( "min" ) = mins
    );
  }
  END_RCPP
}