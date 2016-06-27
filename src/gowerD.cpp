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
      if(i==j){
        // set the diagonal to Inf
        delta(i,j)=R_PosInf;
      }else{
        delta(i,j)=0.0;
        // compute the distance contribution of each variable
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
//            if(i<50){
//              Rprintf("k %d levorder %f x %f y %f DIST %f \n",k,levOrder(k),xMat(i,k+ncolVAR(0)+ncolVAR(1)),yMat(j,k+ncolVAR(0)+ncolVAR(1)),a);
//            }
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
  }

  return Rcpp::List::create(
      Rcpp::Named( "delta" ) = delta
  );

  END_RCPP
}

// Find the index of nR minimal values per column of a matrix xR
RcppExport SEXP whichminN(SEXP xR, SEXP nR, int returnValue) {
  BEGIN_RCPP
  Rcpp::NumericVector x(xR);  // creates Rcpp matrix from SEXP
  int n = Rcpp::as<int>(nR);
  Rcpp::NumericVector out(n);
  Rcpp::NumericVector::iterator it = std::min_element(x.begin(), x.end());  // STL algo // iterator type
  out[0]=it - x.begin()+1;
  if(returnValue==1){
    Rcpp::NumericVector outMin(n);
    outMin[0]=x[it - x.begin()];
    x[it - x.begin()]=R_PosInf;
    for (int i=2; i<=n; i++) {
      it = std::min_element(x.begin(), x.end());
      out[i-1]=it - x.begin()+1;
      outMin[i-1]=x[it - x.begin()];
      x[it - x.begin()]=R_PosInf;
    }
    return Rcpp::List::create(
        Rcpp::Named( "which" ) = out,
      Rcpp::Named( "mins" ) = outMin
    ) ;
  }else{
    x[it - x.begin()]=R_PosInf;
    for (int i=2; i<=n; i++) {
      it = std::min_element(x.begin(), x.end());
      out[i-1]=it - x.begin()+1;
      x[it - x.begin()]=R_PosInf;
    }
    return Rcpp::List::create(
        Rcpp::Named( "which" ) = out
    ) ;
  }
  END_RCPP
}

RcppExport SEXP gowerDind(SEXP dataX, SEXP dataY,SEXP weights,SEXP ncolNUMFAC,SEXP levOrders,
  SEXP mixedConstants,SEXP nR,SEXP returnMinR){
  BEGIN_RCPP
  Rcpp::List dist = gowerD( dataX,  dataY, weights, ncolNUMFAC, levOrders, mixedConstants);
  Rcpp::NumericMatrix delta = Rcpp::as<Rcpp::NumericMatrix>(dist["delta"]);
  int nc=delta.cols();
  int n = Rcpp::as<int>(nR);
  int returnMin = Rcpp::as<int>(returnMinR);
  Rcpp::NumericMatrix inds(n,nc);
  if(returnMin==0){
    for (int i=0; i<nc; i++) {
      Rcpp::NumericVector zz1 = delta( Rcpp::_, i);
      Rcpp::List resultList(whichminN(zz1,nR,returnMin));
      //Rcpp::NumericVector tmp(Rcpp::as<Rcpp::NumericVector>(resultList["which"]));
      inds(Rcpp::_,i)=Rcpp::as<Rcpp::NumericVector>(resultList["which"]);
    }
    return Rcpp::List::create(
        Rcpp::Named( "ind" ) = inds
    );
  }else{
    Rcpp::NumericMatrix mins(n,nc);
    for (int i=0; i<nc; i++) {
      Rcpp::NumericVector zz1 = delta( Rcpp::_, i);
      Rcpp::List resultList(whichminN(zz1,nR,returnMin));
      inds(Rcpp::_,i)=Rcpp::as<Rcpp::NumericVector>(resultList["which"]);
      mins(Rcpp::_,i)=Rcpp::as<Rcpp::NumericVector>(resultList["mins"]);
    }
    return Rcpp::List::create(
        Rcpp::Named( "ind" ) = inds,
        Rcpp::Named( "min" ) = mins
    );
  }
  END_RCPP
}  


