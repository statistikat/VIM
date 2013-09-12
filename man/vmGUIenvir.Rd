\name{vmGUIenvir}
\alias{vmGUIenvir}
\alias{putVm}
\alias{getVm}
\alias{existsVm}
\alias{rmVm}
\title{Environment for the GUI for Visualization and Imputation of Missing Values}
\usage{
putVm(x,value)
getVm(x,mode="any")
existsVm(x,mode="any")
rmVm(...)
}
\arguments{
\item{x}{object name}
\item{value}{value to be assigned to x}
\item{mode}{see 'exists'}
\item{...}{see 'rm'}

}
\description{
    Location were everything from package VIM and VIMGUI is stored.
}
\details{
    Internal information regarding the VIM GUI is stored in the environment 
    \code{vmGUIenvir}.
}
\references{
M. Templ, A. Alfons, P. Filzmoser (2012)  
Exploring incomplete data using visualization tools. 
\emph{Journal of Advances in Data Analysis and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.  
}
\author{Andreas Alfons, based on an initial design by Matthias Templ,
modifications by Bernd Prantner}
\keyword{multivariate}
\keyword{hplot}

