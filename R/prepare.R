# ------------------------------------------
# Authors: Andreas Alfons, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ------------------------------------------

prepare <- function(x, scaling = c("none","classical","MCD","robust","onestep"),
                    transformation = c("none","minus","reciprocal","logarithm",
                                       "exponential","boxcox","clr","ilr","alr"),
                    alpha = NULL, powers = NULL, start = 0, alrVar) {
  UseMethod("prepare", x)
}

prepare.data.frame <- function(x, scaling = c("none","classical","MCD","robust","onestep"),
                               transformation = c("none","minus","reciprocal","logarithm",
                                                  "exponential","boxcox","clr","ilr","alr"),
                               alpha = NULL, powers = NULL, start = 0, alrVar) {
  as.data.frame(prepare_work(x, scaling, transformation, alpha, powers, start, alrVar)) 
}

prepare.survey.design <- function(x, scaling = c("none","classical","MCD","robust","onestep"),
                                  transformation = c("none","minus","reciprocal","logarithm",
                                                     "exponential","boxcox","clr","ilr","alr"),
                                  alpha = NULL, powers = NULL, start = 0, alrVar) {
  x$variables <- as.data.frame(prepare_work(x$variables, scaling, transformation, alpha, powers, start, alrVar)) 
  x$call <- sys.call(-1)
  x
}

prepare.default <- function(x, scaling = c("none","classical","MCD","robust","onestep"),
                            transformation = c("none","minus","reciprocal","logarithm",
                                               "exponential","boxcox","clr","ilr","alr"),
                            alpha = NULL, powers = NULL, start = 0, alrVar) {
  prepare_work(as.data.frame(x), scaling, transformation, alpha, powers, start, alrVar) 
}

prepare_work <- function(x, 
        scaling = c("none","classical","MCD","robust","onestep"),
        transformation = c("none","minus","reciprocal","logarithm",
            "exponential","boxcox","clr","ilr","alr"),
        alpha = NULL, powers = NULL, start = 0, alrVar) {
    if(is.data.frame(x)) x <- data.matrix(x)
    transformation <- match.arg(transformation)
    if(transformation != "none") {
        x <- switch(transformation, minus=-x, reciprocal=1/x, 
            logarithm=log10(x), exponential=10^x, 
            boxcox=boxcoxVIM(x, powers, start), clr=clr(x), 
            ilr=ilr(x), alr=alr(x, alrVar))
    }
    scaling <- match.arg(scaling)
    if(scaling != "none") {
        x <- switch(scaling, classical=scale(x), MCD=scaleMcd(x, alpha=alpha), 
            robust=scaleRob(x), onestep=scaleRob(x, onestep=TRUE))
    }
    x
}

scaleMcd <- function(x, alpha = NULL) {
    tmp <- if(is.null(alpha)) covMcd(x) else covMcd(x, alpha=alpha)
    scale(x, center=tmp$center, scale=diag(tmp$cov))
}

scaleRob <- function(x, onestep = FALSE) {
    if(onestep) {
        if(is.null(dim(x))) cen <- onestepMean(x, na.rm=TRUE) 
        else cen <- apply(x, 2, onestepMean, na.rm=TRUE)
        scale(x, center=cen)
    } else {
        if(is.null(dim(x))) {
            cen <- median(x, na.rm=TRUE)
            sc <- mad(x, na.rm=TRUE)
        }
        else {
            cen <- apply(x, 2, median, na.rm=TRUE)
            sc <- apply(x, 2, mad, na.rm=TRUE)
        }
        scale(x, center=cen, scale=sc)
    }
}

onestepMean <- function(x, na.rm = FALSE) {
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    mMedian <- median(x, na.rm=na.rm)
    mMad <- mad(x, na.rm=na.rm)
    constant <- 3/1.486
    limit <- mMedian + c(-1,1) * constant * mMad
    x[x < limit[1]] <- limit[1]
    x[x > limit[2]] <- limit[2]
    mean(x, na.rm=na.rm)
}

boxcoxVIM <- function(x, powers = NULL, start = 0) {
    if(is.null(dim(x))) {
        if(length(x) == 0) stop("'x' must have positive length")
        if(is.null(powers)) {
            i <- !is.na(x)
            xi <- x[i]
            if(length(xi) == 0) stop("all values in 'x' are NA")
            powers <- box.cox.powers(xi)$lambda
        }
        box.cox(x, powers, start)
    } else {
        if(nrow(x) == 0) stop("'x' has no rows")
        else if(ncol(x) == 0) stop("'x' has no columns")
        if(is.null(powers)) {
            i <- apply(x, 1, function(x) !any(is.na(x)))
            xi <- x[i,, drop=FALSE]
            if(nrow(xi) == 0) stop("all rows in 'x' contain NAs")
            powers <- box.cox.powers(xi)$lambda
        }
        # box.cox only works with vectors
        # if x has only one column, the result of mapply is a vector
        as.matrix(mapply(box.cox, x=x, p=powers, start=start))
    }
}

logratio <- function(x, type = c("clr","ilr","alr"), alrVar) {
    type <- match.arg(type)
    switch(type, clr=clr(x), ilr=ilr(x), alr=alr(x, alrVar))
}

clr <- function(x) {
    if(!inherits(x, c("matrix","data.frame"))) 
        stop("'x' must be a matrix or data.frame")
    else if(ncol(x) < 2) stop("'x' must be at least 2-dimensional")
    xgeom <- 10^apply(log10(x), 1, mean, na.rm=TRUE)
    x/xgeom
}

ilr <- function(x) {
    if(!inherits(x, c("matrix","data.frame"))) 
        stop("'x' must be a matrix or data.frame")
    else if(ncol(x) < 2) stop("'x' must be at least 2-dimensional")
    x.ilr <- matrix(NA, nrow=nrow(x), ncol=ncol(x)-1)
    for(i in 1:ncol(x.ilr)) {
        x.ilr[, i] <- sqrt((i)/(i+1))*
            log(((apply(x[, 1:i, drop=FALSE], 1, prod))^(1/i))/(x[, i+1]))
    }
    colnames(x.ilr) <- paste("ilrVar", 1:ncol(x.ilr), sep="")
    x.ilr
}

alr <- function(x, alrVar) {
    if(!inherits(x, c("matrix","data.frame"))) 
        stop("'x' must be a matrix or data.frame")
    else if(ncol(x) < 2) stop("'x' must be at least 2-dimensional")
    if(is.numeric(alrVar)) w <- alrVar
    else if(is.character(alrVar)) {
        w <- match(alrVar, colnames(x))
        if(is.na(w)) stop(gettextf("'x' does not contain variable '%s'",alrVar))
    }
    else stop("'alrVar' must be numeric or a character string")
    log10(x[, -w])-log10(x[, w])%*%t(rep(1, ncol(x)-1))
}
