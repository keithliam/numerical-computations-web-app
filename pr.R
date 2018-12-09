GaussJordan <- function(mtrx, verbose=FALSE) {
    for(pivotRowCol in 1:dim(mtrx)[1]) {   # pivot row
        max = which.max(abs(t(mtrx[pivotRowCol:dim(mtrx)[1], pivotRowCol]))) + pivotRowCol - 1   # gets the row number of the max in pivot column
        if(mtrx[max, pivotRowCol] == 0) return("No unique solution exists.")    # stops the function if there is no solution
        tempRow = mtrx[pivotRowCol,]   # assigns row-with-max to temporary variable
        mtrx[pivotRowCol, ] = mtrx[max, ]    # assigns row-with-max to current row
        mtrx[max, ] = tempRow    # finishes swap by assigning current row to row-with-max
        mtrx[pivotRowCol, ] = mtrx[pivotRowCol, ] / mtrx[pivotRowCol, pivotRowCol]  # divides the pivot row by pivot element
        for(rowElim in (1:dim(mtrx)[1])[-pivotRowCol]) mtrx[rowElim, ] = mtrx[rowElim, ] - ((mtrx[rowElim, pivotRowCol] / mtrx[pivotRowCol, pivotRowCol]) * mtrx[pivotRowCol, ])  # eliminates all elements except diagonal elements
    }
    if(verbose) print(mtrx)
    return(unname(mtrx[ , dim(mtrx)[2]]))
}

PolynomialRegression <- function(depVec, indepVec, deg, verbose=FALSE) {
    if((length(depVec) != length(indepVec)) || deg < 1) return(NA)
    mtrx = matrix(NA, deg, deg + 1, dimnames=list(1:deg, append(paste("x", 0:(deg - 1), sep=""), c("RHS"))))    # create empty augmented coefficient matrix with degree as dimensions and add appropriate dimnames
    for(row in 1:deg) { # for each row in the augcoeffmatrix
        for(col in 1:deg) mtrx[row, col] = sum(indepVec ** (row + col - 2)) # compute values for each element of the matrix except RHS
        mtrx[row,"RHS"] = sum((indepVec ** (row - 1)) * depVec)    # compute values for RHS
    }
    if(verbose) print(mtrx)
    solutionSet = GaussJordan(mtrx, verbose)
    equation = paste("function(x)", paste(solutionSet," * x ** ", 0:(deg - 1), sep="", collapse=" + "))
    return(eval(parse(text = equation)))
}

getPr <- function(x, verbose=FALSE) {
    if(min(prMtrx[1,]) <= x & x <= max(prMtrx[1,])) {
        if(verbose) print(prEquation)
        print(prEquation(x))
    } else {
        print("Out of scope")
    }
}

readPrCSV <- function(filepath, degree, verbose=FALSE) {
    if(is.null(filepath)) return(NULL)
    vec = scan(filepath, what=numeric(),sep=",")
    returnMtrx = matrix(vec, nrow=(length(vec) / 2), ncol=2, byrow=TRUE, dimnames=list(1:(length(vec) / 2), c("X", "Y")))
    prMtrx <<- matrix(vec, nrow=2, ncol=(length(vec) / 2))
    prEquation <<- PolynomialRegression(prMtrx[, 2], prMtrx[, 1], degree, verbose)
    return(returnMtrx)
}

updatePrMtrx <- function(updatedMtrx, degree, verbose=FALSE) {
    prMtrx <<- t(updatedMtrx)
    prEquation <<- PolynomialRegression(updatedMtrx[, 2], updatedMtrx[, 1], degree, verbose)
}

prMtrx = NULL
prEquation = NULL