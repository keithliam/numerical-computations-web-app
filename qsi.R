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

QuadraticSplineInterpolation <- function(mtrx, verbose=FALSE) {
    cols = dim(mtrx)[1]
    rows = dim(mtrx)[2]
    noOfEquations = (3 * rows) - 3
    augCoeffMatrix = matrix(0, noOfEquations - 1, noOfEquations)
    for(num in 2:(rows - 1)) {
        vec = numeric(noOfEquations)
        if(num > 2) vec[(3 * num) - 6] = mtrx[1, num] ** 2  # a,n-1
        vec[(3 * num) - 5] = mtrx[1, num]                   # b,n-1
        vec[(3 * num) - 4] = 1                              # c,n-1
        vec[noOfEquations] = mtrx[2, num]
        augCoeffMatrix[(2 * num) - 3,] = vec
        
        vec = numeric(noOfEquations)
        vec[(3 * num) - 3] = mtrx[1, num] ** 2              # a,n
        vec[(3 * num) - 2] = mtrx[1, num]                   # b,n
        vec[(3 * num) - 1] = 1                              # c,n
        vec[noOfEquations] = mtrx[2, num]
        augCoeffMatrix[(2 * num) - 2,] = vec
        
        vec = numeric(noOfEquations)
        if(num > 2) vec[(3 * num) - 6] = mtrx[1, num] * 2   # a,n-1
        vec[(3 * num) - 5] = 1                              # b,n-1
        vec[(3 * num) - 3] = -(mtrx[1, num] * 2)            # a,n
        vec[(3 * num) - 2] = -1                             # b,n
        augCoeffMatrix[(2 * rows) - 3 + num,] = vec
    }
    vec = numeric(noOfEquations)
    vec[1] = mtrx[1, 1]
    vec[2] = 1
    vec[noOfEquations] = mtrx[2, 1]
    augCoeffMatrix[(2 * rows) - 3,] = vec
    
    vec = numeric(noOfEquations)
    vec[noOfEquations - 3] = mtrx[1, rows] ** 2
    vec[noOfEquations - 2] = mtrx[1, rows]
    vec[noOfEquations - 1] = 1
    vec[noOfEquations] = mtrx[2, rows]
    augCoeffMatrix[(2 * rows) - 2,] = vec
    
    if(verbose) print(augCoeffMatrix)
    solution = GaussJordan(augCoeffMatrix, verbose)
    solution = append(0, solution)
    
    equations = c()
    for(num in 1:(rows - 1)) {
        equation = paste('function(x){', solution[(3 * num) - 2], '* (x ** 2) +', solution[(3 * num) - 1], '* x +', solution[3 * num], '}')
        equations = append(equations, eval(parse(text = equation)))
    }
    equations
}

getQSI <- function(x, verbose=FALSE, forPlot=FALSE){
    rows = dim(mtrx)[2]
    interval = NULL
    for(num in 1:(rows - 1)) {
        if(mtrx[1, num] <= x & x <= mtrx[1, (num + 1)]) {
            interval = num
            break
        }
    }
    if(verbose) print(equations[[interval]])
    if(!is.null(interval) && !forPlot) print(equations[[interval]](x))
    else if(!is.null(interval) && forPlot) return(equations[[interval]](x))
    else print("Out of scope")
}

readQsiCSV <- function(filepath, verbose=FALSE) {
    if(is.null(filepath)) return(NULL)
    vec = scan(filepath, what=numeric(),sep=",")
    returnMtrx = matrix(vec, nrow=(length(vec) / 2), ncol=2, byrow=TRUE, dimnames=list(1:(length(vec) / 2), c("X", "Y")))
    if(anyDuplicated(returnMtrx[,1]) > 0) return(TRUE)
    mtrx <<- matrix(vec, nrow=2, ncol=(length(vec) / 2))
    return(FALSE)
}

updateQsiMtrx <- function(updatedMtrx, verbose=FALSE) {
    mtrx <<- t(updatedMtrx)
    equations <<- QuadraticSplineInterpolation(mtrx, verbose)
}

mtrx = NULL
equations = NULL