#' @importFrom Rcpp sourceCpp
#' @useDynLib POWER, .registration = TRUE
#'
NULL


#' @title Matrix power
#'
#' @description This is an implementation of a package where return the power
#'   between a matrix and a value choose by user.
#'
#'
#' @details The implementation admits a matrix and a value.
#'   In the end, it returns the power between the matrix and the value.
#'   Matrix, HDF5Matrix and DelayMatrix are not allow and the system returns an error.


#'@rdname POWER
#'@importFrom methods is
#'@export
#'@importClassesFrom DelayedArray DelayedMatrix
#'@param x the matrix
#'@param k power value (default k=1)

#'@return a list with the following attributes: input matrix, power value,
#'  power matrix
#'
#'
#'
#'@references https://github.com/mlampros/ClusterR
#'
#'
#'
setMethod(
    "POWER",
    signature = signature(x ="ANY"),
    definition = function(x, k=1)
    {
        x<- x*1.0
        if(!is(x, "matrix") & !is(x, "Matrix") & !is(x, "HDF5Matrix") &
           !is(x, "DelayedMatrix")) {

            stop("x is not of a supported type")

        } else {

            fit <- powerRcpp(x, k)

        }

        return(fit)
    })

