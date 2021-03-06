#' blocksize
#'
#' Return the maximum number of rows to use based on the amount of ram memory
#'
#' @param data   numeric matrix or integer matrix or HDF5Matrix
#'
#' @importFrom benchmarkme get_ram
#'
#' @return  It returns a value of the maximum number of rows
#'
#'@export
#'
blocksize<-function(data){
  result<-min(floor(as.numeric(get_ram())/(2*8*nrow(data))), ncol(data))
  result
}
