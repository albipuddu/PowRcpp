#define ARMA_DONT_PRINT_ERRORS
# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]


#include "beachmat/numeric_matrix.h"
#include "beachmat/integer_matrix.h"
//#include "functions.h"

#include <math.h>

using namespace arma;


//get the number of rows
template<typename T>
int get_nrow(const T& data){

    auto matrix_type=beachmat::find_sexp_type(data);

    if(matrix_type== INTSXP){
        auto final_matrix=beachmat::create_integer_matrix(data);
        //const size_t& nc = final_matrix->get_ncol();
        const size_t& nr = final_matrix->get_nrow();
        int n_row = nr;
        return n_row;

    }else if(matrix_type== REALSXP){
        auto final_matrix=beachmat::create_numeric_matrix(data);
        //const size_t& nc = final_matrix->get_ncol();
        const size_t& nr = final_matrix->get_nrow();
        int n_row = nr;
        return n_row;
    }else{

        return 0;
    }
}




//get the number of columns
template<typename T>
int get_ncol(const T& data){

    auto matrix_type=beachmat::find_sexp_type(data);

    if(matrix_type== INTSXP){
        auto final_matrix=beachmat::create_integer_matrix(data);
        const size_t& nc = final_matrix->get_ncol();
        //const size_t& nr = final_matrix->get_nrow();
        int n_col = nc;
        return n_col;

    }else if(matrix_type== REALSXP){
        auto final_matrix=beachmat::create_numeric_matrix(data);
        const size_t& nc = final_matrix->get_ncol();
        //const size_t& nr = final_matrix->get_nrow();
        int n_col = nc;
        return n_col;
    }else{

        return 0;
    }
}





//'
//' Mini_batch
//'
//' Mini-batch-k-means for both matrix and HDF5Matrix
//'
//'@param data numeric matrix or integer matrix or HDF5Matrix
//'@param k the number of clusters
//'@param batch_size the size of the mini batches
//'@return a list with the following attributes: centroids, WCSS_per_cluster, best_initialization, iters_per_initialization
//'@details
//'This function performs k-means clustering using mini batches.
//'
//'\strong{kmeans++}: kmeans++ initialization. Reference : http://theory.stanford.edu/~sergei/papers/kMeansPP-soda.pdf AND http://stackoverflow.com/questions/5466323/how-exactly-does-k-means-work
//'
//'\strong{random}: random selection of data rows as initial centroids
//'
//'@references
//'https://github.com/mlampros/ClusterR
//'
//' @export
// [[Rcpp::export]]
Rcpp::List powerRcpp(SEXP data, int k){

    int data_n_rows = get_nrow(data);
    int data_n_cols = get_ncol(data);


    Rcpp::NumericMatrix submat(data_n_rows, data_n_cols);
    Rcpp::NumericMatrix original(data_n_rows, data_n_cols);
    Rcpp::NumericVector tmp(data_n_rows);
    //Rcpp::NumericMatrix final1; //(data)
    auto final_matrix=beachmat::create_numeric_matrix(data);


    for ( unsigned int i = 0; i < data_n_rows; i++ ){
        final_matrix->get_row(i, tmp.begin());
        original.row(i)=tmp;
        submat.row(i) = pow(tmp,k);
    }


    return Rcpp::List::create(Rcpp::Named("Matrix Input") = original, Rcpp::Named("Exp") = k, Rcpp::Named("final_matrix") = submat );
}


