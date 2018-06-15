#' Apply linear mapping
#'
#' Apply the given linear mapping to the given matrix representing a vector space. A linear mapping
#' is given as a matrix with as many rows as dimensions in the source vector space, and as many
#' columns as dimensions in the target vector space. In most cases, vector spaces are mapped to
#' themselves, in which case the linear mapping is described as a square matrix.
#'
#' @param v   A matrix describing a vector space.
#' @param map A matrix describing a linear mapping to apply on v
#'
#' @return A matrix describing a vector space equal to the application of the given mapping to the
#'         given input vector space. I.e. the matrix product map * v.
#'
#' @export
lmap <- function( v, map ) {
    v %<>% V()
    if( vct( map ) != dct( v ) ) error_dimension( 'Wrong dimension for linear map', v, map )
    r <- v_w_op( v, function( v_ ) lmap_( v_, map ), d=dct( map ) )
    return( r %>% t() )
}
# > rbenchmark::benchmark( r %*% t( v ), tcrossprod( r, v ), replications = 100000 )
#              test replications elapsed relative user.self sys.self user.child sys.child
# 1       r %*% t(v)       100000   0.936    1.000     0.936    0.004          0         0
# 2 tcrossprod(r, v)       100000   1.409    1.505     1.408    0.000          0         0
lmap_ <- function( v, map ) return( map %*% t ( v ) )

#' Rotation
#'
#' Produce a linear mapping that will rotate a vector space by the given angle.
#'
#' This function operates exclusively in two dimensions for now. This should change soon.
#'
#' @param theta An angle of rotation
#'
#' @return a matrix describing a linear mapping that will rotate a vector space by the given angle.
#'
#' @export
rotation <- function( theta ) {
    ct <- cos( theta )
    st <- sin( theta )
    R <- matrix( c( ct, -st, st, ct ), byrow=TRUE, nrow=2 )
    return( R )
} # TODO implement Clifford algebras for generalized rotations

