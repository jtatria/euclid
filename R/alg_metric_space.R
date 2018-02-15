# Norms, inner products, distances ------------------------------------------------------------

# Definition of an Eucliudean vector space with L2 norm and dot product as inner product

#' Norms
#' 
#' Computes element-wise norms for all elements in the vector space v using the given exponent p.
#' 
#' Setting p to 2 will yield the euclidean norm, which is the norm you want is almost all cases.
#' 
#' @param v A matrix containing a vector space.
#' @param p An integer indicating the exponent to use for computing the norm.
#' 
#' @return A vector of scalars of lenght equal to the number of elements in v, with the value of 
#'         each element's norm, computed using the given exponent.
#'         
#' @export
vnorm <- function( v, p=2 ) {
    if( length( p ) != 1 ) error_not_scalar( "Non-scalar value for p in vnorm", p )
    if( iszero( v ) ) return( zero() )
    return( v_s_op( v, function( v_ ) vnorm_( v_, p_=p ) ) )
}
vnorm_ <- function( v, p_=2 ) v %>% `^`( p_ ) %>% sum() %>% `^`( 1 / p_ )

#' Inner product
#' 
#' Computes element-wise inner products between the elements in v and u. If the cross parameter is 
#' TRUE, the products are computed for all vector pairs, resulting in a matrix of inner product, 
#' i.e. a bilinear form or matrix product. If the cross parameter is FALSE, then v and u need to be 
#' isomorphic.
#' 
#' @param v     A matrix containing a vector space representation.
#' @param u     A matrix containing a vector space representation.
#' @param cross Logical. If TRUE, compute inner-products for all pairs. If FALSE, compute pair-wise 
#'              products.
#'              
#' @return If cross is FALSE, a scalar vector of the same length as the cardinality in v and u, 
#'         with the inner products for each corresponding (v_i,u_i pair. If TRUE, a matrix with the 
#'         inner product for all (v,u) pairs.
#'         
#' @export
vinnerp <- function( v, u, cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, vinnerp_, cross=cross ) )
}
vinnerp_ <- function( v, u ) sum( v * u )

#' Unit vector
#' 
#' Produces a vector of lenght 1 in the same direction as the given vector. I.e. vector 
#' "normalization".
#' 
#' @param v A matrix containing a vector space representation.
#' 
#' @return A matrix containing a vector space representation, with all elements in the same 
#'         direction as the elements in v, but with length 1.
#'         
#' @export
vunit <- function( v ) {
    if( iszero( v ) ) return( O( dct( v ) ) )
    return( v_v_op( v, vunit_ ) )
}
vunit_ <- function( v ) smul_( v, 1 / vnorm_( v ) )

#' Distance
#' 
#' Computes distances between the given vectors as induced by the norms computed with the given 
#' exponents.
#' 
#' @param v     A matrix containing a vector space representation.
#' @param u     A matrix containing a vector space representation. Defaults to the Origin.
#' @param p     Exponent for the computation of the norm that will be used to induce a distance. 
#'              Defaults to 2, yielding euclidean norms and distances.
#' @param cross Logical. If true, compute distances between all pairs of vectors, i.e. a distance 
#'              matrix. If FALSE, compute them across corresponding pairs. Defaults to FALSE.
#'              
#' @return If cross is TRUE, a distance matrix. If cross is FALSE, a scalar vector with the 
#'         distances between corresponding vectors in v and u.
#'         
#' @export
vdist <- function( v, u=O(), p=2, cross=FALSE ) {
    if( iszero( v ) && iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, function( v_, u_ ) vdist_( v_, u_, p=p ), cross=cross ) )
}
vdist_ <- function( v, u, p ) vnorm_( vdif_( v, u ), p )
