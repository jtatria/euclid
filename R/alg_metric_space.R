# Norms, inner products, distances ------------------------------------------------------------

# Definition of an Eucliudean vector space with L2 norm and dot product as inner product

#' Norms
#'
#' Computes element-wise norms for all elements in the vector space \code{v} using the given
#' exponent \code{p}.
#'
#' Setting \code{p} to 2 will yield the euclidean norm, which is the one you want in vistually all
#' cases.
#'
#' @param v A matrix representing a vector space.
#' @param p An integer indicating the exponent to use for computing the norm.
#'
#' @return A vector of scalars of lenght equal to the number of elements in \code{v}, with the
#'         value of each element's norm, computed using the given exponent.
#'
#' @export
vnorm <- function( v, p=2 ) {
    # TODO: add option for matrix norms?
    if( length( p ) != 1 ) error_not_scalar( "Non-scalar value for p in vnorm", p )
    if( iszero( v ) ) return( zero() )
    return( v_s_op( v, function( v_ ) vnorm_( v_, p_=p ) ) )
}
vnorm_ <- function( v, p_=2 ) v %>% `^`( p_ ) %>% sum() %>% `^`( 1 / p_ )

#' Inner product
#'
#' Computes element-wise inner products between the elements in \code{v} and \code{u}.
#'
#' If \code{u} has a single element, the inner products are computed for all elements in \code{v}
#' and the single element in \code{u}.
#' If \code{u} has the same number of elements as \code{v} and \code{cross} is \code{FALSE}, the
#' inner products are computed for all elements in \code{v} and the corresponding elements in
#' \code{u}.
#' If \code{cross} is \code{TRUE}, the inner products will be computed for all pairs between all
#' elements in \code{v} and \code{u}, i.e. compute the matrix product between them.
#' If \code{cross} is \code{FALSE} and \code{u} has more than one element but not the same number of
#' elements as \code{v}, a non-isomorphic error will be thrown.
#'
#' @param v     A matrix representing a vector space.
#' @param u     A matrix representing a vector space.
#' @param cross Logical. If TRUE, compute inner-products for all pairs. If FALSE, compute pair-wise
#'              products.
#'
#' @return If \code{cross} is \code{FALSE}, a scalar vector of the same length as elements in
#'         \code{v}, with the inner products for each element in \code{v} and either the single
#'         element or the corresponding element in \code{u}. If \code{cross} is \code{TRUE}, a
#'         matrix with as many rows and columsn as elements in \code{v} and \code{u}, respectively,
#'         equal to the matrix product between them.
#'
#' @export
vinnerp <- function( v, u, cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, vinnerp_, cross=cross ) )
}
vinnerp_ <- function( v, u ) sum( v * u )

#' Unit vector
#'
#' Produces a vector(s) of lenght 1 in the same direction as the vector(s) in \code{v}.
#' I.e. carries out vector "normalization".
#'
#' @param v A matrix representing a vector space.
#'
#' @return A matrix representing a vector space, with all elements in the same direction as the
#'         elements in \code{v}, but with magnitude 1.
#'
#' @export
vunit <- function( v ) {
    if( iszero( v ) ) return( O( dct( v ) ) )
    return( v_v_op( v, vunit_ ) )
}
# value of 1 hardcoded to avoid function call in lambda.
vunit_ <- function( v ) smul_( v, 1 / vnorm_( v ) )

#' Distance
#'
#' Computes element-wise distances between the elements in \code{v} and \code{u}.
#'
#' If \code{u} has a single element, the distances are computed between all elements in \code{v}
#' and the single element in \code{u}.
#' If \code{u} has the same number of elements as \code{v} and \code{cross} is \code{FALSE}, the
#' distances are computed for all elements in \code{v} and the corresponding elements in
#' \code{u}.
#' If \code{cross} is \code{TRUE}, the distances will be computed for all pairs between all
#' elements in \code{v} and \code{u}, i.e. compute a distance matrix.
#' If \code{cross} is \code{FALSE} and \code{u} has more than one element but not the same number of
#' elements as \code{v}, a non-isomorphic error will be thrown.
#'
#' @param v     A matrix representing a vector space.
#' @param u     A matrix representing a vector space. Defaults to the Origin (\link{O}), in which
#'              case the result will be equal to the norms of \code{v}.
#' @param p     Exponent for the computation of the norm that will be used to induce a distance.
#'              Defaults to 2, yielding euclidean norms and distances.
#' @param cross Logical. If \code{TRUE}, compute distances between all pairs of vectors, i.e. a
#'              distance matrix. If \code{FALSE}, compute them across corresponding pairs.
#'              Defaults to \code{FALSE}.
#'
#' @return If \code{cross} is \code{FALSE}, a scalar vector of the same length as elements in
#'         \code{v}, with the distance between each element in \code{v} and either the single
#'         element or the corresponding element in \code{u}.
#'         If \code{cross} is \code{TRUE}, a matrix with as many rows and columns as elements in
#'         \code{v} and \code{u}, respectively, equal to the distance matrix between \code{v} and
#'         \code{u}.
#'
#' @export
vdist <- function( v, u=O( dct( v ) ), p=2, cross=FALSE ) {
    if( iszero( v ) && iszero( u ) ) return( zero() )
    if( iszero( u ) ) return( vnorm( v ) )  # avoid computing an unecessary vector diff.
    return( vv_s_op( v, u, function( v_, u_ ) vdist_( v_, u_, p=p ), cross=cross ) )
}
vdist_ <- function( v, u, p ) vnorm_( vdif_( v, u ), p )
