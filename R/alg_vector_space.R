# Vector spaces -------------------------------------------------------------------------------

# Vector addition, scalar multiplication, vector difference

#' Vector addition
#'
#' Adds the elements of \code{v} and \code{u}.
#' 
#' If \code{u} has a single element, this single element is added to all elements in \code{v}.
#' If \code{u} has the same number of elements as \code{v}, each element in \code{u} is added to 
#' the corresponding element in \code{v}.
#' If \code{u} has more than one element but not the same number of elements as \code{v}, a 
#' non-isomorphic error will be thrown.
#' 
#' NB: There is no \code{cross} parameeter because inner (v,u) -> w ops are not implemented yet.
#' See \link{vv_w_op}.
#' 
#' @param v A matrix representing a vector space.
#' @param u A matrix representing a vector space, with either one element or the same number of
#'          elements as \code{v}.
#'
#' @return A vector space isomorphic to \code{v}, with value equal to \code{v + u}.
#'
#' @export
vadd <- function( v, u ) { #, cross=FALSE ) {
    if( iszero( v ) ) return( u )
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vadd_, cross=FALSE ) ) # cross=cross ) )
}
vadd_ <- function( v, u ) v + u

#' Scalar multiplication
#' 
#' Multiplies the elements of \code{v} by the scalar(s) in \code{S}.
#' 
#' If \code{S} has a single element, all elements in \code{v} are multiplied by this value.
#' If \code{S} has the same number of elements as \code{v}, each element in \code{v} is multiplied 
#' by the corresponding value in \code{S}.
#' If \code{S} has more than one element but not the same number of elements as \code{v}, a 
#' non-isomorphic error will be thrown.
#' 
#' @param v A matrix representing a vector space.
#' @param S A vector of scalars, either of length 1 or equal to the number of elements in \code{v}.
#'
#' @return A vector space isomorphic to \code{v}, with value equal to \code{S*v}.
#' 
#' @export
smul <- function( v, S ) {
    if( length( S ) && S == one() ) return( v )
    if( length( S ) && S == zero() ) return( O( dct( v ) ) )
    return( vs_v_op( v, S, smul_ ) )
}
smul_ <- function( v, s ) v * s

#' Vector difference
#' 
#' Substracts the element or elements in \code{u} to the elements in \code{v}.
#' 
#' If \code{u} has a single element, this single element is substracted from all elements in 
#' \code{v}.
#' If \code{u} has the same number of elements as \code{v}, each element in \code{u} is subtracted 
#' from the corresponding element in \code{v}.
#' If \code{u} has more than one element but not the same number of elements as \code{v}, a 
#' non-isomorphic error will be thrown.
#' 
#' NB: There is no \code{cross} parameter because inner (v,u) -> w ops are not implemented yet.
#' See \link{vv_w_op}.
#' 
#' @param v A matrix representing a vector space.
#' @param u A matrix representing a vector space, with either one element or the same number of
#'          elements as \code{v}.
#'
#' @return A vector space isomorphic to \code{v}, with value equal to \code{v - u}.
#' 
#' @export
vdif <- function( v, u ) { #, cross=FALSE ) {
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vdif_, cross=FALSE ) ) # cross=cross ) )
}
vdif_ <- function( v, u ) v - u
