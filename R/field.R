# Field constants -----------------------------------------------------------------------------

#' The One
#' 
#' Multiplicative identity for the field of the reals.
#' 
#' @return The 1.
#' 
#' @export
one <- function() {
    return( 1.0 )
}

#' The Zero
#' 
#' Additive identity for the field of the reals.
#' 
#' @return The 0.
#' 
#' @export
zero <- function() {
    return( 0.0 )
}

#' Is v the 0-vector?
#' 
#' @param v A matrix representing a vector space.
#' 
#' @return \code{TRUE} if \code{v} is the 0-vector.
#' 
#' @export
iszero <- function( v ) {
    return( all( v == zero() ) )
}

#' The Origin
#' 
#' The 0-vector in the given dimensionality.
#' 
#' @param d Number of dimensions. Defaults to 2.
#' 
#' @return The 0-vector in the given dimensions.
#' 
#' @export
O <- function( d=2 ) {
    return( rep( zero(), d ) )
}

# TODO: add tests
