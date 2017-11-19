# Field constants -----------------------------------------------------------------------------

#' The One
#' @export
one <- function() {
    return( 1.0 )
}

#' The Zero
#' @export
zero <- function() {
    return( 0.0 )
}

#' Is The Zero
#' @export
iszero <- function( v ) {
    return( all( v == zero() ) )
}

# The Origin
#' @export
O <- function( d=2 ) {
    return( rep( zero(), d ) )
}
