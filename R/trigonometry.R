# Trigonometric functions ---------------------------------------------------------------------
# SOHCAHTOA

#' Cosine
#' @export
cosf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, cos_, cross=cross ) )
}
cos_ <- function( v, u ) vinnerp_( v, u ) / ( vnorm_( v ) * vnorm_( u ) )

#' Sine
#' @export
sinf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, sin_, cross=cross ) )
}
# TODO: refactor to use crossproduct
sin_ <- function( v, u ) cos_( v, u ) %>% { if( . != zero() ) sign( . ) * sqrt( 1 - .^2 ) }

#' Tangent
#' @export
#' @export
tanf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, tan_, cross=cross ) )
}
tan_ <- function( v, u ) sin_( v, u ) / cos_( v, u )

#' Cosecant
#' @export
cscf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( 0 )
    return( vv_s_op( v, u ), csc_, cross=cross )
}
csc_ <- function( v, u ) 1 / sin_( v, u )

#' Secant
#' @export
secf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( 0 )
    return( vv_s_op( v, u ), sec_, cross=cross )
}
sec_ <- function( v, u ) 1 / cos_( v, u )

#' Cotangent
#' @export
cotf <- function( v, u=east( dct( v ) ), cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( 0 )
    return( vv_s_op( v, u ), cot_, cross=cross )
}
cot_ <- function( v, u ) 1 / tan_( v, u )

#' Angle between two vectors
#' @export
theta <- function( v, u=east( dct( v ) ), cw=FALSE, deg=FALSE, cross=FALSE ) {
    if( iszero( v ) && iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, function( v_, u_ ) theta_( v_, u_, cw=cw, deg=deg ), cross=cross ) )
}
theta_ <- function( v, u, cw=FALSE, deg=FALSE ) {
    add <- vproj( v, u, scalar=TRUE )
    opp <- vproj( v, vorth( u ), scalar=TRUE )
    t  <- atan( opp / add )
    t <- if( add < 0 ) ( if( opp < 0 ) t - pi else t + pi ) else t
    return( t )
} # Angle by strict Pythagoras

