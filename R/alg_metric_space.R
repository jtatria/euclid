# Norms, inner products, distances ------------------------------------------------------------

# Definition of an Eucliudean vector space with L2 norm and dot product as inner product

#' Norms
#' @export
vnorm <- function( v, p=2 ) {
    if( length( p ) != 1 ) error_not_scalar( "Non-scalar value for p in vnorm", p )
    if( iszero( v ) ) return( zero() )
    return( v_s_op( v, function( v_ ) vnorm_( v_, p_=p ) ) )
}
vnorm_ <- function( v, p_=2 ) v %>% `^`( p_ ) %>% sum() %>% `^`( 1 / p_ )

#' Inner product
#' @export
vinnerp <- function( v, u, cross=FALSE ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, vinnerp_, cross=cross ) )
}
vinnerp_ <- function( v, u ) sum( v * u )

#' Unit vector
#' @export
vunit <- function( v ) {
    if( iszero( v ) ) return( O( dct( v ) ) )
    return( v_v_op( v, vunit_ ) )
}
vunit_ <- function( v ) smul_( v, 1 / vnorm_( v ) )

#' Distance
#' @export
vdist <- function( v, u=O(), p=2, cross=FALSE ) {
    if( iszero( v ) && iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, function( v_, u_ ) vdist_( v_, u_, p=p ), cross=cross ) )
}
vdist_ <- function( v, u, p ) vnorm_( vdif_( v, u ), p )
