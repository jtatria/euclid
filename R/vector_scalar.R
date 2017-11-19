# Vector-Scalar ops ---------------------------------------------------------------------------

#' Norms
#' @export
norms <- function( v, p=2 ) {
    if( length( p ) != 1 ) error_not_scalar( "Non-scalar value for p in norms", p )
    if( iszero( v ) ) return( zero() )
    return( v_s_op( v, function( v_ ) norms_( v_, p_=p ) ) )
}
norms_ <- function( v, p_=2 ) v %>% `^`( p_ ) %>% sum() %>% `^`( 1 / p_ )

#' Dot product
#' @export
dot <- function( v, u ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, dot_ ) )
}
dot_ <- function( v, u ) sum( v * u )

#' Distances
#' @export
distf <- function( v, u=O(), p=2 ) {
    if( iszero( v ) && iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, function( v_, u_ ) dist_( v_, u_, p=p ) ) )
}
dist_ <- function( v, u, p ) norms_( v - u, p )

#' Cosine
#' @export
cosf <- function( v, u ) {
    if( iszero( v ) || iszero( u ) ) return( zero() )
    return( vv_s_op( v, u, cos_ ) )
}
cos_ <- function( v, u ) dot_( v, u ) / norms_( v ) * norms_( u )

#' Angle between v and u
#' @export
theta <- function( v, u=O(), ccw=FALSE, deg=FALSE ) {
    if( is.zero( v ) && is.zero( u ) ) return( zero() )
    return( vv_s_op( v, u, theta_ ) )
}
theta_ <- function( v, u, deg=FALSE ) { r <- atan2( v[2], v[1] ) - atan2( u[2], u[1] ); if( deg ) rad2deg( r ) else r }
