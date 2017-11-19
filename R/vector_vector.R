# Vector-Vector ops ---------------------------------------------------------------------------

#' ( v, u ) -> v + u
#' @export
vadd <- function( v, u ) {
    if( iszero( v ) ) return( u )
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vadd_ ) )
}
vadd_ <- function( v, u ) v + u

#' ( v, u ) -> v + u
#' @rdname vadd
translate <- function( v, u ) vadd( v, u )

#' ( v, u ) -> v - u
#' @export
vdif <- function( v, u ) {
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vdif_ ) )
}
vdif_ <- function( v, u ) v - u

#' ( v, s ) -> v * s
#' @export
scalev <- function( v, S ) {
    if( length( S ) == 1 ) return( v_v_op( v, function( v_ ) scalev_( v_, S ) ) )
    if( is.vector( S ) && length( S ) == 2 ) return( cwise( m0=v, m1=S, f_=function( v_, s_ ) scalev_( v_, s_ ) ) )
    return( rwise( m0=v, m1=S, function( v_, u_ ) scalev_( v_, u_ ) ) )
}
scalev_ <- function( v, s ) ( v * s )

#' Project v on u
#' @export
projv <- function( v, u, scalar=FALSE ) {
    if( ncol( v ) != ncol( u ) ) error_dimension( 'Wrong dimensions in projection', v, u )
    if( iszero( v ) || iszero( u ) ) return( O( ncol( v ) ) )
    if( scalar ) return( vv_s_op( projv_s_( v, u ) ) )
    return( projv_v_( v, u ) )
}
projv_v_ <- function( v, u ) ( dot_( v, u ) / norms_( u )^2 ) * u
projv_s_ <- function( v, u ) norms_( projv_v_( v, u  ) )

#' Orth to v
#' @export
orth <- function( v, ccw=TRUE ) {
    if( iszero( v ) ) return( v )
    if( ccw ) return( v_v_op( v, orth_ ) )
    return( v_v_op( v * -1, orth_ ) )
}
orth_ <- function( v ) c( -v[2], v[1] )

#' Normal for v
#' @export
normalize <- function( v ) {
    return( v_v_op( v, normalize_ ) )
}
normalize_ <- function( v ) scalev_( v, 1 / norms_( v ) )
