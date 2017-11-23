# Vector spaces -------------------------------------------------------------------------------

#' ( v, u ) -> v + u
#' @export
vadd <- function( v, u, cross=FALSE ) {
    if( iszero( v ) ) return( u )
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vadd_, cross=cross ) )
}
vadd_ <- function( v, u ) v + u

#' ( v, u ) -> v - u
#' @export
vdif <- function( v, u, cross=FALSE ) {
    if( iszero( u ) ) return( v )
    return( vv_v_op( v, u, vdif_, cross=cross ) )
}
vdif_ <- function( v, u ) v - u

#' ( v, s ) -> v * s
#' @export
smul <- function( v, S ) {
    if( length( S ) && S == one() ) return( v )
    if( length( S ) && S == zero() ) return( O( dct( v ) ) )
    return( vs_v_op( v, S, smul_ ) )
}
smul_ <- function( v, s ) v * s
