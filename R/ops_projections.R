# Vector-Vector ops ---------------------------------------------------------------------------

#' Vector projection
#' @export
vproj <- function( v, u, scalar=FALSE, cross=FALSE ) {
    if( dct( v ) != dct( u ) ) error_dimension( 'Wrong dimensions in projection', v, u )
    if( iszero( v ) || iszero( u ) ) return( if( scalar ) zero() else O( dct( v ) ) )
    if( scalar ) return( vv_s_op( v, u, vproj_s_, cross=cross ) )
    return( vv_v_op( v, u, vproj_v_, cross=cross ) )
}
vproj_s_ <- function( v, u ) vinnerp_( v, u ) / ( vnorm_( u ) * vnorm_( u ) )
vproj_v_ <- function( v, u ) smul_( u, vproj_s_( v, u ) )

#' Vector rejection
#' @export
vrej <- function( v, u, scalar=FALSE, cross=FALSE ) {
    if( dct( v ) != dct( u ) ) error_dimension( 'Wrong dimensions in rejection', v, u )
    if( iszero( v ) || iszero( u ) ) return( if( scalar ) zero() else O( dct( v ) ) )
    if( scalar ) return( vv_s_op( v, u, vrej_s_, cross=FALSE ) )
    return( vv_v_op( v, u, vrej_v_, cross=FALSE ) )
}
vrej_v_ <- function( v, u ) vdif_( v, vproj_v_( v, u ) )
vrej_s_ <- function( v, u ) vrej_v_( v, u ) %>% vnorm_()

#' Vector normal
#' @export
vorth <- function( v, cw=FALSE, a=1 ) {
    if( iszero( v ) ) return( v )
    r <- if( !cw ) rotate( v, pi / 2 ) else rotate( v, ( 3 * pi ) / 2 )
    return( r )
} # TODO: find a more general way of finding normals

# Alias for vorth
#' @rdname vorth
#' @export
normal <- function( ... ) return( vorth( ... ) )

