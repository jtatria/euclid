# Convenience wrappers ------------------------------------------------------------------------

#' Center object on O()
#' @export
center <- function( v ) {
    return( translate( v, -C( v ) ) )
}

#' Resize object
#' @export
resize <- function( v, S=( 1 / ( max( norms( v ) ) ) ), abs=FALSE ) {
    v %<>% rbind()
    o <- C( v )
    # to O
    v %<>% center()
    if( abs ) { # S is not a ratio, but how much to grow each
        v %<>% vadd( normalize( . ) %>% scalev( S ) )
    } else {
        v %<>% scalev( S )
    }
    # back to position
    v %<>% translate( o )
    return( v )
}

#' Rotate object
#' @export
rotate <- function( v, theta=0, cw=FALSE ) {
    if( theta == 0 ) return( v )
    v %<>% rbind()
    if( ncol( v ) != 2 ) stop( '2d rotation only' )
    o <- C( v )
    r <- matrix( c(
        cos( theta ), -sin( theta ),
        sin( theta ), cos( theta )
    ), byrow=TRUE, nrow=2 )
    v %<>% tcrossprod( r )
    v %<>% translate( -o )
    return( v )
}

#' Translate and rotate object
#' @export
position <- function( m, v=O( ncol( m ) ), theta=0 ) {
    m %<>% translate( v )
    #m %<>% rotate( theta )
    return( m )
}
