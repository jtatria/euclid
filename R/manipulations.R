# Convenience wrappers ------------------------------------------------------------------------

#' Translate object
#' @export
translate <- function( v, u ) {
    return( vadd( v, u ) )
}

#' Center object on O()
#' @export
center <- function( v ) {
    return( translate( v, -C( v ) ) )
}

#' Resize object
#' @export
resize <- function( v, S=NULL, abs=FALSE ) {
    if( iszero( v ) ) return( v )
    S <- if( is.null( S ) ) 1 / max( vnorm( v ) ) else S
    if( !is.null( S ) && !abs && all( S == one() ) ) return( v )
    if( !is.null( S ) && abs && iszero( S ) ) return( v )
    v %<>% V()
    o <- C( v )
    # to O
    v %<>% center()
    if( abs ) { # S is not a ratio, but how much to grow each
        v %<>% vadd( vunit( . ) %>% smul( S ) )
    } else {
        v %<>% smul( S )
    }
    # back to position
    v %<>% translate( o )
    return( v )
}

#' Rotate object
#' @export
rotate <- function( m, theta=zero(), cw=FALSE ) {
    if( dct( m ) > 2 ) stop( 'TODO: implement rotations properly' )
    if( theta == zero() ) return( v )
    R <- rotation( theta )
    if( cw ) R <- t( R )
    return( lmap( m, R ) )
}

#' Position object by translation and rotation
#' @export
position <- function( m, v=vdif( O( dct( m ) ), C( m ) ), t=theta( v, vhead( m ) ) ) {
    m %<>% rotate( t )
    m %<>% translate( v )
    return( m )
}
