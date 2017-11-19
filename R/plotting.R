# Plottiing functions -------------------------------------------------------------------------

chart <- function( ..., scale=TRUE, asp=1, axes=TRUE, ylab=NA, xlab=NA, par=NULL ) {
    m <- rbind( ... )
    if( !is.null( par ) ) par( par )
    plot( m, type='n', asp=asp, axes=FALSE, xlab=xlab, ylab=ylab )
    par( xpd=NA )
    if( scale ) {
        grid( 100, 100, lty=1, lwd=.1 )
        grid( 10,  10,  lty=1, lwd=.5 )
    }
    if( axes ) {
        axis( 1, xpd=NA, pos=0 )
        axis( 2, xpd=NA, pos=0 )
    }
}

draw_vec_ <- function( v, o, ... ) {
    if( norms( v - o ) > 1/1000 * xinch() ) {
        arrows( o[1], o[2], v[1], v[2], ... )
    }
    return( 0 )
}

draw_vec <- function( v, o=O( ncol( v ) ), ... ) {
    v %<>% rbind()
    o %<>% rbind()
    if( nrow( o ) == 1 ) {
        null <- v_s_op( v, function( v_ ) draw_vec_( v_, o, ... ) )
    } else if( nrow( o ) == nrow( v ) ) {
        null <- vv_s_op( v, function( v_, o_ ) draw_vec_( v_, o_, ... ) )
    }
    invisible( null )
}

draw_seg <- function( p0, p1, lty=2, ... ) {
    draw_vec( p1, p0, length=0, lty=lty, ... )
}
