# Plottiing functions -------------------------------------------------------------------------

#' Create a chart for thr given objects
#'
#' Set up a new plotting surface of the correct size and coordinates to comfortably include all of
#' the given objects.
#'
#' @param ...   Objects to chart
#' @param scale Logical. If TRUE, add a grid with major and minor coordinate lines.
#'              Defaults to TRUE.
#' @param asp   Logical. If TRUE, ensure the axes are drawn on a 1-1 ratio (i.e. iwth equal size).
#'              Defaults to TRUE.
#' @param axes  Logical. If TRUE, include axes over the origin.
#'              Defaults to TRUE.
#' @param ylab  Label for the abscissa axis. Default to NA (no label)
#' @param xlab  Label for the ordered axis. Default to NA (no label)
#' @param par   Additional graphical parameters to overide any other setting. Defaults to NULL.
#'
#' @export
chart <- function( ..., scale=TRUE, asp=1, axes=TRUE, ylab=NA, xlab=NA, par=NULL ) {
    m <- rbind( ... )
    if( !is.null( par ) ) graphics::par( par )
    graphics::plot( m, type='n', asp=asp, axes=FALSE, xlab=xlab, ylab=ylab )
    graphics::par( xpd=NA )
    if( scale ) {
        graphics::grid( 100, 100, lty=1, lwd=.1 )
        graphics::grid( 10,  10,  lty=1, lwd=.5 )
    }
    if( axes ) {
        graphics::axis( 1, xpd=NA, pos=0 )
        graphics::axis( 2, xpd=NA, pos=0 )
    }
}

#' Draw segments
#'
#' Draws segmented lines between the given p0 and p1 objects. If p0 contains one
#' element, all lines  will be traced between this single element and each element in p1. If p0
#' contains as many elements as p1, an arrow will be drawn between each element in p0 and its
#' correspondoing element in p1. If p0 has more than one element  but not the same number of
#' elements as p1, a non-isomorphic error will be thrown.
#'
#' @param p1  A set of points.
#' @param p0  A set of points. Defaults to the recyprocal of elements in p1.
#' @param lty An integer indicating a line style as a graphical parameter (see par). Defaults to 2.
#' @param ... Additional parameters for the base graphics drawing function.
#'
#' @export
draw_seg <- function( p1, p0=smul( p1, -one() ), lty=2, ... ) {
    draw_vec( p1, p0, length=0, lty=lty, ... )
}

#' Draw vectors
#'
#' Draws arrrows pointing to the elements in v from the point or points in o. If o contains one
#' element, all arrwos will be traced between this single element and each element in v. If o
#' contains as many elements as v, an arrow will be drawn between each element in o and its
#' correspondoing element in v. If o has more than one element  but not the same number of
#' elements as v, a non-isomorphic error will be thrown.
#'
#' @param v   A set of points.
#' @param o   A point or set of points to use as origin. Defaults to the origin: O( dct( v ) ).
#' @param ... Additional parameters for the base graphics drawing function.
#'
#' @export
draw_vec <- function( v, o=O( dct( v ) ), ... ) {
    v %<>% rbind()
    o %<>% rbind()
    if( nrow( o ) == 1 ) {
        null <- v_s_op( v, function( v_ ) draw_vec_( v_, o, ... ) )
    } else if( vct( o ) == vct( v ) ) {
        null <- vv_s_op( v, o, function( v_, o_ ) draw_vec_( v_, o_, ... ) )
    }
    invisible( null )
}

draw_vec_ <- function( v, o, ... ) {
    if( vnorm( v - o ) > 1/1000 * graphics::xinch() ) {
        graphics::arrows( o[1], o[2], v[1], v[2], ... )
    }
    return( 0 )
}

