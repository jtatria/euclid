#' An Axis.
#'
#' Unit vector along a dimension.
#'
#' @param d Total number of dimensions
#' @param a A dimension along which to orient the result. Defaults to 1 (i.e. the ordered axis,
#'          'east', 'to the right')
#'
#' @return A unit vector with no component in all dimensions save for the requested one.
#'
#' @export
A <- function( d=2, a=1 ) {
    if( a > d ) error_dimension( 'Dimension too large for basis', d, a )
    o <- O( d )
    o %<>% setd( a, 1 )
    return( o )
}

#' Alias for A
#' @rdname A
#' @param ... Arguments passed to \link{A}
#' @export
vaxis <- function( ... ) A( ... )

#' Center of m
#'
#' The location of the centroid of the elements contained in m.
#'
#' @param m A matrix describing a vector space
#'
#' @return A single vector giving the location of the centroid of the elements in m
#'
#' @export
C <- function( m ) {
    m %<>% V()
    if( vct( m ) == 1 ) return( m )
    return( colMeans( m ) %>% V() ) # TODO too fast to abstract.
}

#' North
#'
#' Axis pointing "up", i.e. the second dimension in the standard orientation, the abscissa axis.
#'
#' @param d Total number of dimensions
#'
#' @return A unit vector in the north direction
#'
#' @export
north <- function( d=2 ) {
    vaxis( d=d, a=2 )
}

#' East
#'
#' Axis pointing "right", i.e. the first dimension in the standard orientation, the ordered axis.
#'
#' @param d Total number of dimensions
#'
#' @return A unit vector in the east direction
#'
#' @export
east <- function( d=2 ) {
    vaxis( d=d, a=1 )
}

#' South
#'
#' Axis pointing "down", i.e. the second dimension in the standard orientation, the abscissa axis
#' but in the opposite direction.
#'
#' @param d Total number of dimensions
#'
#' @return A unit vector in the south direction
#'
#' @export
south <- function( d=2 ) {
    return( north( d=d ) * -1 )
}

#' West
#'
#' Axis pointing "left". i.e. the first dimension in the standard orientation. the ordered axis, in
#' the opposite direction.
#'
#' @param d Total number of dimensions
#'
#' @return A unit vector in the west direction
#'
#' @export
west <- function( d=2 ) {
    return( east() * -1 )
}

#' Vector from C( m ) (or the given point to use as origin) to the furthest point in m
#'
#' Return the "heading" of the cloud of points in m, the vector from its centroid to the point
#' furthest from the centroid. This is based exclusively on inner products and does not take into
#' account the distribution of mass in the cloud.
#'
#' @param m A matrix containing a vector space
#' @param o A location for the origin. Defaults to the centroid of m.
#' @param scalar Logical. If TRUE, return the magnitude of the vector.
#'
#' @return If scalar is true, the magnitude of the vector from the given origin to the the furthest
#'         point. If scalar is FALSE, the vector itself.
#'
#' @export
vhead <- function( m, o=C( m ), scalar=FALSE ) {
    h <-  m[ which.max( vdist( m, C( m ) ) ), ] %>% V()
    if( scalar ) return( vdist( h, o ) )
    return( vdif( h, o ) )
}

#' Major axis
#'
#' The dimension along which one would define the "length" of the given object.
#'
#' Vector along the dimension of most variation, i.e. the direction defined by a line between the
#' elements in the pair with maximum distance, also equal to the dimension across m that presents
#' the maximal range.
#'
#' @param m A matrix representing a vector space.
#'
#' @return A vector along the widest ranged dimension.
#'
#' @export
axis_major <- function( m ) {
    m %<>% V()
    if( vct( m ) == 1 ) return( O( dct( m ) ) )
    if( vct( m ) == 2 ) return( vdif(  m[ 1 ,],  m[ 2 ,] ) )
    ch <- p_hull( m )
    dist <- vdist( ch, ch, cross=TRUE )
    far <- which( dist == max( dist ), arr.ind=TRUE )[1,]
    h <- ch[far[1],]
    t <- ch[far[2],]
    return( vdif( h, t ) )
}

#' Minor axis
#'
#' The dimension along which one would define the "width" of the given object.
#'
#' A vector orthogonal to the vector describing the dimension of greatest range.
#'
#' @param m A matrix representing a vector space
#'
#' @return A vector along a dimension orthogonal to the widest ranged dimension.
#'
#' @export
axis_minor <- function( m ) {
    m %<>% V()
    if( vct( m ) <= 2 ) return( O( dct( m ) ) )
    a <- vorth( axis_major( m ) )
    l <-  m[ which.max( vinnerp( m, a ) ), ] %>% V()
    r <-  m[ which.min( vinnerp( m, l ) ), ] %>% V()
    return( vdif( vproj( l, a ), vproj( r, a ) ) )
}


