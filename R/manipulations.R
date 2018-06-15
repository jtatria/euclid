# Convenience wrappers ------------------------------------------------------------------------

#' Translate object
#'
#' Translates the elements in \code{v} according to the displacement vectors in \code{u}.
#'
#' If \code{u} contains only one element, every element in \code{v} will de displaced by it.
#' If it contains as many elements as there are elements in \code{v}, each element in \code{v} will
#' be displaced by the corresponding element in \code{u}.
#' If \code{u} has more than one element but not the same number of elements as \code{v}, a
#' non-isomorphic error will be thrown.
#'
#' @param v A matrix representing a vector space.
#' @param u A matrix representing a vector space with displacement vectors, containing either 1
#'          element or as many elements as there are elements in \code{v}.
#'
#' @return A matrix representating a vector space resulting from the displacement of \code{v} by
#'         \code{u}.
#'
#' @export
translate <- function( v, u ) {
    v %<>% V()
    return( vadd( v, u ) )
}

#' Center object
#'
#' Center the object described by the elements in \code{v} on the origin.
#'
#' Internally, this functions computes the centroid ( see \link{C}) of the given shape and
#' translates all points in \code{v} according to the inverse of its centroid, effectivelly pulling
#' the entire set such that the new centroid is over the origin.
#'
#' @param v A matrix representing a vector space.
#'
#' @return A matrix representing a vector space equal to the displacement of \code{v} by the
#'         inverse of its centroid.
#'
#' @export
center <- function( v ) {
    v %<>% V()
    return( translate( v, -C( v ) ) )
}

#' Resize object
#'
#' Resize the object described by the elements in \code{v} according to the scalar(s) given in
#' \code{S}.
#' If \code{abs} is \code{FALSE}, \code{S} is interpreted as a scaling factor.
#' If \code{abs} is \code{TRUE}, it is interpreted as a constant delta by which to grow each
#' element in \code{v}.
#'
#' If \code{S} is \code{NULL}, the object will be resized by a relative factor equal to
#' \code{1 / max( norm( v ) )}, making the entire space described by \code{v} fit into a -1,1 box
#' in every direction.
#'
#' @param v   A matrix representing a vector space.
#' @param S   A vector of scalars to use as factors or deltas. Defaults to 1 / max( norm( v ) ).
#' @param abs Logical. Interpret \code{S} as a constant delta insted of scaling factor.
#'            Defaults to \code{FALSE}.
#'
#' @return A matrix describing a vector space equal to the resizing of \code{v}.
#'
#' @export
resize <- function( v, S=NULL, abs=FALSE ) {
    v %<>% V()
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
#'
#' Rotate the object described by the elements in \code{v} according to the angle \code{theta}
#' (in radians).
#'
#' @param m     A matrix representing a vector space.
#' @param theta An angle of rotation, in radians.
#' @param cw    Logical. Rotate clockwise. Defaults to \code{FALSE} (rotate counter-clockwise).
#'
#' @return A matrix describing a vector space equal to the rotation of \code{m}.
#'
#' @export
rotate <- function( m, theta=zero(), cw=FALSE ) {
    m %<>% V()
    if( dct( m ) > 2 ) stop( 'TODO: implement rotations properly' )
    if( theta == zero() ) return( m )
    R <- rotation( theta )
    if( cw ) R <- t( R )
    return( lmap( m, R ) )
}

#' Position object by translation and rotation.
#'
#' Wrapper for translate and rotate.
#'
#' @param m A matrix describing a vector space.
#' @param v A displacement vector.
#' @param t A rotation angle
#'
#' @return A matrix describing a vector space equal to the rotation and resizing of m
#'
#' @export
position <- function( m, v=vdif( O( dct( m ) ), C( m ) ), t=theta( v, vhead( m ) ) ) {
    m %<>%V()
    m %<>% rotate( t )
    m %<>% translate( v )
    return( m )
}
