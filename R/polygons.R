#' Convex hull
#'
#' Extract a convex hull around the given point cloud.
#'
#' @param m   A matrix describing a vector space.
#' @param idx Logical. If TRUE, return the indices in m of the points in the convez hull. Defaults
#'            to FALSE: return the points themselves.
#'
#' @return If idx is FALSE, a subset of points in m lying on the convex hull around m. If idx is
#'         TRUE, the indices of points in m lying on the convex hull around m.
#'
#' @export
#' @importFrom grDevices chull
p_hull <- function( m, idx=FALSE ) {
    ch <- chull( m ) %>% rev()
    if( idx ) return( ch )
    return( m[ ch, ] )
}

#' Is polygon P stored in counterclockwise order?
#'
#' Returns TRUE if the given set of points describing a polygon are stored in counterclockwise
#' order (i.e. standard orientation), FALSE otherwise.
#'
#' @param P A matrix containing points describing a polygon.
#'
#' @return TRUE if the given polygon is stored in counterclockwise order, FALSE otherwise.
#'
#' @export
p_is_ccw <- function( P ) { # TODO: generalize
    return( sum( diff( chull( P ) ) ) < 0 )
    # ccw <- 0
    # for( i in 1:nrow( P ) ) {
    #     j <- ( i %% nrow( P ) ) + 1
    #     p0 <- P[i,]; p1 <- P[j,]
    #     ccw %<>% `+`( p1[1] - p0[1] ) * ( p1[2] + p0[2] )
    # }
    # return( ccw < 0 )
}

#' Ensure points in P are stored in counterclockwise order
#'
#' Naively store the given points describing a polygon in counterclockwise order.
#' This function assumes that the given object P is in fact a collection of points that describe a
#' polygon, sorted in order, and simply reverses the order if the order is not counterclockwise. In
#'  other words, this function assumes that P is most likely a polygon returned by some other
#'  function and merely enforces the storage convention used in this package for polygons.
#'
#' This function is mostly a convenience wrapper to facilitate storage order check and sanitization
#' for polygons. I.e. calling P %<>% p_to_ccw() checks and, if needed, sorts the given P in the
#' standard order.
#'
#' @param P A matrix containing points describing a polygon.
#'
#' @return A matrix containing the same points as P, stored in counterclockwise order.
#'
p_to_ccw <- function( P ) {
    r <- if( p_is_ccw( P ) ) P else P[ vct( P ):1, ]
}

#' Line-polygon intersection
#'
#' Determine the points at which the given line segment p1 intersects the polygon P, if they exist.
#'
#' This function will determine the points at which the given segment enters and exits the given
#' polygon, along the segment itself.
#'
#' The entry and exit points are described as a pair of scalars
#' ranging between 0 and 1, indicating the point of entry and exit as scaling factors along the
#' segment.
#'
#' If the entry or exit points do not exist (because the segment starts and/or ends inside
#' the polygon), these will be equal to 0 and 1, respectively. If the segment lies entirelly
#' outside the polygon, the result will be NULL. Note that the result will be 0 and 1 if the
#' segment is entirelly contained by the polygon.
#'
#' This function can return either the scalars themselves, or the vector corresponding to the
#' scalar product of the segment and each scalar (i.e. a vector to the entry and exit points from
#' the tail of the segment)
#'
#' @param P     A matrix containing points describing a polygon.
#' @param p1    A point at the leading end of a segment.
#' @param p0    A point at the trailing end of a segment. Defaults to the origin.
#' @param value Character vector indicating the desired return type. One of 's' for scalar or 'v'
#'              for vector. See details.
#'
#' @return If value is equal to 's', a pair of scalars indicating the length of segment up to the
#'         entry and exit points, or 0 and or 1 if either or both of these don't exist. If value is
#'         equal to 'v', a pair of vectors from the tail of the segment to the entry and exit
#'         points, or the zero vector and/or a vector equal to the given segment if either or both
#'         of these points don't exist. If the segment lies entirelly outsode the polygon, the
#'         value will be NULL.
#'
#' @export
pl_clip <- function( P, p1, p0=O( dct( P ) ), value=c('s','v') ) {
    value <- match.arg( value )
    P %<>% p_to_ccw() # P points in ccw order
    v = vdif( p1, p0 )
    tE <- 0; tL <- 1
    for( i in 1:vct( P ) ) {
        j <- ( i %% vct( P ) ) + 1
        e0 <-  P[ i ,]; e1 <- P[ j ,]
        nv <- vorth( vdif( e1, e0 ), cw=TRUE )
        N  <- vinnerp( vdif( e0, p0 ), nv )
        D  <- vinnerp( vdif( p1, p0 ), nv )
        if( D == 0 ) { # parallel
            if( N < 0 ) return( NULL ) # lies outside poly
            else next # no intersection so no concern
        }
        if( D < 0 ) { # S crosses edge to enter
            tE <- max( N / D, tE )
            if( tE > tL ) return( NULL ) # enters after leaving; lies outside
        }
        if( D > 0 ) { # S cross edge to leave
            tL <- min( N / D, tL )
            if( tL < tE ) return( NULL ) # leaves before entering; lies outside
        }
    }
    r <- switch( value, s=c( tE, tL ), v=V( smul( vadd( p0, v ), tE ), smul( vadd( p0, v ), tL ) ) )
    return( r )
}

#' Polygon-polygon intersection
#'
#' Return TRUE if the given polygons intersect.
#'
#' @param P0 A matrix containing points describing a polygon
#' @param P1 A matrix containing points describing a polygon
#'
#' @return TRUE if the given polygons intersect, FALSE otherwise.
#'
#' @export
pp_clip <- function( P0, P1 ) {
    if( is.null( P0 ) || is.null( P1 ) ) return( FALSE ) # TODO: rid of polyclip
    l0 <- list( to_l( P0 ) )
    l1 <- list( to_l( P1 ) )
    return( polyclip::polyclip( l0, l1 ) %>% length() != 0 )
} # TODO: refactor to remove polyclip

#' Size of minimal bounding parallelepiped
#'
#' Compute the area of the minimal bounding parallelepiped around the points in m. The
#' parallelepiped is defined allong the major axis of m and its normal (i.e. properly minimal,
#' including rotation)
#'
#' @param m A matrix containing points describing a polygon.
#'
#' @return The area of the minimal bounding parallelepiped containing the all the points in m.
#'
#' @export
p_boxsize <- function( m ) {
    a1 <- axis_major( m )
    a2 <- axis_minor( m )
    return( vnorm( a1 ) * vnorm( a2 ) )
}

#' Push an object along a direction until all of its elements are outside the region covered by the
#' elements in another object.
#'
#' This function will determine the minimal displacement distance needed for all points in an
#' object (a polyhgon, a cloud of points) to exit the region covered by the points in another
#' object.
#'
#' This function traces convex hulls around both sets of points, and then determines the minimum
#' distance that one would need to translate one of the convex hulls in order to make the
#' intersection between them empty.
#'
#' The value of this function may be a scalar equal to the displacement distance, a displacement
#' vector or the actual points after translation depending on the value of the \code{value}
#' parameter.
#'
#' @param m     A matrix representing a vector space.
#' @param X     A matrix representing a vector space.
#' @param v     A vector indicating the direction along which m can be pushed. Defaults to the line
#'              between the centroids of m and X.
#' @param value A character vector indicating the return type. One of 's', 'v', or 'p' to indicate
#'              scalar, vector or poionts.
#'
#' @return If value is equal to 's', a scalar equal to the distance along v m needs to be pushed
#'         to clear X. If it is equal to 'v', a vector equal to the displacement (i.e. v scaled
#'         by the value returned when 's'). If it is 'p', the points in m translated in the v
#'         direction so they are clear of X (i.e. translated by the displacement vector returned
#'         when 'v').
#'
#' @export
pp_clear_edges <- function( m, X, v=vdif( C( m ), C( X ) ), value=c('s','v','p') ) {
    value <- match.arg( value )
    P <- p_hull( m )
    X <- p_hull( X )
    X %<>% translate( -C( P ) )
    P %<>% translate( -C( P ) )

    # sort vertices in P along v
    vsP <- vproj( P, v, scalar=TRUE ) %>% order()
    # find edges in P inside X
    PinX <- vapply( vsP, function( i ) {
        lc <- pl_clip( X, p0=P[ i, ], p1=P[ ( i %% vct( P ) ) + 1,  ], value='s' )
        return( if( is.null( lc ) ) c( NA, NA ) else lc )
    }, c( 0, 0 ) )

    if( all( is.na( colSums( PinX ) ) ) ) {
        # no P edges in X, return 0, O(), or unmoved P
        r <- switch( value, s=0, v=O( dct( P ) ), p=P )
        return( r )
    }

    # get max displacement along v to make all vertices in P exit X
    ax <- axis_major( X ) %>% smul( 2 ) %>% vproj( v );
    ts <- vapply( 1:length( vsP ), function( i ) {
        if( is.na( sum( PinX[,i] ) ) ) return( 0 )
        p0 <- P[ vsP[i], ]; p1 <- P[ ( vsP[i] %% vct( P ) ) + 1, ]; ev <- vdif( p1, p0 )
        pE <- ev %>% smul( PinX[1,i] ) %>% vadd( p0 ); pL <- ev %>% smul( PinX[2,i] ) %>% vadd( p0 )
        d1 <- pl_clip( X, p0=pE, p1=translate( ax, pE ), value='s' )[2]
        d2 <- pl_clip( X, p0=pL, p1=translate( ax, pL ), value='s' )[2]
        d1 <- if( !is.null( d1 ) && d1 != 1 ) d1 else 0
        d2 <- if( !is.null( d2 ) && d2 != 1 ) d2 else 0
        return( max( d1, d2 ) )
    }, 0 ) %>% max()
    tv <- smul( ax, ts )
    r <- switch( value, s=vnorm( tv ), v=tv, p=translate( m, tv ) )
    return( r )
}

#' Push two objects apart until the intersection between them is empty.
#'
#' This function will determine the minimal displacement needed for the intersection (or overlap)
#' between two objects to be empty. See \link{pp_clear_edges} for details.
#'
#' The value of this function may be a scalar equal to the displacement distance, a displacement
#' vector or the actual points after translation depending on the value of the \code{value}
#' parameter.
#'
#' @param m0    A matrix representing a vector space.
#' @param X     A matrix representing a vector space.
#' @param v     A vector indicating the direction along which m can be pushed. Defaults to the line
#'              between the centroids of m and X.
#' @param value A character vector indicating the return type. One of 's', 'v', or 'p' to indicate
#'              scalar, vector or poionts.
#'
#' @return If value is equal to 's', a scalar equal to the distance along v m needs to be pushed
#'         to clear X. If it is equal to 'v', a vector equal to the displacement (i.e. v scaled
#'         by the value returned when 's'). If it is 'p', the points in m translated in the v
#'         direction so they are clear of X (i.e. translated by the displacement vector returned
#'         when 'v').
#'
#' @export
pp_clear_poly <- function( m0, X, v=C( m0 ), value=c( 's','v','p' ), aprox=FALSE ) {
    P <- p_hull( m0 ); X <- p_hull( X )
    sX <- axis_major( X ) %>% vnorm(); sP <- axis_major( P ) %>% vnorm()
    X %<>% translate( -C( P ) )
    P %<>% translate( -C( P ) )
    if( aprox ) {
        stop('Not implemented')
    } else {
        tv0 <- pp_clear_edges( P, X,  v, value='v' )
        P %<>% translate( tv0 )
        tv1 <- pp_clear_edges( X, P, -v, value='v' )
        P %<>% translate( tv1 )
    }
    tv <- C( P )
    r <- switch( match.arg( value ), s=vnorm( tv ), v=tv, p=translate( m0, tv ) )
    return( r )
}

#' Push an object in the given direction until it clears all given shapes.
#'
#' This funciton takes an object, a series of points and a set of shapes indicated by a point
#' shape membership vector and pushes the given object in direction of the given vector until the
#' intersection between it and all the shapes is empty.
#'
#' The value of this function may be a scalar equal to the displacement distance, a displacement
#' vector or the actual points after translation depending on the value of the \code{value}
#' parameter.
#'
#' @param m0    A matrix representing a vector space.
#' @param m1    A matrix representing a vector space.
#' @param km    A membership vector of length equal to the number of elements in \code{m1},
#'              indicating the shape to which each point is associated to.
#' @param v     A vector indicating the allowed displacement direction.
#' @param value A character vector indicating the desired return type. One of 's', 'v' or 'p' for
#'              scalar, vector or points, respectively. See drtails.
#' @param aprox Logical. If TRUE, calculate intersections approximately (may be considerably faster
#'              but wrong). Defaults to FALSE. Currently not implemented.
#' @param sort  Optional sorting vector indicating an order for shapes in \code{m1}. See
#'              \link{split_sort}. Defaults to NULL (don't sort and process in the order of unique
#'              values in km).
#'
#' @return If \code{value} is equal to 's', a scalar equal to the distance along \code{v} \code{m0}
#'         needs to be pushed to be clear of all shapes in \code{m1}. If it is equal to 'v', a
#'         vector equal to the displacement (i.e. \code{v} scaled by the value returned when 's').
#'         If it is 'p', the points in \code{m0} translated in the \code{v} direction so they are
#'         clear of all sjapes in \code{m1} (i.e. translated by the displacement vector returned
#'         when 'v').
#'
#' @export
pp_clear_all <- function( m0, m1, km, v=C( m0 ), value=c('s','v','p'), aprox=FALSE, sort=NULL ) {
    value <- match.arg( value )
    o <- C( m0 )
    P <- p_hull( m0 )
    X <- m1 %>% split_sort( km, sort=sort )
    while( length( X ) != 0 ) {
        if( pp_clip( P, X[[1]] ) ) P %<>% pp_clear_poly( X=X[[1]], v=v, aprox=aprox, value='p' )
        if( !aprox ) X[[1]] <- NULL # TODO: polyclip uses ints; our clearance is too accurate
        if( length( X ) != 0 ) {
            X %<>% lfilter( function( P_ ) pp_clip( P, P_ ) )
        }
    }
    tv <- vdif( C( P ), o )
    r <- switch( value, s=vnorm( tv ), v=tv, p=translate( P, tv ) )
    return( r )
}

#' Is polygon P stored in counterclockwise or clockwise order?
#' TODO: generalize to arbitrary planes
#' @export
# TODO: generalize to nd
p_is_ccw <- function( P ) {
    return( sum( diff( chull( P ) ) ) < 0 )
    # ccw <- 0
    # for( i in 1:nrow( P ) ) {
    #     j <- ( i %% nrow( P ) ) + 1
    #     p0 <- P[i,]; p1 <- P[j,]
    #     ccw %<>% `+`( p1[1] - p0[1] ) * ( p1[2] + p0[2] )
    # }
    # return( ccw < 0 )
}
