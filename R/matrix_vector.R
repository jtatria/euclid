# Matrix-Vector ops ---------------------------------------------------------------------------

#' Centroid
#' @export
#' @importFrom Matrix colMeans
C <- function( m ) {
    if( !is.numeric( m ) ) {
        browser()
    }
    m %<>% rbind()
    return( matrix( colMeans( m ), 1 ) )
}

#' Major axis across object
#' @export
axis_major <- function( m, acc=FALSE, vct=TRUE ) {
    m <- if( acc ) m else m[ chull( m ), ]
    o <- C( m )
    frw <- m[ which.max( distf( m, o ) ), ]
    bck <- m[ which.min( dot( vdif( m, o ), vdif( frw, o ) ) ), ]
    if( vct ) {
        return( vdif( frw, bck ) )
    } else {
        return( rbind( frw, bck) )
    }
}

#' Minor axis across object
#' @export
axis_minor <- function( m, acc=FALSE, vct=TRUE, major=axis_major( m, acc=FALSE ) ) {
    o <- C( m )
    lft <- m[ which.max( dot( vdif( m, o ), orth( major ) ) ),]
    rgt <- m[ which.min( dot( m, lft ) ), ]
    if( vct ) {
        return( vdif( lft, rgt ) )
    } else {
        return( rbind( lft,rgt ) )
    }
}
