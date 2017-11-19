#  Helper functions ---------------------------------------------------------------------------

#' Degrees to radians
#' @export
deg2rad <- function( d ) {
    return( ( pi * d ) / 180 )
}

#' Radians to degrees
#' @export
rad2deg <- function( r ) {
    return( 180 * r / pi )
}

# This is here to eventually dettach all other functions from the storage order of matrices in R.
# I for one find it easier to deal with rows as primary order, but since R stores everything in
# colmajor format this is rather inefficient :(
#' Vector
#' @export
v <- function( m, i=1 ) {
    m %<>% rbind()
    return( rbind( m[i,] ) )
}

# This is here to eventually dettach all other functions from the storage order of matrices in R.
# I for one find it easier to deal with rows as primary order, but since R stores everything in
# colmajor format this is rather inefficient :(
#' Dimension
#' @export
d <- function( m, i=1 ) {
    return( m[,i] )
}

#' Transform object to list( x, y ) format
#' @export
to_l <- function( m ) {
    return( list( x=m[,1], y=m[,2] ) )
}

#' Extract object from list( x, y ) or list( list( x, y ) ) format
#' @export
to_m <- function( l, combine=FALSE ) {
    if( combine && length( l ) == 2 && is_plist( l ) ) {
        return( cbind( l$x, l$y ) )
    }
    if( length( l ) == 2 && is_plist( l ) ) {
        return( list( to_m( l, combine=TRUE ) ) )
    }
    if( all( lapply( l, function( x ) is_plist( x ) ) %>% unlist() ) ) {
        if( combine ) {
            x <- c(); y <- c()
            for( i in 1:length( l ) )  {
                x %<>% c( x, l[[i]]$x ); y %<>% c( y, l[[i]]$y )
            }
            return( cbind( x, y ) )
        } else {
            return( lapply( l, function( x ) to_m( x, combine=TRUE ) ) )
        }
    } else {
        error_list_format( 'Wrong format in plist for matrix conversion' )
    }
}

#' Check if pl is list( x, y )
#' @export
is_plist <- function( pl ) {
    return( !is.null( names( pl ) ) && all( names( pl ) %in% c( 'x', 'y' ) ) )
}

#' Is polygon P stored in counterclockwise or clockwise order?
#' @export
is_ccw <- function( P ) {
    ccw <- 0
    for( i in 1:nrow( P ) ) {
        j <- ( i %% nrow( P ) ) + 1
        p0 <- P[i,]; p1 <- P[j,]
        ccw %<>% `+`( p1[1] - p0[1] ) * ( p1[2] + p0[2] )
    }
    return( ccw < 0 )
}

#' Create n random points in d dimensions
#' @export
random <- function( n=10, d=2, distr=runif ) {
    m <- matrix( distr( n * d ), ncol=d )
    return( m %>% center() %>% resize() )
}

#' Make color palette
#' @export
color_make <- function( ... ) {
    function( n ) {
        viridis::viridis_pal( ... )( n )
    } %>% return()
}

#' Add alpha to colors
#' @export
color_alpha <- function( col=par('fg'), alpha=.1 ) {
    if( is.matrix( col ) ) {
        col <- apply( col, 2, function( c ) color_add_alpha( c, alpha ) )
    } else {
        col <- col2rgb( col, alpha=TRUE )
        col[4,] <- round( col[4,] * alpha, 0 )
        col %<>% apply( 2, function( x ) {
            rgb( x[1], x[2], x[3], x[4], maxColorValue=255 )
        } )
    }
    return( col )
}



