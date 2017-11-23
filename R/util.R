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

#' Transform object to list( x, y ) format
#' @export
to_l <- function( m ) {
    return( list( x=m[,1], y=m[,2] ) )
}

#' Extract object from list( x, y ) or list( list( x, y ) ) format
#' @export
# TODO: cbind dependency
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

#' Create n random elements of dimensionality d with the given distribution P
#' @export
random <- function( n=10, d=2, P=function( n ) runif( n, -1, 1 ) ) {
    m <- matrix( P( n * d ), ncol=d )
    return( m )
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

#' Filter list
#' @export
lfilter <- function( l, pred ) {
    if( is.null( l ) || length( l ) == 0 ) return( list() )
    out <- list()
    for( i in 1:length( l ) ) {
        if( pred( l[[i]] ) ) out[[length( out ) + 1]] <- l[[i]]
    }
    return( out )
}

#' Split m according to km vector, and list them in the given sort order
#' @export
split_sort <- function( m, km, sort=NULL ) {
    sort <- if( is.null( sort ) ) {
        vapply( unique( km ), function( k ) p_box_size( m[ k == km, ] ), 0 ) %>% order()
    } else sort
    out <- list()
    for( i in 1:length( sort ) ) {
        k <- sort[i]
        out[[k]] <- m[ k == km, ]
    }
    return( out )
}
