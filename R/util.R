#  Helper functions ---------------------------------------------------------------------------

#' Degrees to radians
#'
#' Converts the given number of degrees to its equivalent in radians. One degree is equal to
#' \eqn{\pi / 180} radians.
#'
#' @param d Degrees.
#'
#' @return Radians.
#'
#' @export
deg2rad <- function( d ) {
    return( ( pi * d ) / 180 )
}

#' Radians to degrees
#'
#' Converts the given number of radians to its equivalent in degrees. One degree is equal to
#' \eqn{\pi / 180} radians.
#'
#' @param r Radians.
#'
#' @return Degrees.
#'
#' @export
rad2deg <- function( r ) {
    return( 180 * r / pi )
}

#' Transform object to list( x, y ) format
#'
#' Converts geometric objects from the standard matrix format used in this package to the
#' \code{list( x, y )} format used in some of base graphics functions.
#'
#' @param m A matrix representring a vector space. Only the first two dimensions will be taken into
#'          account.
#'
#' @return A list with two vectors, named 'x' and 'y', containing the first and second dimensions
#'         of the given matrix \code{m}.
#'
#' @export
to_l <- function( m ) {
    return( list( x=m[,1], y=m[,2] ) )
}

#' Extract object from list( x, y ) or list( list( x, y ) ) format
#'
#' Convert from the \code{list( x, y )} or \code{list( list( x, y ) )} used in polyclip and some of
#' base graphics functions to the standard matrix format used in this package.
#'
#' @param l       A \code{list( x, y )} or \code{list( list( x, y ) )}.
#' @param combine Logical. If TRUE and l is a list of lists, combine all lists into one object.
#'                Defaults to FALSE.
#'
#' @return If the given list contains a single element or \code{combine} is TRUE, a single matrix
#'         representing a vector space. Otherwise, a list of matrices representing vector spaces.
#'
#' @export
to_m <- function( l, combine=FALSE ) {
    if( combine && length( l ) == 2 && is_plist( l ) ) {
        return( D( l$x, l$y ) )
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
            return( D( x, y ) )
        } else {
            return( lapply( l, function( x ) to_m( x, combine=TRUE ) ) )
        }
    } else {
        error_list_format( 'Wrong format in plist for matrix conversion' )
    }
}

#' Check if pl is list( x, y )
#'
#' Determine if the reference in pl corresponds to an object of the \code{list( x, y )} or
#' \code{list( list( x, y ) )} format used in some of base graphics functions and the polyclip
#' package.
#'
#' @param pl An \R object
#'
#' @return \code{TRUE} if the given object is (can be coerced to) a polygon list, \code{FALSE}
#'         otherwise.
#'
#' @export
is_plist <- function( pl ) {
    return( !is.null( names( pl ) ) && all( names( pl ) %in% c( 'x', 'y' ) ) )
}

#' Create n random elements of dimensionality d with the given distribution P
#'
#' @param n An integer indicating the number of elements (points, vectors) to create. Defaults to 10.
#' @param d An integer indicating th enumber of dimensions for the created elements. Defaults to 2.
#' @param P An RNG function to create values. Defaults to the uniform distribution between -1 and 1.
#'
#' @return A matrix representing a random vector space with the given structure, created using the
#'         given RNG function.
#'
#' @export
random <- function( n=10, d=2, P=function( n ) stats::runif( n, -1, 1 ) ) {
    m <- matrix( P( n * d ), ncol=d )
    return( m )
}

#' Make color palette factory
#'
#' Wrapper to create color palettes consistently. Currently wraps (and pulls in a dependency on)
#' \link{viridis}.
#'
#' Note that this function will create a palette creation function, i.e. a function taking a
#' single integer that generates a palette with that number of colors in it. See
#' \link{viridis::viridis_pal} for details.
#'
#' @param ... Parameters passed to the actual palette creation function.
#'
#' @return a function taking a single integer as parameter that will create palettes with that
#'         number of colors in them.
#'
#' @export
color_make <- function( ... ) {
    function( n ) {
        viridis::viridis_pal( ... )( n )
    } %>% return()
}

#' Add alpha to colors
#'
#' Add the given alpha value to the given color or colors. This function accepts color
#' specifications in all three formats accepted by base garphics: a color name (see \link{colors}),
#' a hex number #rrggbb or #rrggbbaa (see \link{rgb}) or an integer (see \link{palette}).
#'
#' This function is vectorized similarly to the underlying \link{col2rgb} and \link{rgb} functions
#' used internally. In addition, it the colors are passed as a matrix, this function will operate
#' row-wise recursively.
#'
#' If the given color or colors already contains an alpha channel, this function will interpret the
#'  given alpha level as a factor and multiply the pre-existent alpha level by the given level.
#'
#' @param col   A matrix or vector of any of the three kinds of R color specifications.
#' @param alpha A numeric value between 0 and 1 represinting the desired alpha level to add.
#'
#' @return A new color specification with the alpha level added, in hex format, and in the same
#' shape as the input (vector for vector input, matrix for matrix iunput)
#'
#' @export
color_alpha <- function( col=graphics::par('fg'), alpha=.1 ) {
    if( alpha < 0 || alpha > 1 ) {
        stop( sprintf( 'Invalid alpha value %4.2f. Must be between 0 and 1', alpha ) )
    }
    if( is.matrix( col ) ) {
        col <- apply( col, 2, function( c ) color_alpha( c, alpha ) )
    } else {
        col <- grDevices::col2rgb( col, alpha=TRUE )
        col[4,] <- round( col[4,] * alpha, 0 )
        col %<>% apply( 2, function( x ) {
            grDevices::rgb( x[1], x[2], x[3], x[4], maxColorValue=255 )
        } )
    }
    return( col )
}

#' Filter list
#'
#' Convenience wrapper to filter a list using a predicate function. I'm not entirelly sure why I wrote this function, but the logic seems reminiscent of the Schwartzman transform or some strategy to use lambdas in list subsetting.
#'
#' TODO: check and remove?
#'
#' @param l    A list.
#' @param pred A predicate to filter elements of \code{l} with.
#'
#' @return A list containing only elements from l that were accepted by the given predicate.
#'
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
#'
#' Split the elements contained in the matrix representation of a vector space \code{m} according
#' to the membership vector \code{km} indicating which shape each element in \code{m} should be
#' associated with and return as a list of matrices representing vector spaces. Optionally sort the
#' output list according to the given soryt order.
#'
#' @param m  A matrix representing a vector space.
#' @param km A membership vector, i.e. an integer vector of the same length as the number of
#'           elements in \code{m}, indicating the shape that each element in \code{m} belongs to.
#'
#' @return A list of matrices, with one entry for each distinct value in \code{km}.
#'
#' @export
split_sort <- function( m, km, sort=NULL ) {
    sort <- if( is.null( sort ) ) {
        vapply( unique( km ), function( k ) p_boxsize( m[ k == km, ] ), 0 ) %>% order()
    } else sort
    out <- list()
    for( i in 1:length( sort ) ) {
        k <- sort[i]
        out[[k]] <- m[ k == km, ]
    }
    return( out )
}
