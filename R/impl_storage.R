ORDER <- list(
    V = list(
        count  = nrow,
        coerce = rbind,
        get    = function( m, i ) return( m[i,] ),
        set    = function( m, i, x ) { m[i,] <- x; return( m ) }
    ),
    D = list(
        count  = ncol,
        coerce = cbind,
        get    = function( m, i ) return( m[,i] ),
        set    = function( m, i, x ) { m[,i] <- x; return( m ) }
    )
)

#' @export
V <- function( ... ) {
    return( rbind( ... ) )
}

#' @export
vct <- function( m ) {
    return( nrow( m ) )
}

#' @export
getv <- function( m, i ) {
    return( m[i,] )
}

#' @export
setv <- function( m, i, v ) {
    m[i,] <- v
    return( m )
}

#' @export
D <- function( ... ) {
    cbind( ... )
}

#' @export
dct <- function( m ) {
    return( ncol( m ) )
}

#' @export
getd <- function( m, i ) {
    return( m[,i] ) %>% D()
}

#' @export
setd <- function( m, i, v ) {
    m[,i] <- v
    return( m )
}

#' @export
mk <- function( ... ) {
    return( c( ... ) %>% V() )
}
