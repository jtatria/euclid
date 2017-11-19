# Matrix-Scalar ops ---------------------------------------------------------------------------

#' Compute area of the minimal rectangle containing the given object
#' @export
aarea <- function( m ) {
    maj <- axis_major( m )
    min <- axis_minor( m, major=maj )
    return( norms( maj ) * norms( min ) )
}
