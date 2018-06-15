# Errors --------------------------------------------------------------------------------------

error_dimension <- function( msg, ... ) {
    error( msg, 'Dimension error', ... )
}

error_not_scalar <- function( msg, ... ) {
    error( msg, 'Not a scalar', ... )
}

error_not_square <- function( msg, ... ) {
    error( msg, 'Not a square matrix', ... )
}

error_list_format <- function( msg, ... ) {
    error( msg, 'Unknown list format', ... )
}

error <- function( msg, err, ... ) {
    message( msg )
    for( o in list( ... ) ) cat( utils::str( o ) )
    stop( err )
}
