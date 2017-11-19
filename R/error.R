# Errors --------------------------------------------------------------------------------------

error_dimension <- function( msg, v0, v1 ) {
    message( msg ); cat( str( v0 ) ); cat( str( v1 ) ); stop( 'Dimension error')
}
error_not_scalar <- function( msg, s ) {
    message( msg ); cat( str( p ) ); stop( 'Non-scalar error' )
}

error_list_format <- function( msg, l ) {
    message( msg ); cat( str( l ) ); stop( 'List format error')
}
