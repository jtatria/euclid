#' v -> s
#' @export
v_s_op <- function( v0, f_ ) {
    v0 %<>% rbind()
    if( nrow( v0 ) == 1 ) return( f_( v0 ) )
    return( rwise( v0, function( v ) f_( v ), reduce=TRUE ) )
}

#' v -> v
#' @export
v_v_op <- function( v0, f_ ) {
    v0 %<>% rbind()
    if( nrow( v0 ) == 1 ) return( f_( v0 ) )
    return( rwise( v0, function( v ) f_( v ), reduce=FALSE ) )
}

#' (v,u) -> s
#' @export
vv_s_op <- function( v0, v1, f_ ) {
    v0 %<>% rbind(); v1 %<>% rbind()
    if( ncol( v0 ) != ncol( v1 ) ) stop( 'Wrong dimensions for vector-vector op' )
    if( nrow( v0 ) == 1 && nrow( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( nrow( v1 ) == 1 ) return( rwise( v0, function( v ) f_( v, v1 ), reduce=TRUE ) )
    if( nrow( v0 ) == 1 ) return( rwise( v1, function( v ) f_( v0, v ), reduce=TRUE ) )
    if( nrow( v0 ) != nrow( v1 ) ) stop( 'Non-isomorphics in vector-vector op' )
    return( rwise( v0, m1=v1, function( v, u ) f_( v, u ) ), reduce=TRUE )
}

#' (v,u) -> v
#' @export
vv_v_op <- function( v0, v1, f_ ) {
    v0 %<>% rbind(); v1 %<>% rbind()
    if( ncol( v0 ) != ncol( v1 ) ) error_dimension( 'Wrong dimensions for vector-vector op', v0, v1 )
    if( nrow( v0 ) == 1 && nrow( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( nrow( v1 ) == 1 ) return( rwise( v0, function( v ) f_( v, v1 ), reduce=FALSE ) )
    if( nrow( v0 ) == 1 ) return( rwise( v1, function( v ) f_( v0, v ), reduce=FALSE ) )
    if( nrow( v0 ) != nrow( v1 ) ) stop( 'Non-isomorphics in vector-vector op' )
    return( rwise( v0, m1=v1, function( v, u ) f_( v, u ), reduce=FALSE ) )
}

#' vapply wrapper for row-wise operations
#' @export
rwise <- function( m0, f_, reduce=FALSE, ..., m1=NULL ) {
    m0 %<>% rbind()
    r <- if( reduce ) 0.0 else O( ncol( m0 ) )
    if( is.null( m1 ) ) {
        out <- vapply( 1:nrow( m0 ), function( i ) f_( m0[i,], ... ), r )
    } else {
        m1 %<>% cbind() # coerce to matrix
        if( nrow( m1 ) != nrow( m0 ) ) error_dimension( 'Wrong dimensions for rwise arg', ncol( m1 ), ncol( m0 ) )
        out <- vapply( 1:nrow( m0 ), function( i ) f_( m0[i,], m1[i,], ... ), r )
    }
    out <- if( reduce ) out else t( out )
    return( out )
}

#' vapply wrapper for colun-wise operations
cwise <- function( m0, f_, reduce=FALSE, ..., m1=NULL ) {
    m0 %<>% rbind()
    r <- if( reduce ) 0.0 else O( nrow( m0 ) )
    if( is.null( m1 ) ) {
        out <- vapply( 1:ncol( m0 ), function( i ) f_( m0[,i], ...), r )
    } else {
        m1 %<>% rbind()
        if( ncol( m1 ) != ncol( m0 ) ) error_dimension( 'Wrong dimensions for cwise arg', ncol( m1 ), ncol( m0 ) )
        out <- vapply( 1:ncol( m0 ), function( i ) f_( m0[,i], m1[,i], ... ), r )
    }
    return( out )
}
