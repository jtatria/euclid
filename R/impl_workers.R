#' Low-level workload functions.
#' The functions in this file are prime candidates for implementation in Rcpp/RcppParallel/CUDA
#' Virtually all operations in this package use this functions.
#' The order of computation is defined in 'storage.R' file.

#' s -> v
#' @export
s_v_op <- function( s0, f_, d ) {
    if( !is.vector( s0 ) ) error_not_scalar( 'Need a plain vector for scalar->vector op', s0 )
    if( length( s0 ) == 1 ) return( f_( s0 ) %>% V() )
    return( vwise( s0, function( s ) f_( s ), r=O( d ) ) )
}

#' v -> s
#' @export
v_s_op <- function( v0, f_ ) {
    v0 %<>% V()
    if( vct( v0 ) == 1 ) return( f_( v0 ) )
    return( vwise( v0, function( v ) f_( v ), reduce=TRUE ) )
}

#' (v,s) -> v
#' @export
vs_v_op <- function( v0, s0, f_, cross=FALSE ) {
    return( vs_w_op( v0, s0, f_, d=dct( v0 ), cross=cross ) )
}

#' (v,s) -> w
#' @export
vs_w_op <- function( v0, s0, f_, d, cross=FALSE ) {
    v0 %<>% V()
    if( !is.vector( s0 ) ) error_not_scalar( 'Need a plain vector for vector-scalar->vector op', s0 )
    if( vct( v0 ) == 1 && length( s0 ) == 1 ) return( f_( v0, s0 ) %>% V() )
    if( length( s0 ) == 1 ) return( v_v_op( v0, function( v_ ) f_( v_, s0 ) ) )
    if( vct( v0 ) == 1 ) return( s_v_op( s0, function( s_ ) f_( v0, s_ ), d=d ) )
    if( vct( v0 ) != length( s0 ) && !cross ) stop( 'Incompatible dimensions in vector-scalar op' )
    if( !cross ) return( vwise( v0, m1=s0, function( v_, s_ ) f_( v_, s_ ), r=O( d ) ) )
    out <- vapply( 1:vct( v1 ), function( i ) {
        vwise( v0, function( v ) f_( v, s0[i] ), reduce=TRUE )
    }, rep( 0, vct( v0 ) ) )
    return( out )
}

#' v -> v
#' @export
v_v_op <- function( v0, f_ ) {
    return( v_w_op( v0, f_, d=dct( v0 ) ) )
}

#' v -> w
#' @export
v_w_op <- function( v0, f_, d ) {
    v0 %<>% V()
    if( vct( v0 == 1 ) ) return( f_( v0 ) %>% V() )
    return( vwise( v0, function( v ) f_( v ), r=O( d ) ) )
}

#' (v,u) -> s
#' @export
vv_s_op <- function( v0, v1, f_, cross=FALSE ) {
    v0 %<>% V(); v1 %<>% V()
    if( dct( v0 ) != dct( v1 ) ) stop( 'Wrong dimensions for vector-vector op' )
    if( vct( v0 ) == 1 && vct( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( vct( v1 ) == 1 ) return( vwise( v0, function( v ) f_( v, v1 ), reduce=TRUE ) )
    if( vct( v0 ) == 1 ) return( vwise( v1, function( v ) f_( v0, v ), reduce=TRUE ) )
    if( vct( v0 ) != vct( v1 ) && !cross ) stop( 'Non-isomorphics in vector-vector op' )
    if( !cross ) return( vwise( v0, m1=v1, function( v, u ) f_( v, u ), reduce=TRUE ) )
    out <- vapply( 1:vct( v1 ), function( i ) {
        vwise( v0, function( v ) f_( v, v1[i,] ), reduce=TRUE )
    }, rep( 0, vct( v0 ) ) )
    return( out )
}

#' (v,u) -> v
#' @export
vv_v_op <- function( v0, v1, f_, cross=FALSE ) {
    return( vv_w_op( v0, v1, f_, d=dct( v0 ), cross=cross ) )
}

#' (v,u) -> w
#' @export
vv_w_op <- function( v0, v1, f_, d, cross=FALSE ) {
    v0 %<>% rbind(); v1 %<>% rbind()
    if( dct( v0 ) != dct( v1 ) ) error_dimension( 'Wrong dimensions for vector-vector op', v0, v1 )
    if( vct( v0 ) == 1 && vct( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( vct( v1 ) == 1 ) return( vwise( v0, function( v ) f_( v, v1 ), r=O( d ) ) )
    if( vct( v0 ) == 1 ) return( vwise( v1, function( v ) f_( v0, v ), r=O( d ) ) )
    if( vct( v0 ) != vct( v1 ) && !cross ) stop( 'Non-isomorphics in vector-vector op' )
    if( !cross ) return( vwise( v0, m1=v1, function( v, u ) f_( v, u ), r=O( d ) ) )
    stop( 'Not implemented' )
}

#' vapply wrapper for 'row'-wise operations
#' @export
vwise <- function( m0, f_, reduce=FALSE, ..., m1=NULL, r=NULL ) {
    m0 %<>% V()
    r <- if( is.null( r ) ) if( reduce ) 0.0 else O( dct( m0 ) ) else r
    if( is.null( m1 ) ) {
        out <- vapply( 1:vct( m0 ), function( i ) f_(  m0[ i ,], ... ), r )
    } else {
        m1 %<>% V()
        if( vct( m1 ) != vct( m0 ) ) error_dimension( 'Wrong dimensions for vwise arg', vct( m1 ), vct( m0 ) )
        out <- vapply( 1:vct( m0 ), function( i ) f_(  m0[ i ,],  m1[ i ,], ... ), r )
    }
    out <- if( reduce ) out else t( out )
    return( out )
}

#' vapply wrapper for 'column'-wise operations
#' @export
dwise <- function( m0, f_, ..., m1=NULL, reduce=FALSE, r=NULL ) {
    m0 %<>% D()
    r <- if( is.null( r ) ) if( reduce ) 0.0 else O( dct( m0 ) ) else r
    if( is.null( m1 ) ) {
        out <- vapply( 1:dct( m0 ), function( i ) f_( getd( m0, i ), ...), r )
    } else {
        m1 %<>% D()
        if( dct( m1 ) != dct( m0 ) ) error_dimension( 'Wrong dimensions for dwise arg', dct( m1 ), dct( m0 ) )
        out <- vapply( 1:dct( m0 ), function( i ) f_( getd( m0, i ), getd( m1, i ), ... ), r )
    }
    return( out )
}


