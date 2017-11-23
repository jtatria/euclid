
#' Arranges shapes in m on layout in km according to membs vector
#' @export
compose <- function( m, shp, km, noverlap=noverlap_radial, ... ) {
    if( length( km ) != vct( m ) ) stop('Wrong dimension for shape mebership vector')
    if( length( unique( km ) ) != vct( shp ) ) stop('Wrong dimension for shape')
    ks <- unique( km )
    vapply( ks, function( k ) { m[ k == km,  ] %<>% center(); return( 0 ) }, 0 )
    m %<>% center() %>% resize()
    dist <- vdist( shp, shp, cross=TRUE ) %>% `[`( . != 0 ) %>% min()
    size <- vapply( ks, function( k ) m[ k == km, ] %>% axis_minor() %>% vnorm(), 0 ) %>% max()
    shp %<>% center() %>% resize( size / dist )
    for( k in ks ) m[ k == km ,] %<>% position(  shp[ k ,] )
    if( !is.null( noverlap ) ) m %<>% noverlap( km, K=.05 )
    return( m )
}

#' @export
noverlap_radial <- function( m, km, K=.05, sort=c('-disp','-size','size','disp') ) {
    ks <- unique( km )
    sort <- match.arg( sort )
    size <- vapply( ks, function( k ) p_boxsize( m[ k == km, ] ), 0 )
    disp <- vapply( ks, function( k ) vnorm( C( m[ k == km, ] ) ), 0 )
    sort <- switch( sort,
        'size'=order( size ), '-size'=order( -size ), 'disp'=disp, '-disp'=order( -disp )
    )
    for( i in 1:length( sort ) ) {
        k <- sort[i]
        P <- m[ k == km, ] %>% p_hull() %>% resize( S=K, abs=TRUE )
        tv <- P %>% pp_clear_all( m[ k != km, ], km[ km != k ], sort=sort[ sort != k ], value='v' )
        m[ k == km, ] %<>% translate( tv )
    }
    m %<>% center()
    return( m )
}

#' @export
noverlap_nbody <- function( m, km, K=.05 ) {
    run <- TRUE
    while( run ) {
        ks <- unique( km )
        f <- matrix( zero(), nrow=length( ks ), ncol=dct( m ) )
        for( i in ks ) {
            for( j  in ks ) {
                if( i == j ) next
                mi <-  m[ i == km ,]
                mj <-  m[ j == km ,]
                f[i,] %<>% vadd( repulse( mj, mi ) )
                f[j,] %<>% vadd( repulse( mi, mj ) )
            }
        }
    }
}

