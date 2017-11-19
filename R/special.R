# Special ops ---------------------------------------------------------------------------------

#' Interplate points
#' @export
interpolate <- function( P, poly=TRUE, iter=1 ) {
    if( iter == 0 && poly ) return( P )
    out <- if( poly ) {
        array( NA, dim=c( nrow( P ) * 2, ncol( P ) ) )
    } else {
        array( NA, dim=c( nrow( P ), ncol( P ), nrow( P ) ) )
    }
    for( i in 1:nrow( P ) ) {
        if( poly ) {
            out[ ( i * 2 ) - 1, ] <- P[i,]
            j <- ( i %% nrow( P ) ) + 1
            if( i == j ) next
            out[i*2,] <- C( rbind( P[i,], P[j,] ) )
        } else {
            for( j in 1:i ) {
                out[i,,j] <- out[j,,i] <- if( i == j ) P[i,] else C( rbind( P[i,], P[j,] ) )
            }
        }
    }
    if( poly ) out <- interpolate( out, poly=poly, iter=iter - 1 )
    return( out )
}

#' Line-polygon intersection
#' @export
lineclip <- function( P, p1, p0=c( 0, 0 ), scalars=FALSE ) {
    # P points in ccw order.
    P <- if( !is_ccw( P ) ) P[ nrow( P ):1, ] else P
    v = vdif( p1, p0 )
    # S = p0 + v * t : 0 < t < 1
    tE <- 0; tL <- 1
    for( i in 1:nrow( P ) ) {
        j <- ( i %% nrow( P ) ) + 1
        e0 <- P[i,]
        e1 <- P[j,]
        ev <- vdif( e1, e0 )
        nv <- orth( ev, ccw=FALSE )
        N  <- dot( vdif( e0, p0 ), nv )
        D  <- dot( vdif( p1, p0 ), nv )
        if( D == 0 ) { # parallel
            if( N < 0 ) return( NULL ) # lies outside poly
            else next # no intersection so no concern
        }
        if( D < 0 ) { # S crosses edge to enter
            tE <- max( N / D, tE )
            if( tE > tL ) return( NULL ) # enters after leaving
        }
        if( D > 0 ) { # S cross edge to leave
            tL <- min( N / D, tL )
            if( tL < tE ) return( NULL ) # leaves before entering
        }
    }
    if( scalars ) return( c( tE, tL ) )
    return( rbind( p0 + v * tE, p0 + v * tL ) )
}

#' Arranges shapes in m on layout in km according to membs vector
#' @export
# TODO Remove dependency
#' @importsFrom polyclip polyclip
compose <- function( m, membs, km, bysize=TRUE, K=.05, noverlap=TRUE ) {
    if( length( membs ) != nrow( m ) ) stop('Wrong dimension for shape mebership vector')
    if( length( unique( membs ) ) != nrow( km ) ) stop('Wrong dimension for shape position vectors')

    ks   <- unique( membs )
    km %<>% center() %>% resize()
    disp <- norms( km )
    size <- vapply( 1:length( ks ), function( k ) norms( axis_major( m[ k == membs,] ) ), 0.0 )
    for( k in ks ) {
        m0 <- m[ k == membs, ]
        chart( m0 ); points( m0 )
        m0 %<>% center() %>% resize()
        chart( m0 ); points( m0 )
        m[ k == membs, ] %<>% center() %>% resize( S=( 1 / max( size ) ) )
    }

    sort <- if( bysize ) order( -size ) else order( disp )

    if( noverlap ) occ <- NULL


    for( k in sort ) {
        m0 <- m[ membs == k, ]
        m0 %<>% position( km[k,] )
        if( noverlap ) {
            cur <- m0[ chull( m0 ), ] %>% resize( S=K, abs=TRUE ) %>% to_l()
            if( is.null( occ ) ) occ <- cur
            else {
                int <- polyclip::polyclip( cur, occ, op='intersection'  )
                if( length( int ) != 0 ) {
                    dv <- clearpolys( cur, occ, vector=TRUE )
                    m0 %<>% translate( dv )
                    cur <- m0[ chull( m0 ), ] %>% resize( S=K, abs=TRUE ) %>% to_l()
                }
            }
            occ <- polyclip::polyclip( cur, occ, op='union' )
        }
        m[ membs == k, ] <- m0
        #plotit()
        #browser()
    }
    m %<>% center() %>% resize()
    return( m )
}

#' Push m in the direction of v until m \cap X = \emptyset
#' @export
# TODO Remove dependency
#' @importsFrom polyclip polyclip
clearpolys <- function( m, X, v=C( m %>% to_m( combine=TRUE ) ), vector=FALSE ) {
    mp <- m %>% to_m( combine=TRUE ) %>% `[`( chull( . ), T )

    tdv <- c( 0, 0 )

    while( length( X ) != 0 ) {
        sort <- lapply( X, function( x ) {
            to_m( x, combine=TRUE ) %>% C() %>% vdif( C( mp ) ) %>% norms()
        } ) %>% unlist() %>% order()
        x <- X[[ sort[1] ]] %>% to_m( combine=TRUE ) %>% `[`( chull( . ), T )

        dv <- O( ncol( mp ) )
        collision <- length( polyclip::polyclip( to_l( mp ), to_l( x ), op='intersection' ) ) != 0
        if( collision ) {
            p <- mp

            x %<>% translate( -C( p ) )
            p %<>% translate( -C( p ) )
            v %<>% translate( -C( p ) )

            xd <- dot( x, v )
            pd <- dot( p, v )

            ds0 <- 0
            for( i in order( pd ) ) {
                j <- ( i %% nrow( p ) ) + 1
                if( i == j ) next
                p0 <- v( p, i ); p1 <- v( p, j )
                xrs <- lineclip( x, p0=p0, p1=p1 )
                if( !is.null( xrs ) ) {
                    ep <- xrs[1,]; lp <- xrs[2,]
                    d1 <- lineclip( x, p0=ep, p1=translate( v, ep ), scalars=TRUE )[2]
                    d2 <- lineclip( x, p0=lp, p1=translate( v, lp ), scalars=TRUE )[2]
                    d1 <- if( !is.null( d1 ) && d1 != 1 ) d1 else 0
                    d2 <- if( !is.null( d2 ) && d2 != 1 ) d2 else 0
                    ds0 <- max( ds0, d1, d2 )
                }
            }
            p %<>% translate( scalev( v, ds0 ) )

            ds1 <- 0
            for( i in order( -xd ) ) {
                j <- ( i %% nrow( x ) ) + 1
                if( i == j ) next
                p0 <- v( x, i ); p1 <- v( x, j )
                xrs <- lineclip( p, p0=p0, p1=p1 )
                if( !is.null( xrs ) ) {
                    ep <- xrs[1,]; lp <- xrs[2,]
                    d1 <- lineclip( p, p1=ep, p0=translate( -v, ep ), scalars=TRUE )[2]
                    d2 <- lineclip( p, p1=lp, p0=translate( -v, lp ), scalars=TRUE )[2]
                    d1 <- if( !is.null( d1 ) && d1 != 1 ) d1 else 0
                    d2 <- if( !is.null( d2 ) && d2 != 1 ) d2 else 0
                    ds1 <- max( ds1, d1, d2 )
                }
            }
            p %<>% translate( scalev( v, ds1 ) )

            dv <- C( p )
        }
        tdv <- tdv + dv
        mp %<>% translate( dv )
        X[[ sort[1] ]] <- NULL
        X <- if( length( X ) != 0 ) polyclip::polyclip( X, to_l( mp ), op='intersection' )
    }
    if( vector ) return( tdv )
    else return( translate( m, tdv ) )
}
