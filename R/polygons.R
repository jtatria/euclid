#' Convex hull
#' @export
#' @importFrom grDevices chull
p_hull <- function( m, idx=FALSE ) {
    ch <- chull( m ) %>% rev()
    if( idx ) return( ch )
    return( m[ ch, ] )
}

#' Is polygon P stored in counterclockwise order?
#' @export
p_is_ccw <- function( P ) { # TODO: generalize
    return( sum( diff( chull( P ) ) ) < 0 )
    # ccw <- 0
    # for( i in 1:nrow( P ) ) {
    #     j <- ( i %% nrow( P ) ) + 1
    #     p0 <- P[i,]; p1 <- P[j,]
    #     ccw %<>% `+`( p1[1] - p0[1] ) * ( p1[2] + p0[2] )
    # }
    # return( ccw < 0 )
}

#' Ensure points in P are stored in counterclockwise order
p_to_ccw <- function( P ) {
    r <- if( p_is_ccw( P ) ) P else P[ vct( P ):1, ]
}

#' Line-polygon intersection
#' @export
pl_clip <- function( P, p1, p0=c( 0, 0 ), value=c('s','v') ) {
    value <- match.arg( value )
    P %<>% p_to_ccw() # P points in ccw order
    v = vdif( p1, p0 )
    tE <- 0; tL <- 1
    for( i in 1:vct( P ) ) {
        j <- ( i %% vct( P ) ) + 1
        e0 <-  P[ i ,]; e1 <- P[ j ,]
        nv <- vorth( vdif( e1, e0 ), cw=TRUE )
        N  <- vinnerp( vdif( e0, p0 ), nv )
        D  <- vinnerp( vdif( p1, p0 ), nv )
        if( D == 0 ) { # parallel
            if( N < 0 ) return( NULL ) # lies outside poly
            else next # no intersection so no concern
        }
        if( D < 0 ) { # S crosses edge to enter
            tE <- max( N / D, tE )
            if( tE > tL ) return( NULL ) # enters after leaving; lies outside
        }
        if( D > 0 ) { # S cross edge to leave
            tL <- min( N / D, tL )
            if( tL < tE ) return( NULL ) # leaves before entering; lies outside
        }
    }
    r <- switch( value, s=c( tE, tL ), v=V( smul( vadd( p0, v ), tE ), smul( vadd( p0, v ), tL ) ) )
    return( r )
}

#' Polygon intersection
#' @export
pp_clip <- function( P0, P1 ) {
    if( is.null( P0 ) || is.null( P1 ) ) return( FALSE ) # TODO: rid of polyclip
    l0 <- list( to_l( P0 ) )
    l1 <- list( to_l( P1 ) )
    return( polyclip::polyclip( l0, l1 ) %>% length() != 0 )
} # TODO: refactor to remove polyclip

#' Size of minimal bounding parallelepiped
#' @export
p_boxsize <- function( m ) {
    a1 <- axis_major( m )
    a2 <- axis_minor( m )
    return( vnorm( a1 ) * vnorm( a2 ) )
}

#' Push m until out of X
#' @export
pp_clear_edges <- function( m, X, v=vdif( C( m ), C( X ) ), value=c('s','v','p') ) {
    value <- match.arg( value )
    P <- p_hull( m )
    X <- p_hull( X )
    X %<>% translate( -C( P ) )
    P %<>% translate( -C( P ) )

    # sort vertices in P along v
    vsP <- vproj( P, v, scalar=TRUE ) %>% order()
    # find edges in P inside X
    PinX <- vapply( vsP, function( i ) {
        lc <- pl_clip( X, p0=P[ i, ], p1=P[ ( i %% vct( P ) ) + 1,  ], value='s' )
        return( if( is.null( lc ) ) c( NA, NA ) else lc )
    }, c( 0, 0 ) )

    if( all( is.na( colSums( PinX ) ) ) ) {
        # no P edges in X, return 0, O(), or unmoved P
        r <- switch( value, s=0, v=O( dct( P ) ), p=P )
        return( r )
    }

    # get max displacement along v to make all vertices in P exit X
    ax <- axis_major( X ) %>% smul( 2 ) %>% vproj( v );
    ts <- vapply( 1:length( vsP ), function( i ) {
        if( is.na( sum( PinX[,i] ) ) ) return( 0 )
        p0 <- P[ vsP[i], ]; p1 <- P[ ( vsP[i] %% vct( P ) ) + 1, ]; ev <- vdif( p1, p0 )
        pE <- ev %>% smul( PinX[1,i] ) %>% vadd( p0 ); pL <- ev %>% smul( PinX[2,i] ) %>% vadd( p0 )
        d1 <- pl_clip( X, p0=pE, p1=translate( ax, pE ), value='s' )[2]
        d2 <- pl_clip( X, p0=pL, p1=translate( ax, pL ), value='s' )[2]
        d1 <- if( !is.null( d1 ) && d1 != 1 ) d1 else 0
        d2 <- if( !is.null( d2 ) && d2 != 1 ) d2 else 0
        return( max( d1, d2 ) )
    }, 0 ) %>% max()
    tv <- smul( ax, ts )
    r <- switch( value, s=vnorm( tv ), v=tv, p=translate( m, tv ) )
    return( r )
}

#' Push m along v until completely clear of X
#' @export
pp_clear_poly <- function( m0, X, v=C( m ), value=c( 's','v','p' ), aprox=FALSE ) {
    P <- p_hull( m0 ); X <- p_hull( X )
    sX <- axis_major( X ) %>% vnorm(); sP <- axis_major( P ) %>% vnorm()
    X %<>% translate( -C( P ) )
    P %<>% translate( -C( P ) )
    if( aprox ) {
        browser() # implement
    } else {
        tv0 <- pp_clear_edges( P, X,  v, value='v' )
        P %<>% translate( tv0 )
        tv1 <- pp_clear_edges( X, P, -v, value='v' )
        P %<>% translate( tv1 )
    }
    tv <- C( P )
    r <- switch( match.arg( value ), s=vnorm( tv ), v=tv, p=translate( m0, tv ) )
    return( r )
}

#' Push P in the direction of v until clear of all shapes in m
#' @export
pp_clear_all <- function( m0, m1, km, v=C( m0 ), value=c('s','v','p'), aprox=FALSE, sort=NULL ) {
    value <- match.arg( value )
    o <- C( m0 )
    P <- p_hull( m0 )
    X <- m1 %>% split_sort( km, sort=sort )
    while( length( X ) != 0 ) {
        if( pp_clip( P, X[[1]] ) ) P %<>% pp_clear_poly( X=X[[1]], v=v, aprox=aprox, value='p' )
        if( !aprox ) X[[1]] <- NULL # TODO: polyclip uses ints; our clearance is too accurate
        if( length( X ) != 0 ) {
            X %<>% lfilter( function( P_ ) pp_clip( P, P_ ) )
        }
    }
    tv <- vdif( C( P ), o )
    r <- switch( value, s=vnorm( tv ), v=tv, p=translate( P, tv ) )
    return( r )
}

#' Is polygon P stored in counterclockwise or clockwise order?
#' TODO: generalize to arbitrary planes
#' @export
# TODO: generalize to nd
p_is_ccw <- function( P ) {
    return( sum( diff( chull( P ) ) ) < 0 )
    # ccw <- 0
    # for( i in 1:nrow( P ) ) {
    #     j <- ( i %% nrow( P ) ) + 1
    #     p0 <- P[i,]; p1 <- P[j,]
    #     ccw %<>% `+`( p1[1] - p0[1] ) * ( p1[2] + p0[2] )
    # }
    # return( ccw < 0 )
}
