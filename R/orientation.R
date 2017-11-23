#' Unit vector in the a direction
#' @export
A <- function( d=2, a=1 ) {
    if( a > d ) error_dimension( 'Dimension too large for basis', d, a )
    o <- O( d )
    o %<>% setd( a, 1 )
    return( o )
}

#' Alias for A
#' @export
vaxis <- function( ... ) A( ... )

#' Center of m
#' @export
C <- function( m ) {
    m %<>% V()
    if( vct( m ) == 1 ) return( v )
    return( colMeans( m ) %>% V() ) # TODO too fast to abstract.
}

#' North axis
#' @export
north <- function( d=2 ) {
    vaxis( d=d, a=2 )
}

#' East axis
#' @export
east <- function( d=2 ) {
    vaxis( d=d, a=1 )
}

#' West axis
#' @export
south <- function( d=2 ) {
    return( north( d=d ) * -1 )
}

#' South axis
#' @export
west <- function( d=2 ) {
    return( east() * -1 )
}

#' Vector from C( m ) to the furthest point in m
#' @export
vhead <- function( m, o=C( m ), scalar=FALSE ) {
    h <-  m[ which.max( vdist( m, C( m ) ) ), ] %>% V()
    if( scalar ) return( vdist( h, o ) )
    return( vdif( h, o ) )
}

#' @export
axis_major <- function( m ) {
    if( vct( m ) == 1 ) return( O( dct( m ) ) )
    if( vct( m ) == 2 ) return( vdif(  m[ 1 ,],  m[ 2 ,] ) )
    ch <- p_hull( m )
    dist <- vdist( ch, ch, cross=TRUE )
    far <- which( dist == max( dist ), arr.ind=TRUE )[1,]
    h <- ch[far[1],]
    t <- ch[far[2],]
    return( vdif( h, t ) )
}

#' @export
axis_minor <- function( m ) {
    if( vct( m ) <= 2 ) return( O( dct( m ) ) )
    a <- vorth( axis_major( m ) )
    l <-  m[ which.max( vinnerp( m, a ) ), ] %>% V()
    r <-  m[ which.min( vinnerp( m, l ) ), ] %>% V()
    return( vdif( vproj( l, a ), vproj( r, a ) ) )
}


