debug_plot <- function( m, km=NULL, P=NULL ) {
    chart( m )
    km <- if( is.null( km ) ) rep( 1, vct( m ) ) else km
    ks <- unique( km )
    cols <- color_make()( length( ks ) )
    for( i in 1:length( ks ) ) {
        k <- ks[i]
        m0 <- m[ k == km, ]
        polygon( p_hull( m0 ), border=cols[i] )
        text( C( m0 ), col=cols[i], labels=k, cex=.8  )
    }
    if( !is.null( P ) ) {
        polygon( p_hull( P ), border='red', lwd=2 )
    }
}
