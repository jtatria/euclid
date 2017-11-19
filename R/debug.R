debug_plot <- function( ks, km, m, membs, occ, cur ) {
    chart( km, m, occ %>% to_m( combine=TRUE ) )
    polygon( cur, border='red', lwd=2 )
    cols <- color_make()( length( ks ) )
    text( km, cex=.8, col=cols )
    for( k in ks ) {
        m0 <- m[ k == membs, ]
        polygon( m0[chull(m0),], border=cols[k] )
        text( C( m0 ), col=cols[k], labels=k, cex=.8  )
        draw_vec( km[k,], C( m0 ), col=cols[k] )
    }
    lapply( to_m( occ, combine=FALSE ), function( p ) {
        polygon( p, col=color_alpha( 'red', .1 ) )
    } )
}
