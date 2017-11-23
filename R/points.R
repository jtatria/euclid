#' Interplate points
#' @export
interpolate <- function( P, poly=TRUE, iter=1, value=c('all','new') ) {
    value <- match.arg( value )
    if( iter > 1 ) value <- 'all'
    if( iter == 0 && poly ) {
        r <- switch( value, all=P, new=P[FALSE,] )
        return( r )
    }
    out <- if( poly ) {
        switch( value,
            all=array( NA, dim=c( vct( P ) * 2, dct( P ) ) ),
            new=array( NA, dim=c( vct( P ), dct( P ) ) )
        )
    } else {
        array( NA, dim=c( vct( P ), dct( P ), vct( P ) ) )
    }
    for( i in 1:vct( P ) ) {
        if( poly ) {
            if( value == 'all' ) out[ ( i * 2 ) - 1, ] <-  P[ i ,]
            j <- ( i %% vct( P ) ) + 1
            if( i == j ) next
            p <- C(  P[ c( i, j ), ] )
            if( value == 'all' ) out[i*2,] <- p
            else out[i,] <- p
        } else {
            for( j in 1:i ) {
                out[i,,j] <- out[j,,i] <- if( i == j )  P[ i ,] else C( P[ c( i, j ), ] )
            }
        }
    }
    if( poly ) out <- interpolate( out, poly=poly, iter=iter - 1 )
    return( out )
}
