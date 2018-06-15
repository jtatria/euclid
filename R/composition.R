
#' Arrange shapes according to the given positions.
#' 
#' Composes an arrangement of shapes according to the positions given in \code{shp}, by associating 
#' points in \code{m} to the entries in \code{shp} according to the values in the membership vector
#' (i.e. factor) \code{km}.
#' 
#' \code{shp} must contain as many elements as there are unique values in km.
#' \code{km} must be of the same length as the number of elements in m, and its values must be 
#' useful as indices into \code{shp}, i.e. they must range between 1 and the number of entries in 
#' \code{shp}.
#' 
#' Elements in \code{m} are selected from the values in \code{km} such that each group of elements 
#' in \code{m} corresponding to a given value in \code{km} is treated as a shape that will be 
#' centered (\link{center}), resized (\link{resize}) and translated (\link{translate}) according to 
#' the vectors in \code{shp}.
#' 
#' If \code{noverlap} is not \code{NULL}, it will be assumed to be a reference to a function to use 
#' to remove any overlap there may be between shapes after translation. See \link{noverlap_radial} 
#' for an example. Other functions are planned but not implemented yet.
#' 
#' @param m        A matrix representing a vector space.
#' @param shp      A matrix with the desired position of each shape.
#' @param km       A vector indicating the shape from \code{shp} each element in \code{m} belongs 
#'                 to.
#' @param noverlap A function to apply in order to remove overlap. Defaults to 
#'                 \link{noverlap_radial}.
#'                 Set to NULL to disable any overlap correction.
#' 
#' @return A matrix represnting a vector space isomorphic to \code{m} with all points 
#'         displaced according to the locations given in \code{shp}, following the associations 
#'         indicated in \code{km}.
#'         
#' @export
compose <- function( m, shp, km, noverlap=noverlap_radial ) {
    if( length( km ) != vct( m ) ) stop('Wrong dimension for shape mebership vector')
    if( length( unique( km ) ) != vct( shp ) ) stop('Wrong dimension for shape')
    ks <- unique( km )
    # center each shape in origin
    for( k in ks ) m[ k == km,  ] %<>% center() # no idea why vapply doesn't work.
    # center all shapes and resize the set to -1,1 range
    m %<>% center() %>% resize()
    # get minimum distance
    dist <- vdist( shp, shp, cross=TRUE ) %>% `[`( . != 0 ) %>% min()
    # get maximum size
    size <- vapply( ks, function( k ) m[ k == km, ] %>% axis_minor() %>% vnorm(), 0 ) %>% max()
    # center shapes and resize the arrangement according to distance and size
    shp %<>% center() %>% resize( size / dist )
    # position each shape
    for( k in ks ) m[ k == km ,] %<>% position( shp[ k ,] )
    # apply noverlap function, if any
    if( !is.null( noverlap ) ) m %<>% noverlap( km, K=.05 )
    return( m )
}

#' Remove overlap between shapes by radial displacement.
#' 
#' Attemtps to remove any overlap between the distinct shapes indicated by \code{km} by pushing 
#' each groups of points outwards from the centroid of \code{m}, in the order indicated in 
#' \code{sort} until the intersections between shapes is emprty.
#' 
#' The \code{K} parameter indicates a clearance factor that is added to each shape's convex hull to
#' detect intersections between shapes.
#' Strictly speaking, the unit for this value is the natural unit of the coordinate system of the 
#' space contained in \code{m}.
#' When using normalized coordinates such that shapes are contained within the -1,1 region of the 
#' vector space in \code{m}, this value can be interpreted as a clearance percentage, i.e. .05 is 
#' equal to a clearance of 5% around each shape.
#' This is the case when this function is used as the value for \code{noverlap} in \link{compose}.
#' See \link{resize} for additional details.
#' 
#' @param m    A matrix representig a vector space.
#' @param km   A vector of length equal to the number of elements in \code{m}, indicating the shape
#'             that each element in \code{m} belongs to.
#' @param K    A scalar indicating a required clearance between shapes. Defaults to .05 (i.e. 5% 
#'             when units are normalized, see details).
#' @param sort A characer vector of length one indicating the criterion used for displacing shapes.
#'             One of '-disp', 'disp', '-size' or 'size' to indicate ascending or descending order
#'             according to size or distance to center.
#'             
#' @return A matrix representing a vector space, isomorphic to \code{m} with all points 
#'         displaced radially in the given order, such that the shapes described by the distinct 
#'         values of\code{km} do not overlap.
#'         
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

#' #' @export
#' noverlap_nbody <- function( m, km, K=.05 ) {
#'     run <- TRUE
#'     while( run ) {
#'         ks <- unique( km )
#'         f <- matrix( zero(), nrow=length( ks ), ncol=dct( m ) )
#'         for( i in ks ) {
#'             for( j  in ks ) {
#'                 if( i == j ) next
#'                 mi <-  m[ i == km ,]
#'                 mj <-  m[ j == km ,]
#'                 f[i,] %<>% vadd( repulse( mj, mi ) )
#'                 f[j,] %<>% vadd( repulse( mi, mj ) )
#'             }
#'         }
#'     }
#' }

#' #' @export
#' border <- function( m, k=3, idx=FALSE ) {
#'     orig <- m
#'     iter <- k
#'     out <- NULL
#'     while( iter > 0 ) {
#'         lim <- p_hull( m, idx=TRUE )
#'         out <- c( out, lim )
#'         m[ lim, ] <- matrix( rep( C( m ), length( lim ) ), byrow=TRUE, nrow=length( lim ) )
#'         iter <- iter - 1
#'     }
#'     if( idx ) return( out )
#'     return( orig[ out, ] )
#' }

