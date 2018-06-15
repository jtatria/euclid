# Vector-Vector ops ---------------------------------------------------------------------------

#' Vector projection
#'
#' Project the elements in v over the elements in u.
#'
#' If u contains only one element, all elements in v are projected over that single element. If u
#' has the same number of elements as v, then each element in v is projected over the
#' corresponding element in u. If u has more than one element but not the same number of elements
#' as v, a non-isomorpohic error will be thrown.
#'
#' @param v      A matrix representing a vector space
#' @param u      A matrix representing a vector space with one element or the same number of elements as v
#' @param scalar Logical. If TRUE, return a vector with scalar values equal to the norms of the
#'               resulting vectors.
#'
#' @return If scalar is TRUE, a vector with the lengths of the rejections of u from v. If scalar
#'         is false, a matrix containing the projection of v over u.
#'
#' @export
vproj <- function( v, u, scalar=FALSE ) { #, cross=FALSE ) {
    if( dct( v ) != dct( u ) ) error_dimension( 'Wrong dimensions in projection', v, u )
    if( iszero( v ) || iszero( u ) ) return( if( scalar ) zero() else O( dct( v ) ) )
    if( scalar ) return( vv_s_op( v, u, vproj_s_, cross=FALSE ) ) # cross=cross ) )
    return( vv_v_op( v, u, vproj_v_, cross=FALSE ) ) # cross=cross ) )
}
vproj_s_ <- function( v, u ) vinnerp_( v, u ) / ( vnorm_( u ) * vnorm_( u ) )
vproj_v_ <- function( v, u ) smul_( u, vproj_s_( v, u ) )

#' Vector rejection
#'
#' Reject the elements in from the elements in v.
#'
#' If u contains only one element, that single element will is rejected from all elements in v. If u
#' has the same number of elements as v, then each element in u is rejeted from the
#' corresponding element in v. If u has more than one element but not the same number of elements
#' as v, a non-isomorpohic error will be thrown.
#'
#' @param v      A matrix representing a vector space
#' @param u      A matrix representing a vector space with one element or the same number of elements as v
#' @param scalar Logical. If TRUE, return a vector with scalar values equal to the norms of the
#'               resulting vectors.
#'
#' @return If scalar is TRUE, a vector with the lengths of the rejections of u from v. If scalar
#'         is false, a matrix containing the result of rejecting u from v.
#'
#' @export
vrej <- function( v, u, scalar=FALSE ) { #, cross=FALSE ) {
    if( dct( v ) != dct( u ) ) error_dimension( 'Wrong dimensions in rejection', v, u )
    if( iszero( v ) || iszero( u ) ) return( if( scalar ) zero() else O( dct( v ) ) )
    if( scalar ) return( vv_s_op( v, u, vrej_s_, cross=FALSE ) ) # cross=cross ) )
    return( vv_v_op( v, u, vrej_v_, cross=FALSE ) ) # cross=cross ) )
}
vrej_v_ <- function( v, u ) vdif_( v, vproj_v_( v, u ) )
vrej_s_ <- function( v, u ) vrej_v_( v, u ) %>% vnorm_()

#' Vector normal
#'
#' Obtain a normal vector for each element in v.
#'
#' @param v A matrix representing a vector space
#' @param cw Logical. If TRUE, compute normal in clockwise (non-standard) direction
#'
#' @return A matrix isomorphic to v containing the othogonal normal for each element in v
#'
#' @export
vorth <- function( v, cw=FALSE ) {
    if( iszero( v ) ) return( v )
    r <- if( !cw ) rotate( v, pi / 2 ) else rotate( v, ( 3 * pi ) / 2 )
    return( r )
} # TODO: find a more general way of finding normals

# Alias for vorth
#' @rdname vorth
#' @export
normal <- function( ... ) return( vorth( ... ) )

