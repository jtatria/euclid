#' Apply linear mapping
#' @export
lmap <- function( v, map ) {
    v %<>% V()
    if( vct( map ) != dct( v ) ) error_dimension( 'Wrong dimension for linear map', v, map )
    r <- v_w_op( v, function( v_ ) lmap_( v_, map ), d=dct( map ) )
    return( r %>% t() )
}
# > rbenchmark::benchmark( r %*% t( v ), tcrossprod( r, v ), replications = 100000 )
#              test replications elapsed relative user.self sys.self user.child sys.child
# 1       r %*% t(v)       100000   0.936    1.000     0.936    0.004          0         0
# 2 tcrossprod(r, v)       100000   1.409    1.505     1.408    0.000          0         0
lmap_ <- function( v, map ) return( map %*% t ( v ) )

#' Rotation
#' @export
rotation <- function( theta ) {
    ct <- cos( theta )
    st <- sin( theta )
    R <- matrix( c( ct, -st, st, ct ), byrow=TRUE, nrow=2 )
    return( R )
} # TODO implement Clifford algebras for generalized rotations

