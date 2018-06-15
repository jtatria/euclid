#' Low-level workload functions.
#' The functions in this file are prime candidates for implementation in Rcpp/RcppParallel/CUDA
#' Virtually all operations in this package use these functions.
#' The order of computation is defined in 'impl_storage.R' file.

#' Scalar -> Vector ops
#' 
#' Computes the value of a scalar -> vector function. E.g. produce vectors of a set lenght in the 
#' given directions, etc.
#' 
#' @param s0 A vector of scalars.
#' @param f_ A scalar -> vector function.
#' @param d  An integer indicating the dimensionality of the resulting vectors. Needed in order to 
#'           allocate results.
#'           
#' @return A matrix containing the vector-values of f_( s0 ).
#' 
#' @export
s_v_op <- function( s0, f_, d ) {
    if( !is.vector( s0 ) ) error_not_scalar( 'Need a plain vector for scalar->vector op', s0 )
    if( length( s0 ) == 1 ) return( f_( s0 ) %>% V() )
    return( vwise( s0, function( s ) f_( s ), r=O( d ) ) )
}

#' Vector -> Scalar ops
#' 
#' Computes the value of a vector -> scalar function. E.g. norms, etc.
#' 
#' @param v0 A matrix containing a vector-space representation.
#' @param f_ A vector -> scalar function.
#' 
#' @return A vector containing the scalar values of f_( v0 ) for each element in v0.
#' 
#' @export
v_s_op <- function( v0, f_ ) {
    v0 %<>% V()
    if( vct( v0 ) == 1 ) return( f_( v0 ) )
    return( vwise( v0, function( v ) f_( v ), reduce=TRUE ) )
}

#' Vector, Scalar -> Vector ops.
#' 
#' Computes the value of a (vector,scalar) -> vector bifunction, a mapping of a vector space to 
#' itself or a bilinear form of v0. E.g. vector scaling, etc.
#' 
#' @param v0    A matrix containing a vector-space representation.
#' @param s0    A vector of scalars of length 1 or the same length as the cardinality of v0.
#' @param f_    A (vector,scalar) -> vector bifunction.
#' @param cross Logical. If TRUE, the computation is carried out for vector-scalar pairs in v0, s0.
#'              I.e. as a bilinear form of v0.
#' 
#' @return If cross is FALSE, a matrix containing a vector space isomorphic to v0.
#'         If cross is TRUE, a square matrix containing a bilinear form of v0.
#' 
#' @export
vs_v_op <- function( v0, s0, f_, cross=FALSE ) {
    return( vs_w_op( v0, s0, f_, d=dct( v0 ), cross=cross ) )
}

#' Vector, Scalar -> Vector' ops.
#' 
#' Computes the value of a (vector,scalar) -> vector' bifunction, i.e. a mapping from one vector 
#' space to another or a bilinear form of v0. E.g. scale and project, etc.
#'
#' @param v0    A matrix containing a vector-space representation.
#' @param s0    A vector of scalars of length 1 or the same length as the cardinality of v0.
#' @param f_    A (vector,scalar) -> vector bifunction.
#' @param cross Logical. If TRUE, the computation is carried out for vector-scalar pairs in v0, s0.
#'              I.e. as a bilinear form of v0.
#' 
#' @return If cross is FALSE, a matrix containing a vector space isomorphic to v0.
#'         If cross is TRUE, a square matrix containing a bilinear form of v0.
#'         
#' @export
vs_w_op <- function( v0, s0, f_, d, cross=FALSE ) {
    v0 %<>% V()
    if( !is.vector( s0 ) ) error_not_scalar( 'Need a plain vector for vector-scalar->vector op', s0 )
    if( vct( v0 ) == 1 && length( s0 ) == 1 ) return( f_( v0, s0 ) %>% V() )
    if( length( s0 ) == 1 ) return( v_v_op( v0, function( v_ ) f_( v_, s0 ) ) )
    if( vct( v0 ) == 1 ) return( s_v_op( s0, function( s_ ) f_( v0, s_ ), d=d ) )
    if( vct( v0 ) != length( s0 ) && !cross ) stop( 'Incompatible dimensions in vector-scalar op' )
    if( !cross ) return( vwise( v0, m1=s0, function( v_, s_ ) f_( v_, s_ ), r=O( d ) ) )
    out <- vapply( 1:vct( v0 ), function( i ) {
        vwise( v0, function( v ) f_( v, s0[i] ), reduce=TRUE )
    }, rep( 0, vct( v0 ) ) )
    return( out )
}

#' Vector -> Vector ops.
#' 
#' Computes the value of a simple linear mapping of one vector space to itself. E.g. vector 
#' normalization, etc.
#' 
#' @param v0 A matrix containing a vector-space representation.
#' @param f_ A vector -> vector function.
#' 
#' @return A matrix containing the vector space value of the given function.
#' 
#' @export
v_v_op <- function( v0, f_ ) {
    return( v_w_op( v0, f_, d=dct( v0 ) ) )
}

#' Vector -> Vector' ops.
#' 
#' Computes the value of a simple linear mapping of one vector space to another. E.g. vector 
#' projections in lower or higher dimensions, etc.
#' 
#' @param v0 A matrix containing a vector-space representation.
#' @param f_ A vector -> vector' function.
#' @param d  The dimensionality of the resulting vector space. Needed for allocation of result.
#' 
#' @return A matrix containing the vector space value of the given function.
#' 
#' @export
v_w_op <- function( v0, f_, d ) {
    v0 %<>% V()
    if( vct( v0 == 1 ) ) return( f_( v0 ) %>% V() )
    return( vwise( v0, function( v ) f_( v ), r=O( d ) ) )
}

#' (Vector,Vector) -> scalar ops.
#' 
#' Computes the value of a (vector,vector) -> scalar product. E.g. dot products, etc. v0 and v1 
#' need to be of the same cardinality if the cross parameter is FALSE. If TRUE, they can have 
#' arbitrary cardinality.
#' 
#' @param v0    A matrix containing a vector-space representation.
#' @param v1    A matrix containing a vector-space representation.
#' @param f_    A (vector,vector) -> scalar product function.
#' @param cross Logical. If TRUE, compute the given function across all vectro-vector pairs in v0, 
#'              v1. I.e. an 'inner' matrix product.
#'              
#' @return If cross is FALSE, a vector with the scalar values for each pair in v0, v1.
#'         If cross is TRUE, a matrix with the pairwise values for each v0,v1 pair, of size equal 
#'         to the product of the cardanility in v0 and v1. WARNING: this could easily exhaust all
#'         available memory.
#'         
#' @export
vv_s_op <- function( v0, v1, f_, cross=FALSE ) {
    v0 %<>% V(); v1 %<>% V()
    if( dct( v0 ) != dct( v1 ) ) stop( 'Wrong dimensions for vector-vector op' )
    if( vct( v0 ) == 1 && vct( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( vct( v1 ) == 1 ) return( vwise( v0, function( v ) f_( v, v1 ), reduce=TRUE ) )
    if( vct( v0 ) == 1 ) return( vwise( v1, function( v ) f_( v0, v ), reduce=TRUE ) )
    if( vct( v0 ) != vct( v1 ) && !cross ) stop( 'Non-isomorphics in vector-vector op' )
    if( !cross ) return( vwise( v0, m1=v1, function( v, u ) f_( v, u ), reduce=TRUE ) )
    out <- vapply( 1:vct( v1 ), function( i ) {
        vwise( v0, function( v ) f_( v, v1[i,] ), reduce=TRUE )
    }, rep( 0, vct( v0 ) ) )
    return( out )
}

#' (Vector,Vector) -> Vector ops.
#' 
#' Computes the value of a (vector,vector) -> vector product. E.g. vector projections and 
#' rejections, etc. v0 and v1 need to be of the same cardinality if the cross parameter is FALSE. 
#' This function produces results isomorphic to the given vector-spaces.
#' 
#' If TRUE, this function computes a tensor product, the value of which is a tensor of rank 3.
#' 
#' TENSOR PRODUCTS ARE NOT IMPLEMENTED YET.
#' 
#' @param v0    A matrix containing a vector-space representation.
#' @param v1    A matrix containing a vector-space representation.
#' @param f_    A (vector,vector) -> vector product function.
#' @param cross NOT IMPLEMENTED. Logical. If TRUE, compute the given function across all 
#'              vector-vector pairs in v0, v1, producing a higher-rank tensor.
#'              
#' @return If cross is FALSE, a matrix containing a vector space equal to the value of the given 
#'         function for each concomitant v0, v1 pair.
#'         If cross is TRUE, a higher-rank tensor containing the vector spaces corresponding to all 
#'         pairwise products between elements in v0 and v1.
#'         
#' @export
vv_v_op <- function( v0, v1, f_, cross=FALSE ) {
    return( vv_w_op( v0, v1, f_, d=dct( v0 ), cross=cross ) )
}

#' (Vector,Vector) -> Vector' ops.
#' 
#' Computes the value of a (vector,vector) -> vector' product. E.g. vector projections, rejections,
#' etc. v0 and v1 need to be of the same cardinality if the cross parameter is FALSE. This function
#' produces results of arbitrary structure, needing a dimension parameter to determine the shape of
#' the resulting value.
#' 
#' If TRUE, this function computes a tensor product, the value of which is a tensor of rank 3l i.e. 
#' a three dimensional array.
#' 
#' TENSOR PRODUCTS ARE NOT IMPLEMENTED YET.
#' 
#' @param v0    A matrix containing a vector-space representation.
#' @param v1    A matrix containing a vector-space representation.
#' @param f_    A (vector,vector) -> scalar product function.
#' @param d     Integer indicating the dimensionality of the resulting vectors.
#' @param cross NOT IMPLEMENTED. Logical. If TRUE, compute the given function across all 
#'              vector-vector pairs in v0, v1, producing a higher-rank tensor.
#'              
#' @return If cross is FALSE, a matrix containing a vector space equal to the value of the given 
#'         function for each concomitant v0, v1 pair.
#'         If cross is TRUE, a higher-rank tensor containing the vector spaces corresponding to all 
#'         pairwise products between elements in v0 and v1.
#'         
#' @export
vv_w_op <- function( v0, v1, f_, d, cross=FALSE ) {
    v0 %<>% rbind(); v1 %<>% rbind()
    if( dct( v0 ) != dct( v1 ) ) error_dimension( 'Wrong dimensions for vector-vector op', v0, v1 )
    if( vct( v0 ) == 1 && vct( v1 ) == 1 ) return( f_( v0, v1 ) )
    if( vct( v1 ) == 1 ) return( vwise( v0, function( v ) f_( v, v1 ), r=O( d ) ) )
    if( vct( v0 ) == 1 ) return( vwise( v1, function( v ) f_( v0, v ), r=O( d ) ) )
    if( vct( v0 ) != vct( v1 ) && !cross ) stop( 'Non-isomorphics in vector-vector op' )
    if( !cross ) return( vwise( v0, m1=v1, function( v, u ) f_( v, u ), r=O( d ) ) )
    stop( 'Not implemented' )  # TODO: implement tensor products.
}

#' Wrapper for vapply for vector-wise operations
#' 
#' This function enforces storage conventions and then computes the given function across all 
#' vector elements in the given matrix containing a vector-space representation. Low-level 
#' workhorse function for all operations in this package.
#' 
#' This function is only exported to facillitate the implementation of functions with different 
#' parameter structures to the ones provided in this package.
#' 
#' @param m0     A matrix containing a vector-space representation.
#' @param f_     A function to apply on each vector element in m0.
#' @param reduce Logical. TRUE if the given function is a reduction. Needed for allocation of
#'               results.
#' @param ...    Additional parameters for f_.
#' @param m1     An optional second matrix, if needed.
#' @param r      An optional result specification. Defualts to NULL, in which case the result is 
#'               allocated internally and then returned.
#'
#' @return The result of the application of f_ to all elements in the given inputs.
#' 
#' @export
vwise <- function( m0, f_, reduce=FALSE, ..., m1=NULL, r=NULL ) {
    m0 %<>% V()
    r <- if( is.null( r ) ) if( reduce ) 0.0 else O( dct( m0 ) ) else r
    if( is.null( m1 ) ) {
        out <- vapply( 1:vct( m0 ), function( i ) f_(  m0[ i ,], ... ), r )
    } else {
        m1 %<>% V()
        if( vct( m1 ) != vct( m0 ) ) error_dimension( 'Wrong dimensions for vwise arg', vct( m1 ), vct( m0 ) )
        out <- vapply( 1:vct( m0 ), function( i ) f_(  m0[ i ,],  m1[ i ,], ... ), r )
    }
    out <- if( reduce ) out else t( out )
    return( out )
}

#' Wrapper for vapply for dimension-wise operations
#' 
#' This function enforces storage conventions and then computes the given function across all 
#' dimensions in the given matrix containing a vector-space representation. Low-level 
#' workhorse function for all operations in this package.
#' 
#' This function is only exported to facillitate the implementation of functions with different 
#' parameter structures to the ones provided in this package.
#' 
#' @param m0     A matrix containing a vector-space representation.
#' @param f_     A function to apply on each dimension in m0.
#' @param reduce Logical. TRUE if the given function is a reduction. Needed for allocation of
#'               results.
#' @param ...    Additional parameters for f_.
#' @param m1     An optional second matrix, if needed.
#' @param r      An optional result specification. Defualts to NULL, in which case the result is 
#'               allocated internally and then returned.
#'
#' @return The result of the application of f_ to all dimensions in the given inputs.
#' 
#' @export
#' @export
dwise <- function( m0, f_, ..., m1=NULL, reduce=FALSE, r=NULL ) {
    m0 %<>% D()
    r <- if( is.null( r ) ) if( reduce ) 0.0 else O( dct( m0 ) ) else r
    if( is.null( m1 ) ) {
        out <- vapply( 1:dct( m0 ), function( i ) f_( getd( m0, i ), ...), r )
    } else {
        m1 %<>% D()
        if( dct( m1 ) != dct( m0 ) ) error_dimension( 'Wrong dimensions for dwise arg', dct( m1 ), dct( m0 ) )
        out <- vapply( 1:dct( m0 ), function( i ) f_( getd( m0, i ), getd( m1, i ), ... ), r )
    }
    return( out )
}


