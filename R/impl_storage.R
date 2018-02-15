# The code in this file is an attempt at abstracting away the matrix storage order from higher 
# level functions. Much testing needed to determine how this affects performance, but very 
# convenient for trying out different schemes. The rationale behind this, is that R's storage is 
# not consistent enough for use in computation and plotting: matrix storage is always column major, 
# but plotting funcitons operate row-wise. To avoid transposing pervasively, the functions in this 
# package have been implemented in row-major storage, which is less than ideal for performance 
# reasons; hence the abstraction in order to facilitate a more permanent solution later.

ORDER <- list(
    V = list(
        count  = nrow,
        coerce = rbind,
        get    = function( m, i ) return( m[i,] ),
        set    = function( m, i, x ) { m[i,] <- x; return( m ) }
    ),
    D = list(
        count  = ncol,
        coerce = cbind,
        get    = function( m, i ) return( m[,i] ),
        set    = function( m, i, x ) { m[,i] <- x; return( m ) }
    )
)

#' Vectors
#' 
#' Normalize the given matrix in order for it to be treated as a vector space following the storage
#' conventions used in this package.
#' 
#' Specially necessary for single-vector vector spaces to be treated properly.
#' 
#' @param ... Anything that can be coerced to a matrix.
#' 
#' @return A matrix representing a vector space stored in a format that can be accepted by all 
#'         functions in this package.
#'         
#' @export
V <- function( ... ) {
    return( rbind( ... ) )
}

#' Vector space cardinality
#' 
#' Returns the number of vector space elements contained in the given matrix representation of a 
#' vector space.
#' 
#' @param m A normalized matrix representing a vector space.
#' 
#' @return An integer indicating the number of vectors in the given space.
#' 
#' @export
vct <- function( m ) {
    return( nrow( m ) )
}

#' Extract vectors from vector space
#' 
#' Extracts the vectors corresponding to the values of the given index vector from the space 
#' contained in the given matrix representing a vector space.
#' 
#' @param m A normalized matrix representing a vector space
#' @param i An logical or index vector indicating the desired elements in m.
#' 
#' @return A subspace of m, containing the elements from m indicated by the entries in i.
#' 
#' @export
getv <- function( m, i ) {
    return( m[i,] )
}

#' Assign vectors to vector space
#' 
#' Assigns the values contained in v to the elements in m indicated by the values of the given 
#' index vector.
#' 
#' Elements from v must be isomorphic to elements in m.
#' 
#' @param m A normalized matrix representing a vector space.
#' @param i An logical or index vector indicating the desired elements in m.
#' @param v Anything that can be assigned to subsets of a matrix.
#' 
#' @return A copy of m, with the values in v assigned to the entries in i.
#' 
#' @export
setv <- function( m, i, v ) {
    m[i,] <- v
    return( m )
}

#' Dimensions
#' 
#' Normalize the given matrix in order for it to be treated as a basis set following the storage
#' conventions used in this package.
#' 
#' Specially necessary for single-dimension basis sets to be treated properly.
#' 
#' @param ... Anything that can be coerced to a matrix.
#' 
#' @return A matrix representing a basis set stored in a format that can be accepted by all 
#'         functions in this package.
#'         
#' @export
D <- function( ... ) {
    cbind( ... )
}

#' Vector space dimensionality
#' 
#' Returns the number of dimensions for the vector space contained in the given normalized matrix.
#' 
#' @param m A normalized matrix representing a vector space.
#' 
#' @return An integer indicating the number of dimensions in the given space.
#' 
#' @export
dct <- function( m ) {
    return( ncol( m ) )
}

#' Extract dimensions from vector space
#' 
#' Extracts the dimensions corresponding to the values of the given index vector from the space 
#' contained in the given matrix representing a vector space.
#' 
#' @param m A normalized matrix representing a vector space
#' @param i An logical or index vector indicating the desired dimensions in m.
#' 
#' @return A subspace of m, containing the dimensions from m indicated by the entries in i.
#' 
#' @export
getd <- function( m, i ) {
    return( m[,i] ) %>% D()
}

#' Assign dimensions to vector space
#' 
#' Assigns the values contained in v to the dimensions in m indicated by the values of the given 
#' index vector.
#' 
#' Dimensions from v must be isomorphic to dimensions in m.
#' 
#' @param m A normalized matrix representing a vector space.
#' @param i An logical or index vector indicating the desired dimensions in m.
#' @param v Anything that can be assigned to subsets of a matrix.
#' 
#' @return A copy of m, with the values in v assigned to the entries in i.
#' 
#' @export
setd <- function( m, i, v ) {
    m[,i] <- v
    return( m )
}

#' Coerce and normalize
#' 
#' Coerces the given objects to a vector and then normalizes them according to storage conventions 
#' in this package to be treated as elements in a vector space.
#'
#' @param ... Data.
#' 
#' @return A matrix representing a vector space, with its elements set t the value of the data 
#'         contained in the given objects, following th storage conventions used in this package.
#'         
#' @export
mk <- function( ... ) {
    return( c( ... ) %>% V() )
}
