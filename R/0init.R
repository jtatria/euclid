#' @importFrom magrittr %>% %<>%
NULL
# magrittr's pipes use the '.' special variable to refer to the piped argument in function calls.
# This code registers this placeholder to silence NOTEs in R CMD checks.
if( getRversion() >= "2.15.1" )  utils::globalVariables( c( "." ) )
