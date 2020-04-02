#' judge location translate matrix
#' @param lengthV forget
#' @param Vectorr Vector, input(x)
#' @param judge numic, judge wert
#' @return 2-matrix of location
#' @export
makeLTM <- function(Vectorr, lengthV, judge){
  Location <- matrix(0, lengthV, 2)
  Location[,1][which(Vectorr <= judge)] <- 1
  Location[,2][which(Vectorr > judge)] <- 1
  return(Location)
}

#' function for a 2 Piecewise
#' @param fct1 Function, first function
#' @param fct2 Function, second function
#' @param Vectorr Vector, input(x)
#' @param judge numic, judge wert
#' @return Result output(y)
#' @export
fctPiecewiseFunction <- function(fct1, fct2, Vectorr, judge){
  lengthV <- length(Vectorr)
  TranslatMatrix <- makeLTM(Vectorr, lengthV, judge)
  ResultTerm <- matrix(0, lengthV, 2)
  ResultTerm[,1] <- fct1(Vectorr)
  ResultTerm[,2] <- fct2(Vectorr)
  Result <- rowSums(ResultTerm * TranslatMatrix)
  return(Result)
}

#' Finding the minimum value of the corresponding position of two equal large vectors
#' @param VctA vector, numic, first Vector
#' @param VctB vector, numic, second Vector
#' @return vector, result
#' @examples
#' A = c(1, 2, 3)
#' B = c(2, 2, 2)
#' minVector(A, B)
#' @export
minVector <- function(VctA, VctB){
  VctC = VctA - VctB
  VctD = VctB
  VctD[which(VctC < 0.0)] = VctA[which(VctC < 0.0)]
  return(VctD)
}


#' Finding the maximum value of the corresponding position of two equal large vectors
#'
#' @param VctA vector, numic, first Vector
#' @param VctB vector, numic, second Vector
#' @return vector, result
#' @examples
#' A = c(1, 2, 3)
#' B = c(2, 2, 2)
#' maxVector(A, B)
#' @export
maxVector <- function(VctA, VctB){
  VctC = VctA - VctB
  VctD = VctA
  VctD[which(VctC < 0.0)] = VctB[which(VctC < 0.0)]
  return(VctD)
}

#' Finding the minimum value of one vectors and one wert
#'
#' @param WrtA numic
#' @param VctB vector, numic
#' @return vector, result
#' @examples
#' a = 2
#' B = c(1, 2, 3)
#' maxVector(a, B)
#' @export
minSVector <- function(WrtA, VctB){
  VctC = WrtA - VctB
  VctD = VctB
  VctD[which(VctC < 0.0)] = WrtA
  return(VctD)
}

#' Finding the maximum value of one vectors and one wert
#'
#' @param WrtA numic
#' @param VctB vector, numic
#' @return vector, result
#' @examples
#' a = 2
#' B = c(1, 2, 3)
#' maxVector(a, B)
#' @export
maxSVector <- function(WrtA, VctB){
  VctC = WrtA - VctB
  VctD = VctB
  VctD[which(VctC > 0.0)] = WrtA
  return(VctD)
}
