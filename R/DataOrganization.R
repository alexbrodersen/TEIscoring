#' Number of permutations
#'
#' Calculates the number of permutatons of a given length for a larger collection of objects.
#' 
#' @param n The total number of choices
#' @param r The number of correct response options marked by the examinee
#' @keywords permutation
#' @export
#' @examples
#' Nperm(10,6)

Nperm <- function(n,r) factorial(n)/factorial(n-r)

#' Total Number of Permutations of all sizes
#'
#' Calculates the number of permutatons for a total number of objects when it is possible to select any number of them (with no repeats). This is useful for calculateing how many scoring possibilities for a  click and drag ordering item. Also allows for specifying a minimum number. Such as for items of the form: Select at least 5 most important things to do, and order them by priority.
#' 
#' @param n The total number of choices
#' @param min The total number of choices
#' @keywords permutation
#' @export
#' @examples
#' NumberPermsAnySize(10,6)

NumberPermsAnySize <- function(n,min=1){
    num <- sum(Nperm(n,min:n))
    return(num)
}

#' Subset a correlation matrix
#'
#' Create a subset of a square matrix by column indexes. Allows for either plus or minus.
#' 
#' @param mat The square matrix to subset
#' @param x The required (or unrequired) column indexes.
#' @keywords subset
#' @export
#' @examples
#' ### Select a few columns
#' subCor(mat,c(4,6,8))
#' ### Removed a few columns
#' subCor(mat,-3)

subCor <- function(mat,x){
    mat[x,x]
}

#' Code Parsed Data
#'
#' Coded Data parsed by the ParseData function in the TEI package.
#' 
#' @param parsedData A list containing the values, key, and all possibilities. Created by the ParseData function.
#' @keywords code 
#' @export
#' @examples
#' ### Pasrse the data
#' parsedData <- ParseData(resps,key)
#' 
#' ### Code the data based on the keys
#' codedData <- Code(parsedData)

Code <- function(parsedData){
    vals <- parsedData$vals
    all.vals <- parsedData$all.vals    
    colNames <- names(vals)
    key <- parsedData$key
    keyLength <- lapply(key,length)
    fin <- NULL
    total <- length(colNames)
    pb <- txtProgressBar(min = 0, max = total, style = 3)    
    p <- length(colNames)
    i <- 1
    for(i in 1:p){        
        n <- length(vals[[i]])
        y <- vals[[i]]
        als <- all.vals[[i]]        
        z <- table(als)
        keyZ <- table(c(key[[i]],als)) - 1
        zNames <- paste0(colNames[i],"_cat_",names(z))
        w <- NULL
        for(j in 1:n){
            z[] <- 0
            if(length(y[[j]]) == 0){
                z[] <- NA
            } else if(any(is.na(y[[j]]))){
                z[] <- NA
            } else{
                z[] <- table(c(y[[j]],als)) - 1
            }

            z <- as.numeric(keyZ == z)

            if(sum(keyZ) == 1){
                Z <- z[as.logical(keyZ)]
                names(Z) = zNames[as.logical(keyZ)]
            } else {
                Z <- z
                names(Z) = zNames
            }
            w <- rbind(w,Z)
        }
        

        fin <- cbind(fin,w)
        setTxtProgressBar(pb, i)
    }
    
    close(pb)
    coded <- fin
    
    return(coded)
}

#' Default Parsers
#'
#' An example (default) parser for the ParseData function.
#'
#' @param x a string to parse
#'
#' @keywords code 
#' @export
#' @examples
#' ### Pasrse the data
#' parsedData <- ParseData(resps,key)
#' 
#' ### Code the data based on the keys
#' codedData <- Code(parsedData)

StarsAndCommas <- function(x){
    strsplit(x,"[ *, ]+")
}

#' Default Parsers
#'
#' An example (default) parser for the ParseData function.
#'
#' @param x a string to parse
#'
#' @keywords code 
#' @export
#' @examples
#' ### Pasrse the data
#' parsedData <- ParseData(resps,key)
#' 
#' ### Code the data based on the keys
#' codedData <- Code(parsedData)

strsplit("GGG","")
OnlyLettersNumbers <- function(x){
    z <- strsplit(x,"[^A-Z0-9]")
    z <- lapply(z, function(x) strsplit(x,""))
    
    return(z)
}


#' Parse Data
#'
#' Parse data into a generic form.
#' 
#' @param data A data frame containing a string-encoded response options
#' @param key A data frame of the same size and shape as \code{data}, but containing the keyed-correct responses
#' @param parser A parser function for the strings. Parsers stars (*), commas (,) ,and spaces( ) by default.
#' @keywords code 
#' @export
#' @examples
#' ### Pasrse the data
#' parsedData <- ParseData(resps,key)
#' 
#' ### Code the data based on the keys
#' codedData <- Code(parsedData)

ParseData <- function(data,key,parser = OnlyLettersNumbers){
    vals <- lapply(data,function(x) parser(x))
    all.vals <- lapply(data,function(x) unique(unlist(parser(x))))
    key <- lapply(key,function(x) unique(unlist(parser(x))))
    return(list(vals = vals,all.vals = all.vals,key = key))
}


#' Smoothed Correlation Matrix
#'
#' Smooth a non positive definite matrix by a brute force algorithm.
#' 
#' @param x A matrix requiring smoothing
#' 
#' @examples
#' 
#' mat <- CorSmooth(x)

CorSmooth <- function(x, ridge = .0001){

    PD <- all(eigen(x)$values > 0)
    
    if(PD){
        message("Submitted Matrix Was already positive definite")
        return(x)
    }
    p <- nrow(x)
    evals <- eigen(x)$values
    evectors <- eigen(x)$vectors
    mat1 <- x
    mat2 <- x     
    while(!PD){
        wch <- sample(1:(p*(p-1)/2),p)
        mat1[lower.tri(mat1)][wch] <- mat1[lower.tri(mat1)][wch] + runif(p*(p-1)/2,-ridge,ridge)[wch]        
        mat1[upper.tri(mat1)] <- t(mat1)[upper.tri(mat1)]
        
        evals2 <- eigen(mat2)$values
        evals1 <- eigen(mat1)$values
        
        if(min(evals1) > min(evals2)){
            mat2 <- mat1
        } else {
            mat1 <- mat2
        }
        evals2 <- eigen(mat2)$values
        evals1 <- eigen(mat1)$values
        print(min(evals2))
        PD <- all(eigen(mat1)$values > 0)
    }
    return(mat1)
}

