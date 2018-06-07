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
    fin <- NULL
    total <- 2*length(colNames)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    for(i in 1:length(colNames)){        
        n <- length(vals[[i]])
        y <- vals[[i]]
        als <- all.vals[[i]]
        z <- table(als)
        names(z) <- paste0(colNames[i],"_cat_",names(z))
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
            w <- rbind(w,z)
        }
        fin <- cbind(fin,w)
        setTxtProgressBar(pb, i)
    }

    colNames <- names(key)
    finKey <- NULL
    for(i in 1:length(colNames)){        
        n <- length(key[[i]])
        y <- key[[i]]
        als <- all.vals[[i]]
        z <- table(als)
        names(z) <- paste0(colNames[i],"_cat_",names(z))
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
            w <- rbind(w,z)
        }
        finKey <- cbind(finKey,w)
        setTxtProgressBar(pb, total/2 + i)
    }
    close(pb)
    coded <- apply((fin == finKey),2,function(x) as.numeric(x))
    return(coded)
}

#' Default Parser
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

ParseData <- function(data,key,parser = StarsAndCommas){
    vals <- lapply(data,function(x) parser(x))
    all.vals <- lapply(data,function(x) unique(unlist(parser(x))))
    key <- lapply(key,function(x) parser(x))
    return(list(vals = vals,all.vals = all.vals,key = key))
}
