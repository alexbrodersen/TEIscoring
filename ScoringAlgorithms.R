install.packages("devtools")

install.packages("roxygen2")
library(roxygen2)

create("TEIscoring")

args <- list(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)
do.call(DichoScoring,args)

#' Dichotomous Scoring
#'
#' Dichotomous Scoring for TEIs. Produces a value of 1 the all correct choices were marked
#' and zero incorrect choices were marked.
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' DichoScoring(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)

DichoScoring <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    if(correctCheckNum == keyNum & incorrectCheckNum == 0){
        dichotomous <- 1
    } else {
        dichotomous <- 0
    } 
    return(dichotomous)
}

#' The Morgan scoring algorithm
#'
#' Implements the scoring algorithm described in Morgan (1979). Penalizes incorrect responses and rewards correctly marked responses. Also known as the 'Middlesex Scoring Scheme', discussed in detail in Buckly-Sharp and Harris (1971).
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' Morgan(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)

Morgan <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    morgan <- correctCheckNum/keyNum - incorrectCheckNum/distractorNum
    return(morgan)
}

#' Ripkey Partial Credit Scoring
#'
#' Partial credit scoring algorithm described in Ripkey et al (1996) for TEIs.
#' Evaluates to the number of correctly marked response options divided by the total number of keyed-correct options. If more than the keyed number of options are marked, the resulting score is zero.
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' DichoScoring(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)

Ripkey <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    if(keyNum >= correctCheckNum + incorrectCheckNum){
        ripkey <- correctCheckNum/keyNum
    } else { 
        ripkey <- 0
    }
    return(ripkey)
}


#' Balanced Scoring Algorithm
#'
#' An adaptation of the Ripkey method found in Tarasowa and Auer (2013).  
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' Balanced(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)
Balanced <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
   if(keyNum >= correctCheckNum	+ incorrectCheckNum){
       balanced <- correctCheckNum/keyNum
   } else{
       balanced <- correctCheckNum/keyNum - (correctCheckNum + incorrectCheckNum -keyNum)/distractorNum;
   }
   return(balanced)
 }



#' Balanced Scoring Algorithm
#'
#' An adaptation of the Ripkey method found in Tarasowa and Auer (2013).  
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' Balanced(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)
#' 
TrueFalse <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TF <- (correctCheckNum + (distractorNum -incorrectCheckNum))/(keyNum + distractorNum)
    return(TF)
}

## TrueFalseP
TrueFalseP <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TFP <- ((correctCheckNum + (distractorNum -incorrectCheckNum)) -
            ((keyNum-correctCheckNum)+incorrectCheckNum))/(keyNum + distractorNum)
    return(TFP)
}

## Subset
Subset <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    if(incorrectCheckNum > 0){
        subset <- 0
    } else{
        subset <- correctCheckNum/keyNum
    }
    return(subset)
}

## plusMinus
plusMinus <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    plusMinus = (correctCheckNum - incorrectCheckNum)/(keyNum + distractorNum)
    return(plusMinus)
}

## MCC
MCC <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TP <- correctCheckNum
    FP <- incorrectCheckNum
    FN <- keyNum - correctCheckNum
    TN <- distractorNum -incorrectCheckNum
    nom <- TP * TN - FP * FN
    denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    mcc <- nom/denom
    return(mcc)
}
