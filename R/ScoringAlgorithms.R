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
#' Ripkey(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)

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



#' True-False Scoring Algorithm
#'
#' Formula 4 (Husa et al, 1984) or SA 3 (Domnich et al, 2015). Simply the total number of options appropriately marked divided by the number of options. Therefore, has no penalty marking an incorrect item. Equivalent to the proportion of correct responses if each choices was scored individually as a True-False item. Hence, the name.
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' TrueFalse(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)
 
TrueFalse <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TF <- (correctCheckNum + (distractorNum -incorrectCheckNum))/(keyNum + distractorNum)
    return(TF)
}

#' True-False with Penalty Scoring Algorithm
#'
#' SA 4 from Domnich et al (2015). 
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' TrueFalseP(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)

TrueFalseP <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TFP <- ((correctCheckNum + (distractorNum -incorrectCheckNum))/keyNum -
            ((keyNum-correctCheckNum) + incorrectCheckNum))/(distractorNum)
    return(TFP)
}



#' Plus/Minus Scoring Algorithm
#'
#' SA 2 from Domnich et al (2015). Recieves the addition of
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' plusMinus(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)


plusMinus <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    plusMinus <- (correctCheckNum - incorrectCheckNum)/(keyNum + distractorNum)
    return(plusMinus)
}

#' Matthews correlation coefficient
#'
#' Calculates the Matthews correlation coefficient as a scoring algorithm. 
#' 
#' @param keyNum The number of correct response options in the key.
#' @param correctCheckNum The number of correct response options marked by the examinee
#' @param incrrectCheckNum The number of incorrect response options marked by the examinee
#' @param distractorNum The number of innocrrect response options in the key.
#' @keywords scoring
#' @export
#' @examples
#' MCC(keyNum = 4, correctCheckNum = 4, incorrectCheckNum = 0,distractorNum = 4)


MCC <- function(keyNum, correctCheckNum, incorrectCheckNum,distractorNum){
    TP <- correctCheckNum
    FP <- incorrectCheckNum
    FN <- keyNum - correctCheckNum
    TN <- distractorNum -incorrectCheckNum
    num <- TP * TN - FP * FN
    denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
    mcc <- num/denom
    return(mcc)
}

