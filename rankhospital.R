rankhospital <- function(state, outcome, num="best") {
        
        ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        vstates <- unique(c(ocm$State))
        testst = grep(state, vstates)
        testout = grep(outcome, c("heart attack", "heart failure", "pneumonia") )
        #Subset required variables - smaller dataset
        cb <- cbind(ocm[2],ocm[7],ocm[11],ocm[17],ocm[23])
        
        #Rename variable names for easier handling
        names(cb) <- c("hname","state", "++++++", "hfailure","pneumonia")
        
        if(!state %in% outsub1[,"state"]) {
                stop('invalid state')
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        }
        
        if(testout == "1"){
                cb1 <- cb[, c(1,2,3)]
                cb1 <- subset(cb1, state == vstates[testst])
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                nc <- nrow(cb1)
                if(num == "best"){
                       ret <- best(state, outcome)
                }
                else if(num == "worst"){
                        num = nc
                        ord <- cb1[order(cb1$++++++, cb1$hname),]
                        rank <- rank(ord$+++++++, na.last=TRUE, ties.method = "first")
                        #sort\
                }
                else if(num > nc){
                        stop(print("NA"))
                }
                else if(num > 0){
                        ord <- cb1[order(cb1$hfailure, cb1$hname),]
                        rank <- rank(ord$hfailure, na.last=TRUE, ties.method = "first")
                        #sort
                        #order
                        #return
                }
        }
        if(testout == "3"){
                cb1 <- cb[, c(1,2,5)]
                cb1 <- subset(cb1, state == vstates[testst])
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                nc <- nrow(cb1)
                if(num == "best"){
                        ret <- best(state, outcome)
                }
                else if(num == "worst"){
                        num = nc
                        ord <- cb1[order(cb1$pneumonia, cb1$hname),]
                        rank <- rank(ord$pneumonia, na.last=TRUE, ties.method = "first")
                        #sort
                        #order
                        #return
                }
                else if(num > nc){
                        stop(print("NA"))
                }
                else if(num > 0){
                        ord <- cb1[order(cb1$pneumonia, cb1$hname),]
                        rank <- rank(ord$pneumonia, na.last=TRUE, ties.method = "first")
                        #sort
                        #order
                        #return
                }
        }
        return(ret)
}