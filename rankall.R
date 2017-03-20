## File edited randomly to mask the original code.
rankall <- function(outcome, num="best") {
        library(data.table)
        ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        testout = grep(outcome, c("heart attack", "heart failure", "pneumonia") )
        
        cb <- cbind(ocm[2],ocm[7],ocm[11],ocm[17],ocm[23]) #Subset with required variables
        names(cb) <- c("hname","state", "hattack", "hfailure","pneumonia") #Rename variable names
        
        state <- unique(cb$state)
        states <- data.frame(state, state)
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        }
        
        if(testout == "1"){
                cb1 <- cb[, c(1,2,3)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- cb1[order(cb1$state, cb1$hname),]
                
                cb2 <- do.call(rbind,lapply(split(---,---$---),transform, Order = rank()))
                
                if(num == "best"){
                        num = 1
                        cb3 <- cb2[cb2$Order == num, ]
                }
                else if(num == "worst"){
                        num = 1
                        cbw <- do.call(rbind,lapply(split(cb1,cb1$state),transform, Order = rank(-hattack,ties.method = "first")))
                        cb3 <- cbw[cbw$Order == num, ]
                }
                else if(num > 0){
                        cb3 <- cb2[cb2$Order == num, ]
                }
                
        }
        if(testout == "2"){
                cb1 <- cb[, c(1,2,4)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- cb1[order(cb1$state, cb1$hname),]
                cb2 <- do.call(rbind,lapply(split(cb1,cb1$state),transform, Order = rank(hfailure,ties.method = "first")))
                
                if(num == "best"){
                        num = 1
                        cb3 <- cb2[cb2$Order == num, ]
                }
                else if(num == "worst"){
                        num = 1
                        cb2 <- do.call(rbind,lapply(split(---,---$---),transform, Order = rank()))
                        cb3 <- cbw[cbw$Order == num, ]
                }
                else if(num > 0){
                        cb3 <- cb2[cb2$Order == num, ]
                }
                
        }
        if(testout == "3"){
                cb1 <- cb[, c(1,2,5)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- cb1[order(cb1$state, cb1$hname),]
                cb2 <- cb2 <- do.call(rbind,lapply(split(---,---$---),transform, Order = rank()))
                
                if(num == "best"){
                        num = 1
                        cb3 <- cb2[cb2$Order == num, ]
                }
                else if(num == "worst"){
                        num = 1
                        cb2 <- do.call(rbind,lapply(split(---,---$---),transform, Order = rank()))
                        cb3 <- cbw[cbw$Order == num, ]
                }
                else if(num > 0){
                        cb3 <- cb2[cb2$Order == num, ]
                }
                
        }
        cb4 <- merge(x=cb3, y=states, by = "state", all = TRUE)
        cb5 <- cb4[order(cb4$state, cb4$hname, cb4$Order),]
        
        return(cb5[,2:1])
}
