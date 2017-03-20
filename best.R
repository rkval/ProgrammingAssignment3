best <- function(state, outcome) {
        
        ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        vstates <- unique(c(ocm$State))
        testst = grep(state, vstates)
        testout = grep(outcome, c("heart attack", "heart failure", "pneumonia") )
        #Subset required variables - smaller dataset
        cb <- cbind(ocm[2],ocm[7],ocm[11],ocm[17],ocm[23])
        
        #Rename variable names for easier handling
        names(cb) <- c("hname","state", "hattack", "hfailure","pneumonia")
        
        #test value change
        #cb$hattack[cb$hname == "BANNER HEART HOSPITAL" & cb$state == "AZ"] <- 12
        
        if(!state %in% outsub1[,"state"]) {
                stop('invalid state')
        }
        else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        }  
        else if(testout == "1"){
                cb1 <- cb[, c(1,2,3)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- subset(cb1, state == vstates[testst])
                out <- with(cb1, hname[hattack == min(hattack)])
        }
        else if(testout == "2"){
                cb1 <- cb[, c(1,2,4)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- subset(cb1, state == vstates[testst])
                out <- with(cb1, hname[hfailure == min(hfailure)])
        }
        else if(testout == "3"){
                cb1 <- cb[, c(1,2,5)]
                cb1[,3] <- suppressWarnings(as.numeric(cb1[,3]))
                cb1 <- na.omit(cb1)
                cb1 <- subset(cb1, state == vstates[testst])
                out <- with(cb1, hname[pneumonia == min(pneumonia)])
        }
        ord <- out[order(out)]
        result <- ord[1:1]
        return(result)
}