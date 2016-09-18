dia <- function(x){
     a <- 0
     b <- as.numeric(substr(as.character(format(x,'%Y')),3,5))
     b <- b + trunc(b/4)
     c <- 0
     m=as.numeric(format(x,"%m"))
     if (a%%4==0 & ((m==1||m==2))){
     c <- -1
     }
     d <- as.numeric(substr("622503514624", m, m))
     e <- as.numeric(format(x,"%d"))
     t <- a + b + c + d + e
     t%%7
    
}

# dia(as.Date("2012-10-01","%Y-%m-%d"))