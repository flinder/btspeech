# added TB 21 Jan 2011: return matrix sorted by beta, ratio or both
# to do: implement for other methods than SDdifratio 
"beta.importance2" <- function(out, n, topics=NA,
                              topics.compare=NA,
                              method=c("MADdifratio", "SDdifratio",
                                "difference", "largesmall"), 
                              # added TB                               
                              sortby=c("beta","ratio","both"),
                              #
                              onlypos=TRUE,
                              xtable=FALSE, ...){
  # added TB
  outmat <- c()
  #
  nclust <- ncol(out$beta)
  method = match.arg(method)
  # added TB
  sortby = match.arg(sortby)
  #
  if (is.na(topics[1])){
    topics <- 1:nclust
  }
  if (is.na(topics.compare[1])){
    topics.compare <- 1:nclust
  }
  if (method=="difference"){
    for (i in topics){
      for (j in topics.compare){
        if (j != i){
          dif <- out$beta[,i] - out$beta[,j]
          ord.indL <- order(dif, decreasing=TRUE)
          ord.indS <- order(dif, decreasing=FALSE)
          col1L <- out$beta[ord.indL,i]
          col2L <- out$beta[ord.indL,j]
          col3L <- col1L - col2L
          outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n] )
          colnames(outmatL) <- c(paste("beta", i, sep=""),
                              paste("beta", j, sep=""),
                                 "Difference")
          col1S <- out$beta[ord.indS,i]
          col2S <- out$beta[ord.indS,j]
          col3S <- col1S - col2S
          outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n] )
          colnames(outmatS) <- c(paste("beta", i, sep=""),
                                 paste("beta", j, sep=""),
                                 "Difference")
          cat(paste("\n\nTopic ", i, " vs. Topic ", j, "\n", sep=""))
          cat("  Largest Positive Differences:\n")
          print(outmatL, digits=3)
          if (!onlypos){
            cat("  Largest Negative Differences:\n")
            print(outmatS, digits=3)
          }
        }
      }
    }
  }

  if (method=="largesmall"){
    for (i in topics){
      largest.ind <- order(out$beta[,i], decreasing=TRUE)
      smallest.ind <- order(out$beta[,i], decreasing=FALSE)
      cat(paste("\n\nbeta", i, "\n", sep=""))
      cat("largest values:")
      col1 <- cbind(out$beta[largest.ind,i][1:n])
      colnames(col1) <- ""
      print(col1, digits=3)
      cat("\nsmallest values:")
      col1 <- cbind(out$beta[smallest.ind,i][1:n])
      colnames(col1) <- ""
      print(col1, digits=3)
    }
  }

  if (method=="SDdifratio"){
    for (i in topics){
      SDnoti <- apply(out$beta[,-i], 1, sd)
      avgnoti <- apply(out$beta[,-i], 1, mean)
      dif <- out$beta[,i] - avgnoti
      distSDratio <- dif / SDnoti
      # added TB
      betadistSDratio <- out$beta[,i] + distSDratio
      if (sortby=="both"){
        ord.indL <- order(betadistSDratio, decreasing=TRUE)
        ord.indS <- order(betadistSDratio, decreasing=FALSE)
      }
      if (sortby=="beta"){
        ord.indL <- order(out$beta[,i], decreasing=TRUE)
        ord.indS <- order(out$beta[,i], decreasing=FALSE)
      }
      else {
        ord.indL <- order(distSDratio, decreasing=TRUE)
        ord.indS <- order(distSDratio, decreasing=FALSE)
      }
      col1L <- out$beta[ord.indL,i]
      col2L <- avgnoti[ord.indL]
      col3L <- col1L - col2L
      col4L <- SDnoti[ord.indL]
      col5L <- distSDratio[ord.indL]
      # added TB
      col6L <- betadistSDratio[ord.indL]
      outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n], col4L[1:n],
                       col5L[1:n], col6L[1:n])
      #outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n], col4L[1:n],
      #                 col5L[1:n])
      colnames(outmatL) <- c(paste("beta", i, sep=""),
                            paste("avg. ~i", sep=""),
                            "Difference", "SD ~i", "ratio", "beta+ratio")
      #colnames(outmatL) <- c(paste("beta", i, sep=""),
      #                      paste("avg. ~i", sep=""),
      #                      "Difference", "SD ~i", "ratio")
      outtmp <- cbind(rep(i,n), outmatL)
      colnames(outtmp)[1] <- "topic"
#     colnames(outtmp)[2] <- "beta"
      # end added 
      col1S <- out$beta[ord.indS,i]
      col2S <- avgnoti[ord.indS]
      col3S <- col1S - col2S
      col4S <- SDnoti[ord.indS]
      col5S <- distSDratio[ord.indS]
      outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n], col4S[1:n],
                       col5S[1:n])
      colnames(outmatS) <- c(paste("beta", i, sep=""),
                            paste("avg. ~i", sep=""),
                            "Difference", "SD ~i", "ratio")

      cat(paste("\n\nTopic ", i, " vs. All Others\n", sep=""))
      # TB added
      cat("  Largest Positive Ratios/Betas:\n")
      print(outmatL, digits=3)
      if (!onlypos){
        cat("  Largest Negative Ratios:\n")
      #
      print(outmatS, digits=3)
      }
    outmat <- rbind(outmat, outtmp)
    }
  }

  if (method=="MADdifratio"){
    for (i in topics){
      MADnoti <- apply(as.matrix(out$beta[,topics.compare[topics.compare!=i]]),
                       1, mad)
      MADnoti[MADnoti==0] <- 1
      mediannoti <- apply(as.matrix(out$beta[,topics.compare[topics.compare!=i]]),
                          1, median)
      dif <- out$beta[,i] - mediannoti
      distMADratio <- dif / MADnoti
      criterion <- rank(distMADratio) + rank(out$beta[,i])
      ord.indL <- order(criterion, decreasing=TRUE)
      ord.indS <- order(criterion, decreasing=FALSE)
      col1L <- out$beta[ord.indL,i]
      col2L <- mediannoti[ord.indL]
      col3L <- col1L - col2L
      col4L <- MADnoti[ord.indL]
      col5L <- distMADratio[ord.indL]
      outmatL <- cbind(col1L[1:n], col2L[1:n], col3L[1:n], col4L[1:n],
                       col5L[1:n])
      colnames(outmatL) <- c(paste("beta", i, sep=""),
                            paste("med. ~i", sep=""),
                            "Difference", "MAD ~i", "ratio")
      col1S <- out$beta[ord.indS,i]
      col2S <- mediannoti[ord.indS]
      col3S <- col1S - col2S
      col4S <- MADnoti[ord.indS]
      col5S <- distMADratio[ord.indS]
      outmatS <- cbind(col1S[1:n], col2S[1:n], col3S[1:n], col4S[1:n],
                       col5S[1:n])
      colnames(outmatS) <- c(paste("beta", i, sep=""),
                            paste("med. ~i", sep=""),
                            "Difference", "MAD ~i", "ratio")

      if (!xtable){
        cat(paste("\n\nTopic ", i, " vs. ", topics.compare[topics.compare!=i],
                  "\n", sep=""))
        cat("  Largest Positive Ratios:\n")
        print(outmatL, digits=3)
        if (!onlypos){
          cat("  Largest Negative Ratios:\n")
          print(outmatS, digits=3)
        }
      }
      else{
        print(xtable(outmatL, digits=c(2,2,2,2,2,2), ...))
        if (!onlypos){
          print(xtable(outmatS, digits=c(2,2,2,2,2,2), ...))
        }
      }
    }

  }
# TB added
return(outmat)
#
}
