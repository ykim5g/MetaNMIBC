for(var.y in var.names){
  # var.y <- var.names[6]
  slab <- NULL
  
  fit <- get.Meta(var.y=var.y, data = mData)
  res <- fit$mixed
  # fit$cData[,var.names]
  # fit$cData[,1:3]
  
  source("./forest.rma.v2.R")
  #  col.polygon <- c("red", "black", "blue")
  #  names(col.polygon) <- unique(fit$cData$Disease.Site)
  
  if( length(fit$mixed) > 0){
    forest.rma.v2(res, clim=c(0,1), 
                  addfit = is.addfit, ## include overall summary estimate
                  col.polygon = rev(col.polygon[ as.character(fit$cData$Disease.Site)]),
                  xlim=c(-1.55, 1.7), alim=c(0,1))
    
    basic.stat <- round(c(min(res$yi, na.rm=T), max(res$yi, na.rm=T), median(res$yi, na.rm=T)),3)
    
    string.title <- paste(var.y, " \n(k=", res$k, ")", sep="")
    string.title <- paste(var.y, " \n(k=", res$k, "; I2=", round(res$I2,2),"; H2=", round(res$H2,2),")", sep="")
    string.title <- paste(var.y, " \n(k=", res$k, "; I2=", round(res$I2,2),"; H2=", round(res$H2,2),")", 
                          "\nm=", basic.stat[1], ", M=", basic.stat[2], ", Med=", basic.stat[3], 
                          sep="")
    
    title(string.title)
  }
}
