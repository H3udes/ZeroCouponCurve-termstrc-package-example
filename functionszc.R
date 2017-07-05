rm(list = ls())         #Borramos TODA la lista de variables.
library(termstrc)   
library(devtools)       
library(knitr)
library(R.utils)
bond.data <- function(TMADRE,TCF){
  
  #Reading data sets: MotherTable.csv and CF.csv
  
  Tbonds <- read.table(file = TMADRE, header = TRUE, sep = ";", dec = ",")
  TCF <- read.table(file = TCF, header = TRUE, sep = ";", dec = ",")
  
  
  #Building the class counponbonds data.frame
  
  ISIN <- as.character(Tbonds$ISIN)
  MATURITYDATE <- as.Date(Tbonds$MATURITYDATE,  format="%d/%m/%Y")
  ISSUEDATE <- as.Date(Tbonds$ISSUEDATE, format="%d/%m/%Y")
  COUPONRATE <- Tbonds$COUPONRATE
  PRICE <- Tbonds$PRICE
  ACCRUED <- Tbonds$ACCRUED
  
  CFISIN <- as.character(TCF$ISIN)
  CF <- TCF$CF
  DATE <- as.Date(TCF$DATE, format = "%d/%m/%Y")
  
  CASHFLOWS <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")
  
  TODAY <- as.Date(Tbonds$TODAY[1], format="%d/%m/%Y")
  
  DATA <- list(ISIN,MATURITYDATE,ISSUEDATE,
                COUPONRATE,PRICE,ACCRUED,CASHFLOWS,TODAY)
  
  names(DATA) <- c("ISIN","MATURITYDATE","ISSUEDATE","COUPONRATE",
                    "PRICE","ACCRUED","CASHFLOWS","TODAY")
  
  
  Bonds <- list(DATA)
  
  names(Bonds) <- c("DATA")
  class(Bonds) <- "couponbonds"
  
  # Verify if the data was loaded correctly
  
  T.Na.Bonds.TM <-is.na.data.frame(Bonds$DATA)
  T.Na.Bonds.CF <-is.na.data.frame(Bonds$DATA$CASHFLOWS)
  Na.Bonds.TM <-sum(T.Na.Bonds.TM)
  Na.Bonds.CF <-sum(T.Na.Bonds.CF)
  
  
  if (Na.Bonds.TM > 0){
    cat("\n")
    cat("The file MotherTable.csv, is corrupted. Try to change the date format to d/m/a, instead d-m-a")
    cat("\n")
  }else{
    cat("\n")
    cat("The file MotherTable.csv was loaded correctly")
    cat("\n")
  }
  
  if (Na.Bonds.CF > 0){
    cat("\n")
    cat("The file MotherTable.csv, is corrupted. Try to change the date format to d/m/a, instead d-m-a")
    cat("\n")
  }else{
    cat("\n")
    cat("The file CF.csv was loaded correctly")
    cat("\n")
  }
  
  return(Bonds)
}

ytm.data <- function(YTM,CFYTM){
  
  #Leemos los achivos: YTM.csv y CFYTM.csv
  
  YTM <- "YTM.csv"
  CFYTM <- "CFYTM.csv"
  
  TYTM <- read.table(file = YTM, header = TRUE, sep = ";", dec = ",")
  TCFYTM <- read.table(file = CFYTM, header = TRUE, sep = ";", dec = ",")
  
  ISIN <- as.character(TYTM$ISIN)
  MATURITYDATE <- as.Date(TYTM$MATURITYDATE,  format="%d/%m/%Y")
  YIELDYTM <- TYTM$YIELDYTM
  
  ISSUEDATE <- as.Date(TYTM$ISSUEDATE, format="%d/%m/%Y")
  COUPONRATE <- TYTM$COUPONRATE
  PRICE <- TYTM$PRICE
  ACCRUED <- TYTM$ACCRUED
  
  CFISIN <- as.character(TCFYTM$ISIN)
  CF <- TCFYTM$CF
  DATE <- as.Date(TCFYTM$DATE, format = "%d/%m/%Y")
  
  CASHFLOWS <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")
  
  TODAY <- as.Date(TYTM$TODAY[1], format="%d/%m/%Y")
  
  DATA <- list(ISIN,MATURITYDATE,ISSUEDATE,
                COUPONRATE,PRICE,ACCRUED,CASHFLOWS,YIELDYTM,TODAY)
  
  names(DATA) <- c("ISIN","MATURITYDATE","ISSUEDATE","COUPONRATE",
                    "PRICE","ACCRUED","CASHFLOWS","YIELDYTM","TODAY")
  
  
  Bonds <- list(DATA)
  
  names(Bonds) <- c("DATA")
  class(Bonds) <- "couponbonds"
  
  Hay.Na.YTM.TM <-suppressWarnings(is.na.data.frame(Bonds$DATA))
  Hay.Na.YTM.CF <-suppressWarnings(is.na.data.frame(Bonds$DATA$CASHFLOWS))
  Na.YTM.TM <-sum(Hay.Na.YTM.TM)
  Na.YTM.CF <-sum(Hay.Na.YTM.CF)
  
  
  if (Na.YTM.TM > 0){
    cat("\n")
    cat("The file YTM.csv, is corrupted. Try to change the date format to d/m/a, instead d-m-a")
    cat("\n")
  }else{
    cat("\n")
    cat("The file YTM.csv was loaded correctly")
    cat("\n")
  }
  
  if (Na.YTM.CF > 0){
    cat("\n")
    cat("The file CFYTM.csv, is corrupted. Try to change the date format to d/m/a, instead d-m-a")
    cat("\n")
  }else{
    cat("\n")
    cat("The file CFYTM.csv was loaded correctly")
    cat("\n")
  }
  
  return(Bonds)
}

FNSASV <- function(Bonds,t){
  NSASV <- suppressWarnings(tryCatch({
    evalWithTimeout({
      estim_nss(Bonds,c("DATA"),matrange = "all", tauconstr = list(c(0.01917808, t, 1, 0)), method = "asv")
    }, timeout = 120)}, TimeoutException = function(ex){
      auxNSASV = 0
    }, error = function(e){
      cat("\n")
      cat("The model doesn't fit for convergence speed-up parameters")
      cat("\n")
      cat("Trying fitting without speed-up parameters. This could take awhile...")
      cat("\n")                      
      tryCatch({evalWithTimeout({
        estim_nss(Bonds,c("DATA"),matrange = "all", method ="asv") 
      }, timeout = 120)}, TimeoutException = function(ex){
        auxNSASV = 0
      }, error = function(e){
        cat("\n")
        cat("The model doesn't fit...")
        auxNSASV = 0
      })
      
      
    }))
  return(NSASV)
}

FNSSV <- function(Bonds,t){
  NSSV <- suppressWarnings(tryCatch({
    evalWithTimeout({
      estim_nss(Bonds,c("DATA"),matrange = "all", tauconstr = list(c(0.01917808, t, 1, 0)), method = "sv")
    }, timeout = 120)}, TimeoutException = function(ex){
      auxNSSV = 0
    }, error = function(e){
      cat("\n")
      cat("The model doesn't fit for convergence speed-up parameters")
      cat("\n")
      cat("Trying fitting without speed-up parameters. This could take awhile...")
      cat("\n")                      
      tryCatch({evalWithTimeout({
        estim_nss(Bonds,c("DATA"),matrange = "all", method ="sv") 
      }, timeout = 120)}, TimeoutException = function(ex){
        auxNSSV = 0
      }, error = function(e){
        cat("\n")
        cat("The model doesn't fit...")
        auxNSSV = 0
      })
    }))
  return(NSSV)
}

FNS <- function(Bonds,t){
  NS <-suppressWarnings(tryCatch({
    evalWithTimeout({
      estim_nss(Bonds,c("DATA"),matrange = "all", tauconstr = list(c(0.01917808, t, 1, 0)), method = "ns")
    }, timeout = 120)}, TimeoutException = function(ex){
      auxNSASV = 0
    }, error = function(e){
      cat("\n")
      cat("The model doesn't fit for convergence speed-up parameters")
      cat("\n")
      cat("Trying fitting without speed-up parameters. This could take awhile...")
      cat("\n")                      
      tryCatch({evalWithTimeout({
        estim_nss(Bonds,c("DATA"),matrange = "all", method ="ns") 
      }, timeout = 120)}, TimeoutException = function(ex){
        auxNS = 0
      }, error = function(e){
        cat("\n")
        cat("The model doesn't fit...")
        auxNS = 0
      })
      
      
    }))
  return(NS)
}

zc.curve <- function(Bonds){
  
  #Parametros para agilizar el tiempo de convergencia de los algoritmos:
  
  auxNS <- 1
  auxNSSV <- 1
  auxNSASV <- 1
  TODAY <- as.Date(Bonds$DATA$TODAY[1])
  TODAY <- format(TODAY,"%d/%m/%Y")                  
  t <- suppressWarnings((as.numeric(max(Bonds$DATA$MATURITYDATE))-as.numeric(min(Bonds$DATA$MATURITYDATE)))/365)  #t: Represents the max maduration in years
  #Nelson Siegel
  cat("Adjusting Nelson Siegel Model")
  cat("\n")
  cat("-----------------------------------------------------")
  cat("\n")
  NS <- tryCatch({ 
    evalWithTimeout({ 
      FNS(Bonds,t); 
    }, timeout=120); 
  }, TimeoutException=function(ex) { 
    cat("Timeout. Skipping.\n"); 
  })
  cat("\n")
  
  #Nelson Siegel-Svenson
  cat("\n")
  cat("Adjusting Nelson Siegel-Svenson Model")
  cat("\n")
  cat("-----------------------------------------------------")
  cat("\n")  
  NSSV <- tryCatch({ 
    evalWithTimeout({ 
      FNSSV(Bonds,t); 
    }, timeout=180); 
  }, TimeoutException=function(ex) { 
    cat("Timeout. Skipping.\n"); 
  })
  cat("\n")
  
  
  #Adjusted Svenson
  cat("\n")
  cat("Adjusting Nelson Siegel - Adjusted Svenson Model")
  cat("\n")
  cat("-----------------------------------------------------")
  cat("\n")
  NSASV <- FNSASV(Bonds,t)
  
  cat("\n")
  #Results resume:
  
  #R square calcution and best fitted curve
  
  
  if (is.list(NS)){
    
    SumNS <- summary(NS)
    NS.R2 <- 1-(var(NS$yerrors$DATA[,2]) /var(NS$y$DATA[,2]))
    
    
    BetaNS <- SumNS$startparam
    BetaNS[5] <- "-"
    BetaNS[6] <- "-"
    BetaNS <- t(BetaNS)
    BetaNS <- t(BetaNS)
    
    ResuNS <- SumNS$gof[3:4]
    
    mu.yhat.RMSENS <- ResuNS[1]/mean((NS$yhat$DATA[,2])*100)
    
    ResuNS[3] <- mu.yhat.RMSENS
    ResuNS[4] <- NS.R2
    
    
    
    names(NS$opt_result)[1] <- paste("Nelson Siegel fit (",TODAY,")")
    plot(NS)
    
    
  }else{
    
    NS.R2 <- "NA"
    
    BetaNS <- rep("NA",4)
    BetaNS[5] <- "-"
    BetaNS[6] <- "-"
    mu.yhat.RMSENS <- "NA"
    
    ResuNS <- rep("NA",2)
    ResuNS[3] <- "NA"
    ResuNS[4] <- NS.R2
    
    
  } 
  
  if (is.list(NSSV)){
    
    SumNSSV <- summary(NSSV)
    NSSV.R2 <- 1-(var(NSSV$yerrors$DATA[,2]) /var(NSSV$y$DATA[,2]))
    
    BetaNSSV <- t(SumNSSV$startparam)
    ResuNSSV <- SumNSSV$gof[3:4]
    
    mu.yhat.RMSENSSV <- ResuNSSV[1]/mean((NSSV$yhat$DATA[,2])*100)
    ResuNSSV[3] <- mu.yhat.RMSENSSV
    ResuNSSV[4] <- NSSV.R2
    
    
    
    names(NSSV$opt_result)[1] <- paste("Nelson Siegel - Svenson fit (",TODAY,")")
    plot(NSSV)
    
  }else{
    
    NSSV.R2 <- "NA"
    BetaNSSV <- rep("NA",6)
    ResuNSSV <- rep("NA",2)
    ResuNSSV[3] <- "NA"
    ResuNSSV[4] <- NSSV.R2
    
    
    
  } 
  
  
  if (is.list(NSASV)){
    
    SumNSASV <- summary(NSASV)
    NSASV.R2 <- 1-(var(NSASV$yerrors$DATA[,2]) /var(NSASV$y$DATA[,2]))
    
    BetaNSASV <- t(SumNSASV$startparam)
    ResuNSASV <- SumNSASV$gof[3:4]
    
    mu.yhat.RMSENSASV <- ResuNSASV[1]/mean((NSASV$yhat$DATA[,2])*100)
    
    ResuNSASV[3] <- mu.yhat.RMSENSASV
    ResuNSASV[4] <- NSASV.R2
    
    names(NSASV$opt_result)[1] <- paste("Nelson Siegel - Adj. Svenson fit (",TODAY,")")
    plot(NSASV)
    
  }else{
    
    NSASV.R2 <- "NA"
    BetaNSASV <- rep("NA",6)
    ResuNSASV <- rep("NA",2)
    ResuNSASV[3] <- "NA"
    ResuNSASV[4] <- NSASV.R2
    
  }
  
  if (!is.list(NS) & !is.list(NSSV) & !is.list(NSASV))
  {
    cat("Is not possible to fit any model")
    return(NULL)
    cat("\n")
  }else{
    
    BetaTable <- t(data.frame(BetaNS,BetaNSSV,BetaNSASV))
    rownames(BetaTable) <- c("NS","NSSV","NSASV")
    colnames(BetaTable) <- c("Beta0","Beta1","Beta2","Tau1","Beta3","Tau2")
    View(BetaTable)
    cat("\n")
    cat("Coefficients resume for every model")
    cat("\n")
    print(kable(BetaTable))
    cat("\n")
    
    ResumTable <- t(data.frame(ResuNS,ResuNSSV,ResuNSASV))
    rownames(ResumTable) <- c("NS","NSSV","NSASV")
    colnames(ResumTable) <- c("RMSE-Yields (in %)","AABSE-Yields (in %)","RMSE/mu.yhat (in %)","R2")
    View(ResumTable)
    cat("\n")
    cat("Goodness of fit for each model")
    print(kable(ResumTable))
    cat("\n")
    
    write.table(ResumTable, file = "Statistic resume (Last model fitted).txt",   sep = "\t", eol = "\n", dec = ",", row.names = FALSE ,col.names = TRUE)
    write.table(BetaTable, file = "Coefficients (Last model fitted).txt",   sep = "\t", eol = "\n", dec = ",", row.names = FALSE ,col.names = TRUE)
    
  }
  
  Results <- list(NS,NSSV,NSASV,BetaTable,ResumTable)
  return(Results)
  
}