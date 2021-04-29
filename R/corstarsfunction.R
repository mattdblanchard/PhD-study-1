#taken from:
#http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html

corstarsl <- function(x){ 
  require(Hmisc) 
  require(tidyverse)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "**", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  Rnew <- Rnew %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_all(list(~str_replace(., "0.", ".")))
  
  rownames(Rnew) <- Rnew$rowname
  
  Rnew <- Rnew %>% 
    select(-rowname)
  
  rownames(Rnew) <- renamevarnames(rownames(Rnew))
  rownames(Rnew) <- c(paste0(1:nrow(Rnew), ". ", rownames(Rnew)))
  colnames(Rnew) <- 1:ncol(Rnew)
  
  return(Rnew) 
}

corstarssigcheck <- function(x){ 
  require(Hmisc) 
  require(tidyverse)
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "**", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  Rnew <- Rnew %>% 
    rownames_to_column() %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_all(list(~str_replace(., "0.", ".")))
  
  rownames(Rnew) <- Rnew$rowname
  
  Rnew <- Rnew %>% 
    select(-rowname)

  return(Rnew) 
}
