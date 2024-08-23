hnl = new.env()
hnl$new <- function (n, token = rep(5, n)) {
  self = hnl
  self$token = token
  self$Coop = matrix(0, nrow = n, ncol = n)
}

# replaced nested loops with vectorized operations
hnl$coop <- function () {
  self = hnl
  n = nrow(self$Coop)
  
  lower_triangle <- lower.tri(self$Coop)
  unpaired <- which(self$Coop == 0 & lower_triangle, arr.ind = TRUE)
  
  # determine chance for each unpaired relationship
  token_pairs <- pmin(self$token[unpaired[, 1]], self$token[unpaired[, 2]])
  pair_chances <- ifelse(token_pairs > 13, 0.1, ifelse(token_pairs > 3, 0.2 * ((token_pairs - 3) / 10), 0))
  
  # pair or unpair based on calculated chances
  to_pair <- runif(length(pair_chances)) < pair_chances
  self$Coop[unpaired[to_pair, , drop = FALSE]] <- 1
  
  # unpair logic for existing pairs
  paired <- which(self$Coop == 1 & lower_triangle, arr.ind = TRUE)
  to_unpair <- runif(nrow(paired)) < 0.2
  self$Coop[paired[to_unpair, , drop = FALSE]] <- 0
}


hnl$tax_reinvest <- function(tax_rate) {
  self = hnl
  n = length(self$token)
  total_tax = 0
  
  # Collect tax from each agent
  for (k in 1:n) {
    tax_amount = round(self$token[k] * tax_rate, 0)
    if (self$token[k] - tax_amount > 0) {
      self$token[k] = self$token[k] - tax_amount
      total_tax = total_tax + tax_amount
    }
  }
  
  # strategy for the remainder, if total_tax has rest after division
  if (total_tax > 0) {
    redistribution = floor(total_tax / n)
    remainder = total_tax %% n
    
    for (k in 1:n) {
      self$token[k] = self$token[k] + redistribution
    }
   
    # distribute remainder randomly 
    if (remainder > 0) {
      extra_tokens = sample(1:n, remainder)
      for (k in extra_tokens) {
        self$token[k] = self$token[k] + 1
      }
    }
  }
}


hnl$iter <- function (games=10,model="winlo",region=9,progress=TRUE,coop=FALSE, tax_rate=0.1) {
    self = hnl
    n=length(self$token)
    idx=c(-region:-1,1:region)
    A = matrix(NA,nrow=n,ncol=n)
    if (coop) {
        self$coop()
    }
    for (g in 1:games) {
        start=sample(1:n,1)
        if (start==1) {
            seq=1:n
        } else if (start == n) {
            seq=c(n,1:(n-1))
        } else {
            seq=c(start:n,1:(start-1))
        }
        for (i in seq) {
            # get current game partner
            ci = sample(idx,1)
            if ((i+ci)<0) {
                j=n+(i+ci)
            } else if ((i+ci)>n) {
                j=(i+ci)-n
            } else if ((i+ci)==0) {
                j=n
            } else {
                j=i+ci
            }
            toki=self$token[i]
            tokj=self$token[j]
            if (coop & self$Coop[i,j] != 1) {
                if (sum(self$Coop[i,])>0) {
                    idx=which(self$Coop[i,]==1)
                    toki=toki+self$token[idx]
                }
                if (sum(self$Coop[j,])>0) {
                    idx=which(self$Coop[j,]==1)
                    tokj=tokj+self$token[idx]
                }
            }
            spoints=1
            if (progress) {
                if (min(c(toki,tokj))>5) {
                    spoints=2
                }
                if (min(c(toki,tokj))>10) {
                    spoints=3
                }
                if (min(c(toki,tokj))>20) {
                    spoints=4
                }
            }
            if (model=="null") {
                s=c(rep(1,5),rep(-1,5))
                s=sample(s,2)
                if (mean(s)==0) {
                    A[i,j]=0
                    A[j,i]=0
                } else if (mean(s) > 0) {
                    A[i,j] = 1
                    A[j,i] = -1
                    if (self$token[j]>0) {
                        self$token[i]=self$token[i]+spoints
                        self$token[j]=self$token[j]-spoints
                    }
                } else {
                    A[i,j] = -1
                    A[j,i] =  1
                    if (self$token[i]>0) {
                        self$token[i]=self$token[i]-spoints
                        self$token[j]=self$token[j]+spoints
                    }
                }
            } else {
                # winlo(oser) model
                if (toki+tokj==0 | self$Coop[i,j] == 1) {
                    A[i,ci]=0
                    A[j,i]=0
                } else if (toki == 0) {
                    A[i,j]= -1
                    A[j,i]=  1
                } else if (tokj == 0) {
                    A[i,j]=  1
                    A[j,i]= -1
                } else {
                    # both still have token
                    s=c(rep(1,toki),rep(-1,tokj))
                    s=sample(s,2)
                    if (mean(s)==0) {
                        A[i,j]=0
                        A[j,i]=0
                    } else if (mean(s) > 0) {
                        A[i,j] = 1
                        A[j,i] = -1
                        self$token[i]=self$token[i]+spoints
                        self$token[j]=self$token[j]-spoints
                        if (self$token[j]<0) {
                            idx=which(self$Coop[j,]==1)
                            self$token[idx]=self$token[idx]+self$token[j]
                            self$token[j]=0
                        }

                    } else {
                        A[i,j] = -1
                        A[j,i] =  1
                        self$token[i]=self$token[i]-spoints
                        if (self$token[i]<0) {
                            idx=which(self$Coop[i,]==1)
                            self$token[idx]=self$token[idx]+self$token[i]
                            self$token[i]=0
                        }

                        self$token[j]=self$token[j]+spoints
                    }
                }
            }
        }
        
        self$tax_reinvest(tax_rate)
    }
    invisible(A)
}

