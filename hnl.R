hnl = new.env()
hnl$new <- function (n, token = rep(5, n)) {
  self = hnl
  self$token = token
  }


# --------  Tax strategy -------#
# after equal redistribution we carry the remainder to the next iteration, instead of ranomly redistributing the rest

hnl$carried_forward_remainder <- 0

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
# add carried-forward remainder to the total tax
  total_tax = total_tax + self$carried_forward_remainder
  
  # Redistribute equally
  redistribution = floor(total_tax / n)
  remainder = total_tax %% n
  
  for (k in 1:n) {
    self$token[k] = self$token[k] + redistribution
  }
   
  # update carried-forward remainder for the next round
  self$carried_forward_remainder = remainder
}

# -------- Game Iteration ------- #
hnl$iter <- function (games=10,model="winlo",region=9,progress=TRUE, tax_rate=0.1) {
    self = hnl
    n=length(self$token)
    idx=c(-region:-1,1:region)
    A = matrix(NA,nrow=n,ncol=n)
    
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
                       
                    } else {
                        A[i,j] = -1
                        A[j,i] =  1
                        self$token[i]=self$token[i]-spoints
                        self$token[j]=self$token[j]+spoints
                    }
                }
            }
        }
        
        self$tax_reinvest(tax_rate)
    }
    invisible(A)
}

