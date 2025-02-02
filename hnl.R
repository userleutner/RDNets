hnl = new.env()
hnl$new <- function (n, token = rep(50, n)) {
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
  
 # print(paste("Total tokens before tax:", sum(self$token), "Agents:", n))
  
  # Collect tax from each agent
  for (k in 1:n) {
    tax_amount = round(self$token[k] * tax_rate, 0)
    if (self$token[k] - tax_amount > 0) {
      self$token[k] = self$token[k] - tax_amount
      total_tax = total_tax + tax_amount
    }
  }
 # print(paste("Total tax collected:", total_tax))

  # Add carried-forward remainder to total tax
  total_tax <- total_tax + self$carried_forward_remainder
 # print(paste("Total tax after adding carried-forward remainder:", total_tax))

  # Redistribute the tax
  if (total_tax > 0) {
    redistribution = floor(total_tax / n)
    remainder = total_tax %% n
    
    for (k in 1:n) {
      self$token[k] = self$token[k] + redistribution
    }
   # print(paste("Redistribution amount per agent:", redistribution))
   # print(paste("Remainder to be carried forward:", remainder))

    # Store the remainder to carry forward
    self$carried_forward_remainder = remainder
  }
  
  total_tokens_after = sum(self$token)
 # print(paste("Total tokens after redistribution:", total_tokens_after))
}


hnl$iter <- function (games=10,model="winlo",region=9,progress=FALSE, tax_rate=0.1) {
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
      # print(paste("Starting game", g, "with tokens:", paste(self$token, collapse=", ")))

        for (i in seq) {
        # Get current game partner
        ci = sample(idx, 1)
        j = (i + ci) %% n
        if (j == 0) j = n

        # Ensure partner has valid index
        while (j == i) {
        ci = sample(idx, 1)
        j = (i + ci) %% n
        if (j == 0) j = n
        }
            toki=self$token[i]
            tokj=self$token[j]

 
            spoints=10
            if (progress) {
                if (min(c(toki,tokj))>25) {
                    spoints=10
                }
                if (min(c(toki,tokj))>50) {
                    spoints=15
                }
                if (min(c(toki,tokj))>100) {
                    spoints=20
                }
            } 
            if (model == "null") {
                s = c(rep(1, 5), rep(-1, 5))
                s = sample(s, 2)
                if (mean(s) == 0) {
                    A[i, j] = 0
                    A[j, i] = 0
                } else if (mean(s) > 0) {
                    A[i, j] = 1
                    A[j, i] = -1
                    if (self$token[j] > 0) {
                        # Prevent negative tokens
                        if (self$token[j] >= spoints) {
                            self$token[i] = self$token[i] + spoints
                            self$token[j] = self$token[j] - spoints
                        } else {
                            self$token[i] = self$token[i] + self$token[j]
                            self$token[j] = 0
                        }
                    }
                } else {
                    A[i, j] = -1
                    A[j, i] = 1
                    if (self$token[i] > 0) {
                        # Prevent negative tokens 
                        if (self$token[i] >= spoints) {
                            self$token[i] = self$token[i] - spoints
                            self$token[j] = self$token[j] + spoints
                        } else {
                            self$token[j] = self$token[j] + self$token[i]
                            self$token[i] = 0
                        }
                    }
                }
            } else {
                # winlo(oser) model
                if (toki + tokj == 0) {
                    A[i, j] = 0
                    A[j, i] = 0
                } else if (toki == 0) {
                    A[i, j] = -1
                    A[j, i] = 1
                } else if (tokj == 0) {
                    A[i, j] = 1
                    A[j, i] = -1
                } else {
                    # both still have token
                    s = c(rep(1, toki), rep(-1, tokj))
                    s = sample(s, 2)
                    if (mean(s) == 0) {
                        A[i, j] = 0
                        A[j, i] = 0
                    } else if (mean(s) > 0) {
                        A[i, j] = 1
                        A[j, i] = -1;

                        # Prevent negative tokens 
                        if (self$token[j] >= spoints) {
                            self$token[i] = self$token[i] + spoints;
                            self$token[j] = self$token[j] - spoints;
                        } else {
                            self$token[i] = self$token[i] + self$token[j];
                            self$token[j] = 0;
                        }
                    } else {
                        A[i, j] = -1;
                        A[j, i] = 1;

                        # Prevent negative tokens 
                        if (self$token[i] >= spoints) {
                            self$token[i] = self$token[i] - spoints;
                            self$token[j] = self$token[j] + spoints;
                        } else {
                            self$token[j] = self$token[j] + self$token[i];
                            self$token[i] = 0;
                        }
                    }
                }
            }

           # print(paste("Tokens after interaction between", i, "and", j, ":", 
                   #     "Agent", i, "=", self$token[i], "Agent", j, "=", self$token[j]))
        }
        
        self$token[self$token < 0] = 0
       # print(paste("Tokens before tax collection:", paste(self$token, collapse=", ")))

        self$tax_reinvest(tax_rate)
    }
    invisible(A)
}