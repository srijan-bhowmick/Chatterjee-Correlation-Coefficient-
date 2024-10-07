# Defining control parameters #

epsilon = 0.01
N = 1000     # Number of observations to be generated #


#####################################################################################
############## Code to generate V given U in view of Nelsen's Theorem ###############
#####################################################################################

# Defining necessary parameters #
m = ceiling(3/epsilon)    
n = m**2
S = 1:n

# Generating the permutation as needed #
P = c()
L = m*(0:(m-1))
for(a in 1:m){
  P = append(P, a + L)
}

# Randomizing slopes of every piece-wise function #
W = sample(c(1,-1), n, rep = T, prob = c(0.5, 0.5))

# Function to convert a vector of U-values to their corresponding V-values #
U_V = function(Vector){
  Output = c()    # Initializing vector to store the output #
  for(u in Vector){
    if(u==0){
      Output = append(Output, 0)    # Handling the only boundary case #
    }
    else{
      k = ceiling(n*u)    # "Piece number" of the value #
      r = k/n - u     # "Error" from the upper boundary of the piece #
      v = (P[k]+(W[k]-1)/2)/n - W[k]*r   # Calculating v #
      Output = append(Output, v) # Appending to the output vector #
    }
  }
  return(Output)
}


#####################################################################################
##################### Code to generate dependent data (U and V) #####################
#####################################################################################

dep = function(n){    # n = number of observations needed #
  U = runif(n)        # Generating U ~ Unif(0,1) #
  
  dD = data.frame(    # Creating a data-frame with U and V as columns #
    U = U,
    V = U_V(U)        #  Generating V given U #
  )
  
  return(dD)
}

dData = dep(N)   # Storing the data in a variable #


#####################################################################################
##################### Code to create independent data (X and Y) #####################
#####################################################################################

ind = function(n){      # n = number of observations needed #
  X = runif(n)          # Generating X ~ Unif(0,1) #
  Y = runif(n)          # Generating Y ~ Unif(0,1) #
  
  iD = data.frame(      # Creating a data-frame with X and Y as columns #
    X = X,
    Y = Y
  )
  
  return(iD)
}

iData = ind(N)      # Storing the data in a variable #
#iData[,1] = U      # For specific tests #

#####################################################################################
############# Code to calculate the Chatterjee Correlation Coefficient ##############
####################### given bivariate data in a data-frame ########################
#####################################################################################

# Function to handle (randomize) ties in the data #
randomize = function(D){    # D = Data #
  
  L = D[,1]         # First column of the data #
  
  # The following code assumes the entries in the first column are ordered, #
  # which is ensured naturally by the CCC calculation algorithm #
  
  i = 1             # Indicator running over the length of L #
  while(i < N){       # Stop condition #
    
    j = i           # Sub-indicator to check for repetitions #
    while(j < N && L[j+1] == L[i]){
      j = j + 1
    }               # Stops when 'j' is the last index with repetition #
    
    if(j > i){        # If there are repetitions of an observation #
      
      # Random permutation of numbers from 1 to 'no. of repetitions' #
      r = sample(j-i+1)
      # Applying the permutation to the repeated part of the data #
      D[i:j,] = D[i:j,][r,]  
    }
    
    i = j + 1     # Indicating the next unchecked index #
  }
  return(D)
}

# Chatterjee Correlation Coefficient Calculator #
CCC = function(D){      # D = Data #
  
  # Arranging the data #
  D = D[order(D[,1]),]  # Ordering the first column #
  D = randomize(D)      # Randomizing in case of ties #
  
  # Calculating ranks of the second column #
  R = rank(D[,2], ties.method = c("max"))     # |{Y: Y <= Y_i}| = r_i #
  L1 = rank(D[,2], ties.method = c("min"))-1  # |{Y: Y < Y_i}| = N - l_i #
  L2 = N - L1                                 # |{Y: Y => Y_i}| = l_i #
  
  # Calculating the coefficient #
  num = 0   # Initializing variable to store the numerator value #
  den = 0   # Initializing variable to store the denominator value #
  
  # Adding the required values to the variables #
  for(i in 1:(N-1)){
    num = num + Mod(R[i+1]-R[i])
    den = den + L1[i]*L2[i]
  }
  den = den + L1[N]*L2[N]
  
  Z = 1 - (N/2)*(num/den)     # Final value of the CCC #
  return(Z)
}


#####################################################################################
#################### Code to test for independence of given data ####################
#####################################################################################

# Defining a function to summarize essential observations #
explore = function(D){      # D = Data #
  C = CCC(D)                # CCC of Data #
  Z = sqrt(N)*Mod(C)     # |Observed value| of RV that follows N(0,0.4) #
  
  summ = data.frame(        # Initializing  a data-frame to store the summary #
    Characteristic = c("CCC", 
                       "|Z|", 
                       "p-value", 
                       "B"),
    Value = c(C,
              Z, 
              2*pnorm(Z, sd = sqrt(0.4), lower.tail = FALSE),
              qnorm(0.975, sd = sqrt(0.4))))
    
    print.data.frame(summ)  # Printing the summary #
}

explore(iData)      # Exploring independent data #
explore(dData)      ## Exploring dependent data ##
#####################################################################################