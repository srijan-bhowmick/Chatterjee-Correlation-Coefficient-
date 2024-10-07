import numpy as np
import matplotlib.pyplot as plt
import random

n = 3                           # number of sections our line [0,1] gets divided into
x = np.linspace(0, 1, 20000)  

# Section 1 :- Single Graph Display

# Function 1 - This function will be used if just want to get all lines with slope 1 

def f(i,x,j): 
    return np.where(((i/n+j)/n < x) & (x <= (j+((i+1)/n))/n), x + ((n-1)*(i-j)/n)/n, np.nan)

# Function 2 - This function can be used to plot the graph where we can randomise the slope of lines between 1 and -1.

'''
def f(i,x,j):
    m = random.choice([1,-1])
    return np.where(((i/n+j)/n < x) & (x <= (j+((i+1)/n))/n), m*x + ((n-1)*(i-j)/n)/n + ((1-m)*((i+1/2)/n+j))/n, np.nan)
'''

for j in range(n):
    for i in range(n):  
        plt.plot(f(i,x,j), x,
                #linewidth = 4, linestyle = 'dotted', alpha = 1         #Attributes can be given to the lines in the graph
                )



# Section 2 :- Double Graph Display

# This is to display the Nelson Function Graph and y =  x both in the same graph
# to explain the construction of the function by modfying y = x using same colours.

'''
def f(c,i,x,j):
    return np.where(((i/n+j)/n < x) & (x <= (j+((i+1)/n))/n), c*(x + ((n-1)*(i-j)/n)/n) + (1-c)*x ,np.nan)

for j in range(n):
    for i in range(n):  
        r = random.random()   
        g = random.random()   
        b = random.random()   
        plt.plot(f(0,i,x,j), x, color=(r,g,b),alpha = 0.4,#linewidth=4,linestyle='dotted'
        )
        plt.plot(f(1,i,x,j), x, color=(r,g,b),#alpha = 1,linewidth=4,linestyle='dotted'
        )
'''

plt.xlabel('U')
plt.ylabel('V')
plt.xticks(np.arange(0, 1+1/n, 1/n))
plt.yticks(np.arange(0, 1+1/n, 1/n))
plt.xlim(0, 1)  
plt.ylim(0, 1)
plt.grid(True)
plt.show()