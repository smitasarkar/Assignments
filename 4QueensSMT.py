#!/usr/bin/env python
# coding: utf-8

# In[3]:


from z3 import *

# we represent each queen by the column position
Q = [ Int(f"Q_{row + 1}") for row in range(4) ]

# Each queen is in a column {1,2,3,4}
value = [And(1 <= Q[row], Q[row] <= 4) for row in range(4)]

# At most one queen per column
column = [Distinct(Q)]

# Diagonal constraint
diagonal = [If(i == j,
               True,
            And(Q[i] - Q[j] != i - j, Q[i] - Q[j] != j - i))
            for i in range(4) for j in range(i)]

solve(value + column + diagonal)


# In[ ]:





# In[ ]:




