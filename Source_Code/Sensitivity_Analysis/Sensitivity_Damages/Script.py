#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr  8 10:13:06 2019

@author: mxz414
"""

# In[1]:
# ## Defining the Model
# Begin by importing the necessary libraries.  
# We also adjust some plotting options to improve the appearance of plots in your web browser.

import os
os.chdir(os.path.dirname('/Users/mxz414/Documents/Research/House_Elevation_Project/Source_code/Sensitivity_Analysis/Sensitivity_Damages/'))
print(os.getcwd())

from rhodium import *
import numpy as np
from Damag_fuction import expected_damages

sns.set()
sns.set_style('darkgrid')

# In[2]:

# Next, we define our model.  The model can be any Python function, with any number of input arguments.  The names of the input arguments will become important later.  The model can also return any number of outputs.  For this example, we will use the [lake problem](https://github.com/OpenMORDM/OpenMORDM/wiki/Defining%20a%20Model):

from scipy.optimize import brentq as root

# In[3]:

# Next, we define a Rhodium model.  A Rhodium model provides additional annotations for the function above.  In particular, we will identify all parameters (inputs) and responses (outputs) of interest.
# The following lines define the Rhodium model including parameters, responses and constrains! This is a syntax speciic for Rhodium.
# Parameter names must match the argument name in the Python function.  You can also define default values for parameters, but it will fall back to using any default values in the function definition.  Responses identify the model outputs.  In addition to providing a name for each response, you can also specify if the response is an objective to be minimized or maximized, or indicate the response is "informational" and should not be optimized.

model = Model(expected_damages)
model.parameters = [Parameter("delta_h"),Parameter("Struc_Value"),Parameter("House_Initial_Stage"),Parameter("life_span"),Parameter("disc_rate"),Parameter("mu"),Parameter("mu"),Parameter("sigma"),Parameter("xi")]
model.responses = [Response("lifetime_expected_damages", Response.MINIMIZE)]

# In[4]:

# Rhodium also supports constraints.  For example, we could define a constraint on reliability.  Note how the constraint is a valid Python expression that can reference any parameter or response.  You should in general try to keep each constraint simple, such as simple equality or inequality expressions, which will help improve the performance of the optimization algorithm.

# model.constraints = [Constraint("reliability >= 0.95")] # this is a hard constraint.

# In[5]:

# Or we could have an unconstrained problem, in which case we simply do not define any constraints:

model.constraints = []

# At this point, the model is ready for use with Rhodium.

# In[6]:

# ## Optimizing the Model

# With the model defined, we can perform a variety of analyses within Rhodium.  First we will demonstrate optimizing the model.  Rhodium will attempt the optimize the responses you defined subject to any constraints by tweaking levers.  Levers are parameters that we can control when implementing a policy.  For the lake problem, we are controlling the amount of pollution released yearly into the lake.  Thus, we will define a lever for our `pollution_limit` parameter to be 100 values between 0.0 and 0.1.

model.levers = [RealLever("delta_h",0,14,length=1)]

# In[7]:

# Next, we call `optimize` to optimize the problem.  The optimization algorithm will iteratively adjust the levers while searching for an optimal policy.  If more than one response is defined, such as in this example, then typically many Pareto optimal responses can be found.  None of the identified Pareto optimal policies is better than any other; instead they form a tradeoff among the responses, some with better performance with respect to one or more responses but worse in others.  Running the optimization below, we should find many optimal policies:

output = optimize(model, "NSGAII", 1000)
print(output)
print("Found", len(output), "optimal policies!")




# In[24]:

# Then, using the `evaluate` method, we evaluate our policy in each SOW.  To do this, we need to combine our SOWs with our fixed policy.  Using the `update` function, we update each SOW with the parameters defined in our policy, namely the fixed `pollution_limit` we specified previously.
policy = {"delta_h" :6}


# In[30]:
# ## Sensitivity Analysis
# Rhodium also automates the task of performing global and regional sensitivity analysis on the model uncertainties.  Rhodium provides a convenient wrapper on top of Python's [SALib](https://github.com/SALib/SALib/), allowing you to specify only the model, the response of interest, an optional policy you want to investigate, and details on the sensitivity analysis method.  Lets start with the Morris method to identify the sensitive parameters with respect to reliability:
model.uncertainties = [UniformUncertainty("Struc_Value",100000,500000),
                       UniformUncertainty("House_Initial_Stage",30,40),
                       UniformUncertainty("life_span",5,100),
                       UniformUncertainty("disc_rate",0.01,0.09),
                       UniformUncertainty("mu",12,25),
                       UniformUncertainty("sigma",1,5),
                       UniformUncertainty("xi",0.0001,0.9)]
SOWs = sample_lhs(model, 1000)
results = evaluate(model, update(SOWs, policy))

result = sa(model, "lifetime_expected_damages", policy=policy, method="morris", nsamples=1000, num_levels=4, grid_jump=2)
print(result)

result = sa(model, "lifetime_expected_damages", policy=policy, method="sobol", nsamples=10000)
print(result)







#fig = result.plot()
#plt.show(block=True)
#exit()
# In[33]:
# If using Sobol's method, we can also generate a "spider" or "radial" plot showing the second-order indices.  In the figure below, the first and total-order indices for each parameter are indicated by the solid circle and the outer ring, respectively, where a larger radius indicates larger effects.  The gray lines connecting the circles represents the second-order indices, with thicker lines corresponding to larger second-order effects.

fig = result.plot_sobol(threshold=0.01)
plt.show(block=True)
exit()
# In[34]:
# If you have a plot with a large number of parameters, it can be useful to visually group similar parameters.  For example:

fig = result.plot_sobol(threshold=0.01,
                        groups={"Discounting" : ["disc_rate"],"Lifespan" : ["n"]})
plt.show(block=True)





