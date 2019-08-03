#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 18 14:09:55 2019

@author: mxz414 
"""
# This page modifies the Lake example if in the Rhodium examples for the house elevation problem.

# In[1]:
# ## Defining the Model
# Begin by importing the necessary libraries.  We also adjust some plotting options to improve the appearance of plots in your web browser.
import os
os.chdir(os.path.dirname('/Users/mxz414/Documents/Research/House_Elevation_Project/Source_code/'))
print(os.getcwd())

from rhodium import *
import numpy as np
from house_elevation_problem_function import house_elevation_problem
from house_elevation_problem_function_GEV import house_elevation_problem_GEV
from house_elevation_problem_function_GEVMCMC import house_elevation_problem_GEVMCMC

# plotting options
# Uncomment the following line if you are using Spyder
#get_ipython().run_line_magic('matplotlib', 'inline')
sns.set()
sns.set_style('darkgrid')

# In[2]:

# Next, we define our model.  The model can be any Python function, with any number of input arguments.  The names of the input arguments will become important later.  The model can also return any number of outputs.  For this example, we will use the [lake problem](https://github.com/OpenMORDM/OpenMORDM/wiki/Defining%20a%20Model):

from scipy.optimize import brentq as root
# test: house_elevation_problem(1.00)

# In[3]:

# Next, we define a Rhodium model.  A Rhodium model provides additional annotations for the function above.  In particular, we will identify all parameters (inputs) and responses (outputs) of interest.

# The following lines define the Rhodium model including parameters, responses and constrains! This is a syntax speciic for Rhodium.
    
model = Model(house_elevation_problem)
model_GEV = Model(house_elevation_problem_GEV)
model_GEVMCMC = Model(house_elevation_problem_GEVMCMC)

model.parameters = [Parameter("Delta_Elev"),Parameter("disc_rate"),Parameter("n")]
model_GEV.parameters = [Parameter("Delta_Elev"),Parameter("disc_rate"),Parameter("n")]
model_GEVMCMC.parameters = [Parameter("Delta_Elev"),Parameter("disc_rate"),Parameter("n")]

model.responses = [Response("cost_elevation", Response.MINIMIZE),Response("flood_damage", Response.MINIMIZE),Response("flooding_reliability", Response.MAXIMIZE)]
model_GEV.responses = [Response("cost_elevation", Response.MINIMIZE),Response("flood_damage", Response.MINIMIZE),Response("flooding_reliability", Response.MAXIMIZE)]
model_GEVMCMC.responses = [Response("cost_elevation", Response.MINIMIZE),Response("flood_damage", Response.MINIMIZE),Response("flooding_reliability", Response.MAXIMIZE)]


# Parameter names must match the argument name in the Python function.  You can also define default values for parameters, but it will fall back to using any default values in the function definition.  Responses identify the model outputs.  In addition to providing a name for each response, you can also specify if the response is an objective to be minimized or maximized, or indicate the response is "informational" and should not be optimized.

# In[4]:

# Rhodium also supports constraints.  For example, we could define a constraint on reliability.  Note how the constraint is a valid Python expression that can reference any parameter or response.  You should in general try to keep each constraint simple, such as simple equality or inequality expressions, which will help improve the performance of the optimization algorithm.

# model.constraints = [Constraint("reliability >= 0.95")] # this is a hard constraint.

# In[5]:

# Or we could have an unconstrained problem, in which case we simply do not define any constraints:

model.constraints = []
model_GEV.constraints = []
model_GEVMCMC.constraints = []

# At this point, the model is ready for use with Rhodium.

# In[6]:

# ## Optimizing the Model

# With the model defined, we can perform a variety of analyses within Rhodium.  First we will demonstrate optimizing the model.  Rhodium will attempt the optimize the responses you defined subject to any constraints by tweaking levers.  Levers are parameters that we can control when implementing a policy.  For the lake problem, we are controlling the amount of pollution released yearly into the lake.  Thus, we will define a lever for our `pollution_limit` parameter to be 100 values between 0.0 and 0.1.

model.levers = [RealLever("Delta_Elev",0,14,length=1)]
model_GEV.levers = [RealLever("Delta_Elev",0,14,length=1)]
model_GEVMCMC.levers = [RealLever("Delta_Elev",0,14,length=1)]

# In[7]:

# Next, we call `optimize` to optimize the problem.  The optimization algorithm will iteratively adjust the levers while searching for an optimal policy.  If more than one response is defined, such as in this example, then typically many Pareto optimal responses can be found.  None of the identified Pareto optimal policies is better than any other; instead they form a tradeoff among the responses, some with better performance with respect to one or more responses but worse in others.  Running the optimization below, we should find many optimal policies:

output = optimize(model, "NSGAII", 1000)
output_GEV = optimize(model_GEV, "NSGAII", 1000)
output_GEVMCMC = optimize(model_GEVMCMC, "NSGAII", 1000)

print("Found", len(output), "optimal policies!")

# In[8]:

# Here, `output` is a Rhodium data set containing one or more optimal policies.  We can reference specific policies by index, such as `output[5]`, or by searching for a policy with certain traits using the `find`, `find_min`, and `find_max` methods.  For example:

#policy = output[5]
#print(policy)

#policy = output.find_min("flood_damage")
#print(policy)

#policy = output.find_min("cost_elevation")
#print(policy)

#policies = output.find("cost_elevation < 4000")
#print(policies)

policy = output.find_max("flooding_reliability")
print(policy)

policy = output.find_min("flooding_reliability")
print(policy)

policy = output_GEV.find_max("flooding_reliability")
print(policy)


# In[9]:

# As demonstrated above, a Rhodium data set acts like a list with additional methods for querying data.  Each entry (aka policy) within the dataset is a Python dictionary mapping keys to values.  The key corresponds to the name of the parameter or response we defined in the model.  For example, we can print information about a selected policy:

policy = output.find_min("cost_elevation")
print("**********************************************************")
print("Cost of Elevating the house:          ", policy["cost_elevation"])
print("Expected flood damages:               ", policy["flood_damage"])
print("Flooding Reliability:                 ", policy["flooding_reliability"])
print("**********************************************************")

# In[10]:
# We can also use the `apply` method to perform calculations on the data.  The expression can reference any keys (the parameters and responses) that are defined in the policy.

# result = output.apply("sum(cost_elevation)") # did not work for my case, I dont know why

# In[11]:
# Here, the `apply` method returns a list storing the sum of the yearly pollution limits for each policy.  If we instead use an assignment in the expression, as shown below, we can calculate and add a new field to the dataset.

#output.apply("total_pollution = sum(pollution_limit)");
#policy = output.find_min("total_pollution")

# In[12]:
# For some analyses, it can be useful to have this data in other formats, such as Pandas' `DataFrame` and Numpy's `ndarray`.  Rhodium can convert its data sets into these other formats:

df = output.as_dataframe()
dfs=df.sort_values('Delta_Elev')
dfs=dfs.reset_index(drop=True)
arr = output.as_array()
dfs['lossf']=dfs.flood_damage+dfs.cost_elevation
cb_opt_USGS=dfs['lossf'].idxmin()

df_GEV = output_GEV.as_dataframe()
dfs_GEV=df_GEV.sort_values('Delta_Elev')
dfs_GEV=dfs_GEV.reset_index(drop=True)
arr_GEV = output_GEV.as_array()
dfs_GEV['lossf']=dfs_GEV.flood_damage+dfs_GEV.cost_elevation
cb_opt_GEV=dfs_GEV['lossf'].idxmin()

df_GEVMCMC = output_GEVMCMC.as_dataframe()
dfs_GEVMCMC=df_GEVMCMC.sort_values('Delta_Elev')
dfs_GEVMCMC=dfs_GEVMCMC.reset_index(drop=True)
arr_GEVMCMC = output_GEVMCMC.as_array()
dfs_GEVMCMC['lossf']=dfs_GEVMCMC.flood_damage+dfs_GEVMCMC.cost_elevation
cb_opt_GEVMCMC=dfs_GEVMCMC['lossf'].idxmin()

# In[13]:

# ## Plotting

# While Rhodium provides many tools to manipulate, query, and explore the raw data, it is often more convenient to inspect the results visually.  For example, here are the Pareto optimal policies in a 2D scatter plot:

#fig = scatter2d(model, output)
#fig.set_size_inches(11,8)
#fig.suptitle('Tradeoffs between cost of elevation and flood damages', fontsize=20)
#ax2=fig.gca()
#ax2.scatter(0, 0, marker="*",s=100,color="black")
#ax2.scatter(house_elevation_problem(6)[0],house_elevation_problem(6)[1], marker="o",s=50,color="black")
#ax2.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1], marker="s",s=50,color="black")
#fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/scatter2d_no_brush.pdf')
#plt.show(block=True)

# In[14]: 2D TRADEOFFS CODE BY Rhodium

# This section plots 2D tradeoffs between cost of elevating the house and flood damages.
# Flood damages In this section are obtained from USGS return level data

fig = scatter2d(model, output, c="Delta_Elev",show_legend=False,
                figure_xlab="Cost of House Elevating[1,000 US$]",
                figure_ylab="Flood Damages[1,000 US$]",
                figure_clab="Elevation(ft)")

fig.set_size_inches(11,8)

fig.suptitle('Tradeoffs between elevation cost and flood damages \n Damages are based on USGS data', 
             fontsize=15)

ax2=fig.gca()

l0=ax2.scatter(0, 0, marker="*",s=100,color="black")

l1=ax2.scatter(house_elevation_problem(6)[0],house_elevation_problem(6)[1], 
               marker="o",s=50,color="black")

l2=ax2.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1],
               marker="s",s=50,color="black")

l3 = ax2.scatter([],[],c='black' ,s=30, edgecolors='none')

l4 = ax2.scatter([],[],c='black', s=100, edgecolors='none')

ln1=ax2.scatter([],[],facecolors='none', edgecolors='none')

ln2=ax2.scatter([],[],facecolors='none', edgecolors='none')

labels = ['Ideal Point','CBA Optimal Policy','FEMA Recommendation','','$\\bf{Reliability}$',
          str(round(min(df['flooding_reliability']),1)),
          str(round(max(df['flooding_reliability']),1))]

ax2.legend([l0,l1, l2,ln1,ln2,l3,l4], labels, ncol=1, frameon=False, fontsize=12,handlelength=2, 
           loc =0, borderpad = 1.8,handletextpad=1, scatterpoints = 1)

fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Tradeoffs_2D_USGS_with_elevation.pdf')

plt.show(block=True)


# In[14]: 2D TRADEOFFS CODE BY Mahkameh

# This section plots 2D tradeoffs between cost of elevating the house and flood damages.
# Flood damages In this section are obtained from USGS return level data
import matplotlib as mpl
#mpl.rcParams['grid.color'] = 'k'
#mpl.rcParams['grid.linestyle'] = ':'
#mpl.rcParams['grid.linewidth'] = 0.5
#mpl.style.use('classic')
mpl.rcParams.update(mpl.rcParamsDefault)

# The following lines will make the colormap based on reliabiliability data
mycolormap_USGS=np.zeros((len(df.flooding_reliability)),float)
mycolormap_USGS[df.flooding_reliability<0.5]=1   #>0.8

for i in range(0,len(mycolormap_USGS)):
    if df.flooding_reliability[i]>=0.5 and df.flooding_reliability[i]<0.7:
        mycolormap_USGS[i]=2  

mycolormap_USGS[df.flooding_reliability>=0.7]=3

# Use the default colormap of Rhodium for the plot
kwargs={'cmap':'None'}
kwargs["cmap"] = RhodiumConfig.default_cmap

# Make the plot
plt.scatter(df.cost_elevation,df.flood_damage,c=mycolormap_USGS,**kwargs)
plt.grid(color='gray', linestyle=':', linewidth=1)

# X and Y labels 
plt.xlabel("Cost of House Elevating[1,000 US$]")
plt.ylabel("Flood Damages[1,000 US$]")
#plt.suptitle('Tradeoffs between elevation cost and flood damages \n Damages are based on USGS data', 
 #            fontsize=15)

l0=plt.scatter(0, 0, marker="*",s=100,color="black")

l1=plt.scatter(house_elevation_problem(6)[0],house_elevation_problem(6)[1], 
               marker="o",s=100,color="black")

l2=plt.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1],
               marker="s",s=100,color="black")

p1 = plt.scatter([],[],c= RhodiumConfig.default_cmap(1), s=50,edgecolors='none')
p2 = plt.scatter([],[],c=RhodiumConfig.default_cmap(100) ,s=50, edgecolors='none')
p3 = plt.scatter([],[],c=RhodiumConfig.default_cmap(1000) ,s=50, edgecolors='none')

l4 = plt.scatter([],[],c='black', s=100, edgecolors='none')

ln1=plt.scatter([],[],facecolors='none', edgecolors='none')

ln2=plt.scatter([],[],facecolors='none', edgecolors='none')

labels = ['Ideal Point','Cost-Benefit Optimal Policy','FEMA Recommendation','','$\\bf{Reliability}$',
          'Reliability<0.5',
          '0.5<Reliability<0.7',
          'Reliability>0.7']

plt.legend([l0,l1, l2,ln1,ln2,p1,p2,p3], labels, ncol=1, frameon=False, fontsize=12,handlelength=2, 
           loc =0, borderpad = 1.8,handletextpad=1, scatterpoints = 1)

# Adding arrows:
#plt.arrow(50,0,-38,0,color="black",head_width=100/50,head_length=2) # Horizontal
#plt.arrow(0,100,0,-75,color="black",head_width=2,head_length=100/50) # Vertical 
fig = plt.gcf()

fig.set_size_inches(18.5, 10.5)  

# Saving the figure 
plt.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Tradeoffs_2D_USGS.pdf')

plt.show(block=True)


# In[14]: 2D TRADEOFFS

# This section plots 2D tradeoffs between cost of elevating the house and flood damages.
# Flood damages In this section are obtained from USGS return level data

# The following lines will make the colormap based on reliabiliability data
mycolormap_GEV=np.zeros((len(df_GEV.flooding_reliability)),float)
mycolormap_GEV[df_GEV.flooding_reliability<0.5]=1   #>0.8

for i in range(0,len(mycolormap_GEV)):
    if df_GEV.flooding_reliability[i]>=0.5 and df_GEV.flooding_reliability[i]<0.7:
        mycolormap_GEV[i]=2  

mycolormap_GEV[df_GEV.flooding_reliability>=0.7]=3

# Use the default colormap of Rhodium for the plot
kwargs={'cmap':'None'}
kwargs["cmap"] = RhodiumConfig.default_cmap

# Make the plot
plt.scatter(df_GEV.cost_elevation,df_GEV.flood_damage,c=mycolormap_GEV,**kwargs)

# X and Y labels 
plt.xlabel("Cost of House Elevating[1,000 US$]")
plt.ylabel("Flood Damages[1,000 US$]")
plt.suptitle('Tradeoffs between elevation cost and flood damages \n Damages are based on USGS data', 
             fontsize=15)

l0=plt.scatter(0, 0, marker="*",s=100,color="black")

l1=plt.scatter(house_elevation_problem(6)[0],house_elevation_problem(6)[1], 
               marker="o",s=100,color="black")

l2=plt.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1],
               marker="s",s=100,color="black")

p1 = plt.scatter([],[],c= RhodiumConfig.default_cmap(1), s=50,edgecolors='none')
p2 = plt.scatter([],[],c=RhodiumConfig.default_cmap(100) ,s=50, edgecolors='none')
p3 = plt.scatter([],[],c=RhodiumConfig.default_cmap(1000) ,s=50, edgecolors='none')

l4 = plt.scatter([],[],c='black', s=100, edgecolors='none')

ln1=plt.scatter([],[],facecolors='none', edgecolors='none')

ln2=plt.scatter([],[],facecolors='none', edgecolors='none')

labels = ['Ideal Point','Cost-Benefit Optimal Policy','FEMA Recommendation','','$\\bf{Reliability}$',
          'Reliability<0.5',
          '0.5<Reliability<0.7',
          'Reliability>0.7']

plt.legend([l0,l1, l2,ln1,ln2,p1,p2,p3], labels, ncol=1, frameon=False, fontsize=12,handlelength=2, 
           loc =0, borderpad = 1.8,handletextpad=1, scatterpoints = 1)

# Adding arrows:
plt.arrow(50,0,-38,0,color="black",head_width=100/50,head_length=2) # Horizontal
plt.arrow(0,100,0,-75,color="black",head_width=2,head_length=100/50) # Vertical 

# Saving the figure 
plt.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Tradeoffs_2D_GEV.pdf')

plt.show(block=True)



# In[14]: 2D TRADEOFFS

# This section plots 2D tradeoffs between cost of elevating the house and flood damages.
# Flood damages In this section are obtained from USGS return level data
#plt.figure(facecolor="white")
#import matplotlib.style
import matplotlib as mpl
#mpl.rcParams['grid.color'] = 'k'
#mpl.rcParams['grid.linestyle'] = ':'
#mpl.rcParams['grid.linewidth'] = 0.5
#mpl.style.use('classic')
mpl.rcParams.update(mpl.rcParamsDefault)
fs=5
ls=4

# The following lines will make the colormap based on reliabiliability data

def createmycolormap(x):
    z=np.zeros((len(x),),float)
    for i in range(0,len(x)):
        if x[i]<=0.25:
            z[i]=0
        elif x[i]<=0.5:
            z[i]=25
        elif x[i]<=0.75:
            z[i]=50
        elif x[i]<=1:
            z[i]=75
        else:
            print('THERE IS A PROBLEM HERE')
    return(z)


# Use the default colormap of Rhodium for the plot


mycolormap_GEVMCMC=np.zeros((len(dfs_GEVMCMC.flooding_reliability)),float)
mycolormap_GEVMCMC[dfs_GEVMCMC.flooding_reliability<0.5]=1   #>0.8

for i in range(0,len(mycolormap_GEVMCMC)):
    if dfs_GEVMCMC.flooding_reliability[i]>=0.5 and dfs_GEVMCMC.flooding_reliability[i]<0.7:
        mycolormap_GEVMCMC[i]=2  

mycolormap_GEVMCMC[dfs_GEVMCMC.flooding_reliability>=0.7]=3


#kwargs={'cmap':'None'}
#kwargs["cmap"] = RhodiumConfig.default_cmap
cm= mpl.colors.ListedColormap(["crimson","gold","limegreen","navy"])    # "navy", "crimson", "limegreen", "gold"])
#cm="rainbow"
fig=plt.figure(figsize=(3.94,2.43))
plt.plot(dfs_GEVMCMC.cost_elevation,dfs_GEVMCMC.flood_damage,'-',color="gray",zorder=-1)
plt.scatter(dfs_GEVMCMC.cost_elevation,dfs_GEVMCMC.flood_damage,s=20,c=createmycolormap(dfs_GEVMCMC.flooding_reliability),zorder=1,cmap=cm)

#plt.grid(color='gray', linestyle=':', linewidth=1)

# X and Y labels 
plt.xlabel("Cost of House Elevating [1,000 US$]",fontsize=fs)
plt.ylabel("Discounted Costs of Total Flood Damages [1,000 US$]",fontsize=fs)

plt.xlim(-2,173)
plt.ylim(-100,4000)
#plt.suptitle('Tradeoffs between elevation cost and flood damages \n Damages are based on USGS data', 
#             fontsize=15)

#plt.scatter(df_GEV.cost_elevation,df_GEV.flood_damage,c=mycolormap_GEV,s=20,**kwargs)



mycolormap_USGS=np.zeros((len(dfs.flooding_reliability)),float)
mycolormap_USGS[dfs.flooding_reliability<0.5]=1   #>0.8

for i in range(0,len(mycolormap_USGS)):
    if dfs.flooding_reliability[i]>=0.5 and dfs.flooding_reliability[i]<0.7:
        mycolormap_USGS[i]=2  

mycolormap_USGS[dfs.flooding_reliability>=0.7]=3



plt.plot(dfs.cost_elevation,dfs.flood_damage,'-',color="gray",zorder=-1)

ccc=plt.scatter(dfs.cost_elevation,dfs.flood_damage,c=createmycolormap(dfs.flooding_reliability),s=20,zorder=1,cmap=cm)
ax=fig.gca()
ax2 = ax.twiny()
ax.tick_params(axis = 'both',labelsize = ls)


l0=plt.scatter(1, 2, marker="*",s=30,color="blue")

l1=plt.scatter(dfs['cost_elevation'][cb_opt_USGS],dfs['flood_damage'][cb_opt_USGS], 
               marker="o",s=30,color="black")

l2=plt.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1],
               marker="s",s=30,color="black")

#plt.scatter(house_elevation_problem_GEVMCMC(12)[0],house_elevation_problem_GEVMCMC(12)[1],
#               marker="o",s=30,color="black")


p1 = plt.scatter([],[],c= RhodiumConfig.default_cmap(1), s=20,edgecolors='none')
p2 = plt.scatter([],[],c=RhodiumConfig.default_cmap(100) ,s=20, edgecolors='none')
p3 = plt.scatter([],[],c=RhodiumConfig.default_cmap(1000) ,s=20, edgecolors='none')

l4 = plt.scatter([],[],c='black', s=100, edgecolors='none')

ln1=plt.scatter([],[],facecolors='none', edgecolors='none')

ln2=plt.scatter([],[],facecolors='none', edgecolors='none')

ln3=plt.scatter([],[],facecolors='none', edgecolors='none')

ln4=plt.scatter([],[],facecolors='none', edgecolors='none')
ln5=plt.scatter([],[],facecolors='black', edgecolors='none',marker="o",s=3)
ln6=plt.scatter([],[],facecolors='black', edgecolors='none',marker="o",s=3*14)

labels = ['Infeasible Ideal Point','Minimizing Discounted Total Costs',"FEMA Recommendation"]#'','$\\bf{Probability\ of\ avoiding\ flooding}$','$\\bf{during\ a\ 30-year\ period}$']
#labels = ['Ideal Point','Cost-Benefit Optimal Policy',"FEMA Recommendation",'','$\\bf{Probability\ of\ avoiding\ flooding\ during\ a\ 30-year\ period}$']

          #'Reliability<0.5',
          #'0.5<Reliability<0.7',
          #'Reliability>0.7']#,'','$\\bf{Elevation(ft)}$','1','14']

plt.legend([l0,l1, l2], labels, ncol=1, frameon=False, fontsize=fs,handlelength=2, 
           loc =0,  bbox_to_anchor=(1, 0.9), borderpad = 1.8,handletextpad=1, scatterpoints = 1)



ax2.set_xlim(ax.get_xlim())
#x1=ax2.set_xlim(ax.get_xlim())[0]
#x2=ax2.set_xlim(ax.get_xlim())[1]
x1=dfs.cost_elevation[0]
x2=dfs.cost_elevation[len(dfs.cost_elevation)-1]
ax2labs=np.zeros((15,),float)
for i in range(1,15):
    ax2labs[i]=(i)*((x2-x1)/14)

#ax2.set_xticks(ax2labs)
ax2.set_xticks(dfs.cost_elevation[[0,30,40,50,60,70,80,90,100]])
ax2.tick_params(axis = 'x', labelsize = ls)

#ax2.set_xticklabels(['0','1','2','3','4','5','6','7','8','9','10','11','12','13','14'])


#list1=np.linspace(0,14,15)+14.23
list1=dfs.Delta_Elev[[0,30,40,50,60,70,80,90,100]]+14.23
list2=[round(e,1) for e in list1]
str1 = [str(e) for e in list2]

ax2.set_xticklabels(str1,rotation=0)

ax2.set_xlabel('Elevation (ft)',fontsize=fs)

# Adding arrows:
plt.text(10,500,'current USGS \n flood map using \n spatial model and \n neglecting uncertainty',fontsize=fs)
#plt.text(150,100,'GEV',fontsize=12)
plt.text(10,3000,'using river gage \n observations and accounting \n for key uncertainties',fontsize=fs)
plt.text(2,3800,'Initial house elevation',fontsize=fs)
plt.text(house_elevation_problem(10)[0]+2,3800,'FEMA base flood elevation',fontsize=fs)
plt.arrow(house_elevation_problem(10)[0],4000-100,0,100,color="black",head_width=2,head_length=50,length_includes_head=True) # Horizontal
plt.arrow(0,4000-100,0,100,color="black",head_width=2,head_length=50,length_includes_head=True) # Vertical 
plt.arrow(5,1500,-6,350-1500,color="black",head_width=2,head_length=50,length_includes_head=True) # Vertical 

plt.text(5,1500,'house value',fontsize=fs)
plt.text(90,2200,'Probability of avoiding flooding \n during a 30-year period',fontsize=fs)

fig = plt.gcf()
# Saving the figure 


from matplotlib.colors import LinearSegmentedColormap

#cm = LinearSegmentedColormap.from_list('my_list', [(1, 0, 0), (0, 1, 0), (0, 0, 1)], N=4)

cax = fig.add_axes([0.65, .4, 0.03, 0.1])
cbar=fig.colorbar(ccc, orientation='vertical', cax=cax,ticks=np.linspace(0,75,3))
cbar.ax.set_yticklabels(["0","50","100"])
cbar.ax.tick_params(labelsize=ls) 

#fig.set_size_inches(6.36,3.94)  
fig.set_size_inches(3.94,2.43)  

plt.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Tradeoffs_2D_ALL5.png',dpi=300)

plt.show(block=True)


# In[14]:

# Rhodium by default assumes that it should plot the model responses.  These responses are automatically plotted on the x, y, z (for 3D plots), color, and size axes.  This can be customized.  For example, we can color each point with their reliability:
#from matplotlib import rc
# activate latex text rendering
#rc('text', usetex=False)

#subprocess.call(['python','House_elevation_rhodium.py'])

fig = scatter2d(model, output, c="Delta_Elev",show_legend=False,figure_xlab="Cost of Elevating the House (1000 USD)",figure_ylab="Flood Damages(1000 USD)",figure_clab="Elevation(ft)")
fig.set_size_inches(11,8)
#fig.suptitle('Tradeoffs between cost of elevation and flood damages', fontsize=20)
ax2=fig.gca()

s_range=(10,50)
s=df.flooding_reliability
s_gev=df_GEV.flooding_reliability
s_gevmcmc=df_GEVMCMC.flooding_reliability
s_min = min(s)
s_max = max(s)


s = (s_range[1]-s_range[0]) * ((s_gev-s_min) / (s_max-s_min)) + s_range[0]
c=df_GEV.Delta_Elev
kwargs={'cmap':'None'}
kwargs["cmap"] = RhodiumConfig.default_cmap
ax2.scatter(df_GEV.cost_elevation,df_GEV.flood_damage,s=s ,marker="o",c=c,**kwargs)

s_gevmcmc = (s_range[1]-s_range[0]) * ((s_gevmcmc-s_min) / (s_max-s_min)) + s_range[0]
c=df_GEVMCMC.Delta_Elev
kwargs={'cmap':'None'}
kwargs["cmap"] = RhodiumConfig.default_cmap
ax2.scatter(df_GEVMCMC.cost_elevation,df_GEVMCMC.flood_damage,s=s_gevmcmc ,marker="o",c=c,**kwargs)

l0=ax2.scatter(0, 0, marker="*",s=100,color="black")
l1=ax2.scatter(house_elevation_problem(6)[0],house_elevation_problem(6)[1], marker="o",s=50,color="black")
l2=ax2.scatter(house_elevation_problem(11)[0],house_elevation_problem(11)[1], marker="s",s=50,color="black")
l3 = ax2.scatter([],[],c='black' ,s=10, edgecolors='none')
l4 = ax2.scatter([],[],c='black', s=50, edgecolors='none')
ln1=ax2.scatter([],[],facecolors='none', edgecolors='none')
ln2=ax2.scatter([],[],facecolors='none', edgecolors='none') 
#labels = ['Ideal Point','CBA Optimal Policy','FEMA Recommendation','','$\\bf{Reliability}$',str(round(min(df['flooding_reliability']),1)),str(round(max(df['flooding_reliability']),1))]

labels = ['Ideal Point','CBA Optimal Policy','FEMA Recommendation','','$\\bf{Reliability}$',str(round(s_min,1)),str(round(s_max,1))]

ax2.legend([l0,l1, l2,ln1,ln2,l3,l4], labels, ncol=1, frameon=False, fontsize=12,handlelength=2, loc =0, borderpad = 1.8,handletextpad=1, scatterpoints = 1)
ax2.text(5,1150,'Damages from GEV Return Levels')
ax2.text(-2,250,'Damages from \n USGS Return Levels')

fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/Tradeoffs_2D_comparision_methods.pdf')
plt.show(block=True)

# In[15]:
# It is also possible to group or classify policies together using brushing.  Brushing in Rhodium uses Python expressions to match the policies of interest.  When plotting with brushes, the points will be highlighted if they match the brush:

fig = scatter2d(model, output,
                brush=[Brush("flooding_reliability > 0.7"), Brush("flooding_reliability <= 0.7")])
fig.set_size_inches(11,8)
fig.suptitle('Tradeoffs between cost of elevation and flood damages', fontsize=20)
fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/scatter2d_brush.pdf')
plt.show(block=True)

# In[16]:

# Brushing includes several additional options, such as allowing additional control over the plotting (e.g., setting color or point size).  As shown, it is possible to define more than one brush.  Also, any points not painted by a brush will appear as "Unassigned".  
# 
# We can similary generate 3D plots in Rhodium:

fig = scatter3d(model, output, c="Delta_Elev",
                brush=[Brush("flooding_reliability > 0.7"), Brush("flooding_reliability <= 0.7")])
fig.set_size_inches(11,8)
fig.suptitle('Tradeoffs between cost of elevation and flood damages', fontsize=20)
fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/scatter3d_brush.pdf')
plt.show(block=True)

# In[16]:

# Brushing includes several additional options, such as allowing additional control over the plotting (e.g., setting color or point size).  As shown, it is possible to define more than one brush.  Also, any points not painted by a brush will appear as "Unassigned".  
# 
# We can similary generate 3D plots in Rhodium:

fig = scatter3d(model, output, c="Delta_Elev")
fig.set_size_inches(11,8)
fig.suptitle('Tradeoffs between cost of elevation and flood damages', fontsize=20)
fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/scatter3d_no_brush.pdf')

plt.xlabel('Cost of House Elevating[1000 US$]')
plt.ylabel('Flood Damages[1000 US$]')
#plt.zlabel('Reliability')
ax=fig.gca()
ax.set_zlabel('Reliability')
#cb = colorbar()

#cb1   = mpl.colorbar.ColorbarBase(colorbar1, cmap=cmap1, norm=norm1, orientation='horizontal')
#cb1.set_label('magnitude')
#cb.set_label("Foo", labelpad=-1)

plt.show(block=True)

# In[17]:
# This part fif not work, IDK why

fig = pairs(model, output,brush=[Brush("flooding_reliability > 0.7"), Brush("flooding_reliability <= 0.7")])
#fig.set_size_inches(11,8)
#fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/pairs_brush.pdf')

# In[18]:
# Another commonly used way to view interactions between factors is parallel coordinates plots:

fig = parallel_coordinates(model, output,cols=['cost_elevation','flood_damage'],c="flooding_reliability",colormap="rainbow", target="top",cblabel="Reliability(Probability of not being flooded)l")
fig.set_size_inches(11,8)
fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/parallel_coordinate.pdf')
#plt.show(block=True)

# In[19]:
# Similar to the other plots, brushing is also supported.

fig = parallel_coordinates(model, output, target="top",
                           brush=[Brush("flooding_reliability > .7"), Brush("flooding_reliability <= .7")])
fig.set_size_inches(11,8)
fig.savefig('/Users/mxz414/Documents/Research/House_Elevation_Project/Results/parallel_coordinate_brush.pdf')
#plt.show(block=True)

# In[20]:
# ## Scenario Discovery

# Using optimization, we leveraged an automated search procedure to identify optimal policies for our model.  We can also manually explore different policies.  Recall that a policy is simply a Python dictionary with keys mapping to our parameters.  We can construct new policies by creating dictionaries with whatever parameters we wish to control.  For example, here is a policy where we allow a yearly pollution level of 0.02:

policy = {"Delta_Elev" :6}


# In[21]:
# We can use `evaluate` to evaluate the policy and display the responses:

result = evaluate(model, policy)

print("Cost if Elevating the house:", result["cost_elevation"])
print("Flood damages:               ", result["flood_damage"])
print("Delta_Elev:               ", result["Delta_Elev"])
print("Reliability:               ", result["flooding_reliability"])

# In[22]:
# When we evaluate the policies in this manner, we are assuming there is no uncertainty.  The default values for each parameter is used in the calculation.  We can also explore the effects of uncertainties on our policy.  First, we must define the uncertain parameters and their distributions.  Below we use uniformly-distributed uncertainties, but other distributions such as normal and log-normal are also supported.

model.uncertainties = [UniformUncertainty("disc_rate",0.03,0.1),UniformUncertainty("n",10,100)]

# In[23]:

# Next, we will create many possible states-of-the-world (SOWs), each representing some combination of the uncertainty parameters.  Here, we generate 1000 SOWs:

SOWs = sample_lhs(model, 1000)

# In[24]:

# Then, using the `evaluate` method, we evaluate our policy in each SOW.  To do this, we need to combine our SOWs with our fixed policy.  Using the `update` function, we update each SOW with the parameters defined in our policy, namely the fixed `pollution_limit` we specified previously.

results = evaluate(model, update(SOWs, policy))
#print(results)

# In[25]:

# It can be cumbersome to explore the results from the 1000 SOWs separately.  Instead, we are really only interested in SOWs where our policy achieved a reliability of 90% or greater (conversely, we could be interested in SOWs where our system failed with poor reliability).  We "classify" each SOW as "Reliable" or "Unreliable":

classification = results.apply("'Reliable' if flooding_reliability >=0.1 else 'Unreliable'")


# In[30]:
# ## Sensitivity Analysis
# Rhodium also automates the task of performing global and regional sensitivity analysis on the model uncertainties.  Rhodium provides a convenient wrapper on top of Python's [SALib](https://github.com/SALib/SALib/), allowing you to specify only the model, the response of interest, an optional policy you want to investigate, and details on the sensitivity analysis method.  Lets start with the Morris method to identify the sensitive parameters with respect to reliability:

result = sa(model, "flooding_reliability", policy=policy, method="morris", nsamples=1000, num_levels=4, grid_jump=2)
print(result)

# In[31]:
# Concurring with the PRIM analysis, parameters `b`, `q`, and `mean` exhibit the largest sensitivites.  Some methods, like the Morris method, only analyze the effects of parameters in isolation.  The Sobol method lets us also compute second-order and total-order indices capturing the interactions between parameters:

result = sa(model, "flooding_reliability", policy=policy, method="sobol", nsamples=10000)
print(result)

# In[32]:
# We can also visualize the results from sensitivity analysis.  For example, below we plot the first and total-order sensitivity indices for the Sobol method.  Similar 2D bar plots can be shown for all other supported methods.

fig = result.plot()
plt.show(block=True)
exit()
# In[33]:
# If using Sobol's method, we can also generate a "spider" or "radial" plot showing the second-order indices.  In the figure below, the first and total-order indices for each parameter are indicated by the solid circle and the outer ring, respectively, where a larger radius indicates larger effects.  The gray lines connecting the circles represents the second-order indices, with thicker lines corresponding to larger second-order effects.

fig = result.plot_sobol(threshold=0.01)
plt.show(block=True)

# In[34]:
# If you have a plot with a large number of parameters, it can be useful to visually group similar parameters.  For example:

fig = result.plot_sobol(threshold=0.01,
                        groups={"Discounting" : ["disc_rate"],"Lifespan" : ["n"]})
plt.show(block=True)

exit()
# In[26]:

# Now we will use the Patient Rule Induction Method (PRIM) to identify the key uncertainties that cause a SOW to fall within either class.  This process is termed "scenario discovery".  PRIM works by restricting the dimensions (bounds) for each uncertainty that contains the SOWs of interest.  We refer to these restrictions as a "box".
# 
# PRIM is an iterative process.  We call `find_box()` followed by `box.show_tradeoff()` to display visually the dimension restrictions in terms of two metrics: coverage and density.  Coverage measures the percentage of cases of interest contained within the box and density considers the percentage of cases within the box that are of interest.  Having a coverage and density of 100% is ideal.  Note we provide the `coi` argument to indicate we are interested in solutions classified as "Reliable".

p = Prim(results, classification, include=model.uncertainties.keys(), coi="Reliable")
box = p.find_box()
fig = box.show_tradeoff()
plt.show(block=True)
exit()
# In[27]:

# Each point in the above plot represents a sequence of restrictions on the dimensions (a peeling trajectory).  Typically, as we restrict each dimension further, we improve density but reduce coverage.  We can view the details on a specific peeling trajectory by first selecting the i-th trajectory with `box.select(i)` and running:

print(box)
box.show_details()

# If you're running this example directly from Python, you can also run `box.show_details()` to produce a nicely formatted visual display of the box limits, the covered SOWs, and the box statistics.  See [here](https://github.com/Project-Platypus/PRIM/blob/master/docs/images/screenshot2.png) for an example.

# In[28]:
# Classification and Regression Trees (CART) is an alternative method for scenario discovery.  Unlike PRIM, which produces a single box containing the cases of interest, CART can form disjoint partitions of the dataset.  As such, it tends to offer better classification rates at the expensive of interpretability (the partitions can become complex and difficult to understand).

c = Cart(results, classification, include=model.uncertainties.keys(), min_samples_leaf=50)
c.show_tree()


# In[29]:

# The graphic above shows each recursive partition identified by CART, color-coded by their classification.  We can also print the contents of the tree.  If the case of interest (`coi`) is specified, then it prints only those nodes with the given class, letting you compare the density and coverage of a node (including intermediate nodes) as we did with PRIM.  It also prettifies the rules as shown below.

c.print_tree(coi="Reliable")


# In[35]:
# Global sensitivity analysis is useful for identifying the key parameters and their interactions.  One limitation is that it does not show where the sensitivities occur.  For example, the analyis above shows that `b`, `q`, and `mean` are the three sensitive parameters, but it doesn't tell us where the parameters are having effect.  We can use one-at-a-time (OAT) or regional sensitivity analysis to explore each parameter in detail.  In OAT sensitivity analysis, we fix all parameters at their default value except one.  For this one parameter, we then sample across its entire range and observe how the response of interest changes.


fig = oat(model, "Delta_Elev", policy=policy, nsamples=1000)


# The plot above shows on the left the percentage of total variance caused by each parameter.  We see that `b` and `q` contribute most to the variance.  On the right we see a line plot showing how the response of interest (reliability) changes as the sampled parameter changes.  Take `b`, for example, shown in blue.  `b` approches 100% reliability as its value increases.  On the other hand, the mean level of pollution, `mean`, causes a substantial reduction in reliability as the value increases.
# 
# You may have also noticed that `q` has no effect here, contrary to what we saw from global sensitivity analysis.  Recall that OAT sensitivity analysis fixes all but one parameter at their default value.  This indicates that `q` has little to no effect when the other parameters are fixed at their default.  But since we observed large sensitivites from `q` from global sensitivity analysis, one would expect `q` would become more significant if the other parameters deviate from their default.

# ## Conclusion

# As demonstrated, Rhodium is a powerful tool for analyzing and exploring a model.  We demonstrated connecting Rhodium to a model written in Python.  Rhodium also supports tools for connecting to models written natively (and compiled into a dynamically-linked library or shared object), Excel models, and more.  Please visit our [Github page](https://github.com/Project-Platypus/Rhodium) to learn more.
