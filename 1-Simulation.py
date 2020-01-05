import numpy as np
import numpy.matlib
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import random
from math import acos, degrees

def dist(x1, x2, y1, y2):
    return (np.sqrt( (x2 - x1)**2 + (y2 - y1)**2))

def vmove(x1, x2, y1, y2, inverse_dist=False):
    '''
    Calculate trig angle and return direction
    Inverse distance moves away from target
    '''
    # Calc angle
    C = dist(x1, x2, y1, y2) 
    B = dist(x1, x2, y2, y2) 
    A = dist(x1, x1, y1, y2) 
    radius = C

    # Get angle at B/C
    theta = np.arccos( (B**2 + C**2 - A**2) / (2*B*C) )

    if inverse_dist == False:
        # Update location adjusting for sign
        if x1 > fx1:
            x2 = x1 - v * (np.cos(theta) * radius)
        if x1 < fx1:
            x2 = x1 + v * (np.cos(theta) * radius)
        if x1 == fx1:
            x2 = fx1
        if y1 > fy1:
            y2 = y1 - v * (np.sin(theta) * radius)
        if y1 < fy1:
            y2 = y1 + v * (np.sin(theta) * radius)
        if y1 == fy1:
            y2 = fy1
        
    if inverse_dist == True:
        # Update location adjusting for sign (inverse)
        if x1 > fx1:
            x2 = x1 + 2*v * (np.cos(theta) * radius)
        if x1 < fx1:
            x2 = x1 - 2*v * (np.cos(theta) * radius)
        if x1 == fx1:
            x2 = fx1
        if y1 > fy1:
            y2 = y1 + 2*v * (np.sin(theta) * radius)
        if y1 < fy1:
            y2 = y1 - 2*v * (np.sin(theta) * radius)
        if y1 == fy1:
            y2 = fy1
            
    return x2, y2
       
      

NAGENTS = 100     # Number of agents
NTIME = 100    # Number of time steps
length = 1
v = 0.00625 * length
dt = 1.0     # Time step size
rho = 1.0    # agent density
error = 0.1  # noice

# Agents
xVec = np.mod(np.random.uniform(0, 1, NAGENTS)*length, length)
yVec = np.mod(np.random.uniform(0, 1, NAGENTS)*length, length)

# Define Fishing area
# x = 0.60, 0.80
# y = 0.20, 0.80
farea = list(((x, y) for x in np.linspace(0.6, 0.8, 20) for y in np.linspace(0.2, 0.8, 60)))

fxVec = [item[0] for item in farea]
fyVec = [item[1] for item in farea]

# Define all vessels as fishing = 0
agents = pd.DataFrame({'fishing_status': np.zeros(NAGENTS),
                             'alert_status': np.zeros(NAGENTS),
                             'xLoc': xVec,
                             'yLoc': yVec,
                             'fxLoc': random.sample(fxVec, NAGENTS),
                             'fyLoc': random.sample(fyVec, NAGENTS)})

agents


#sns.scatterplot([x1], [y1])
#sns.scatterplot([x2], [y2])
#plt.show()

v = 0.0625
e = 0.01
odat = pd.DataFrame()
for t in range(250):
    for i in range(100):        
        
        # Fishing location
        fx1 = agents['fxLoc'][i]
        fy1 = agents['fyLoc'][i]
        
        # Vessel Location
        x1 = agents['xLoc'][i]
        y1 = agents['yLoc'][i]
        
        # If at location find new fishing location
        if (dist(x1, fx1, y1, fy1) <= e):
            agents['fxLoc'][i] = random.sample(fxVec, 1)[0]
            agents['fyLoc'][i] = random.sample(fyVec, 1)[0]
        
        # Calc distances for all vessels
        agents['dist'] = dist(x1, agents['xLoc'], y1, agents['yLoc'])
        dist_check = agents.sort_values('dist')[1:2]
        dx1 = dist_check['xLoc']
        dy1 = dist_check['yLoc']
        
        # If closests vessel is within error (e) move away from vessel
        if dist_check['dist'].iat[0] <= e:
            x2, y2 = vmove(x1, dx1, y1, dy1, inverse_dist=True)        
        else:
            x2, y2 = vmove(x1, fx1, y1, fy1)
        
        agents['xLoc'][i] = x2
        agents['yLoc'][i] = y2
        
        # Save data 
        indat = pd.DataFrame({'time': [t], 'vessel': [i], 'x1': x1, 'y1': y1, 'x2': x2, 'y2': y2, 'fx1': fx1, 'fy1': fy1})
        odat = pd.concat([odat, indat])
    
    
        

odat = odat.reset_index(drop=True)
odat.to_feather('data/test_dat.feather')


# Todo
# (1) Allow different speeds for fishing vs alert
# (2) Separation to prevent collisions
# (3) Alert vessels move away from illegal vessel

