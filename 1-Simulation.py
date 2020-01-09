import numpy as np
import numpy.matlib
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import random
from math import acos, degrees

def dist(x1, x2, y1, y2):
    return ( np.sqrt( (x2 - x1)**2 + (y2 - y1)**2) )


def vmove(x1, x2, y1, y2, inverse_dist=False, vmult=1):
    '''
    Calculate trig angle and return direction
    inverse_dist: Inverse distance moves away from target
    vmult: velocity multiplier 
    '''
    # Calc angle
    C = dist(x1, x2, y1, y2) 
    B = dist(x1, x2, y2, y2) 
    A = dist(x1, x1, y1, y2) 
    radius = C

    # Get angle at B/C
    #      0          0 - Location of vessel 
    #      |\
    #      | \  
    #    A |  \ C
    #      |   \   
    #      |    \              
    #      |____*\    * - Angle to calculate
    #         B
    theta = np.arccos( (B**2 + C**2 - A**2) / (2*B*C) )

    # More towards target
    if inverse_dist == False:
        # Update location adjusting for sign
        if x1 > x2:
            mx2 = x1 - v * (np.cos(theta) * radius)
        if x1 < x2:
            mx2 = x1 + v * (np.cos(theta) * radius)
        if x1 == x2:
            mx2 = x2
        if y1 > y2:
            my2 = y1 - v * (np.sin(theta) * radius)
        if y1 < y2:
            my2 = y1 + v * (np.sin(theta) * radius)
        if y1 == y2:
           my2 = y2
    
    # Move away from target
    if inverse_dist == True:
        # Update location adjusting for sign (inverse)
        if x1 > x2:
            mx2 = x1 + vmult * v * (np.cos(theta) * radius)
        if x1 < x2:
            mx2 = x1 - vmult * v * (np.cos(theta) * radius)
        if x1 == x2:
            mx2 = x2
        if y1 > y2:
            my2 = y1 + vmult * v * (np.sin(theta) * radius)
        if y1 < y2:
            my2 = y1 - vmult * v * (np.sin(theta) * radius)
        if y1 == y2:
            my2 = y2
            
    return mx2, my2
       
     
# Constants
NAGENTS = 50     # number of agents
NTIME = 720       # number of time steps
IUU_EVENT = 312   # Time of illegal event
v = 0.0625        # velocity 
e = 0.01          # separation error
ie = 0.30         # IUU separation error
iee = 0.1        # separation error from other vessels if alert
FA_X1 = 0.4             # Fishing area coords.
FA_X2 = 0.8
FA_Y1 = 0.2
FA_Y2 = 0.8



# Agents
xVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)
yVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)

# Define Fishing area
# x = 0.60, 0.80
# y = 0.20, 0.80
farea = list(((x, y) for x in np.linspace(FA_X1, FA_X2, 50) for y in np.linspace(FA_Y1, FA_Y2, 60)))

fxVec = [item[0] for item in farea]
fyVec = [item[1] for item in farea]

# Define all vessels as fishing = 0
agents = pd.DataFrame({'fishing_status': "Traveling",
                             'alert_status': "Fishing",
                             'xLoc': xVec,
                             'yLoc': yVec,
                             'fxLoc': random.sample(fxVec, NAGENTS),
                             'fyLoc': random.sample(fyVec, NAGENTS)})


# Illegal vessel location data
ivessel = pd.DataFrame({'xLoc': [0.99], 'yLoc': [0.01], 'fxLoc': [0.70], 'fyLoc': [0.60]})

# -------------------------------------------------------------------
# Start simulation
odat = pd.DataFrame()    # Output data frame to store results
idat = pd.DataFrame()    # Illegal vessel location data
for t in range(NTIME):
    # Day 1 of IUU Event
    if t == 312:   
        ivessel['fxLoc'][0] = 0.70
        ivessel['fyLoc'][0] = 0.30
    
    # Day 2 of IUU Event
    if t == 336:   
        ivessel['fxLoc'][0] = 0.70
        ivessel['fyLoc'][0] = 0.80
    
    # Day 3 of IUU Event
    if t == 360:   
        ivessel['fxLoc'][0] = 0.99
        ivessel['fyLoc'][0] = 0.99
    
    # Assign illegal locations
    ix1 = ivessel['xLoc'][0]
    iy1 = ivessel['yLoc'][0]
    ifx1 = ivessel['fxLoc'][0]
    ify1 = ivessel['fyLoc'][0]

    # Introduce IUU Event 
    if t >= IUU_EVENT:
        # Move vessel
        ix2, iy2 = vmove(ix1, ifx1, iy1, ify1)
        
        # Update vessel
        ivessel['xLoc'][0] = ix2
        ivessel['yLoc'][0] = iy2
        
    # Save data 
    iindat = pd.DataFrame({'t': [t], 'x1': ix1, 'y1': iy1, 'fx1': ifx1, 'fy1': ify1})
    idat = pd.concat([idat, iindat])
    
    for i in range(NAGENTS):        
        
        # Fishing location
        fx1 = agents['fxLoc'][i]
        fy1 = agents['fyLoc'][i]
        
        # Vessel Location
        x1 = agents['xLoc'][i]
        y1 = agents['yLoc'][i]
        
        # If at location find new fishing location
        if (dist(x1, fx1, y1, fy1) <= e):
            agents.loc[i, 'fxLoc'] = random.sample(fxVec, 1)[0]
            agents.loc[i, 'fyLoc'] = random.sample(fyVec, 1)[0]
            # fx1 = agents['fxLoc'][i]
            # fy1 = agents['fyLoc'][i]
        
        # Calc distances for all vessels
        agents.loc[:, 'dist'] = dist(x1, agents['xLoc'], y1, agents['yLoc'])
        dist_check = agents.sort_values('dist')[1:2]
        dx1 = dist_check['xLoc'].iat[0]
        dy1 = dist_check['yLoc'].iat[0]
        
        # Get distance to IUU Vessel
        idist = dist(x1, ix1, y1, iy1)
                    
        # Separation:
        # If close to IUU Vessel move away
        if (t > IUU_EVENT and idist <= ie):
            x2, y2 = vmove(x1, ix1, y1, iy1, inverse_dist=True, vmult=2) 
            agents['alert_status'][i] = "Alert"
        # If outside second margin of IUU don't move
        elif ( (t > IUU_EVENT) and (t <= (IUU_EVENT + 48)) and (idist >= ie) and (idist < ie + 0.20) and (dist_check['dist'].iat[0] <= iee)):
            x2, y2 = vmove(x1, dx1, y1, dy1) 
        elif ( (t > IUU_EVENT) and (t <= (IUU_EVENT + 48)) and (idist >= ie) and (idist < ie + 0.20) and (dist_check['dist'].iat[0] > iee)):
            x2 = x1
            y2 = y1
        # If closests vessel is within error (e) move away from vessel
        elif dist_check['dist'].iat[0] <= e:
            x2, y2 = vmove(x1, dx1, y1, dy1, inverse_dist=True) 
        # Otherwise, move towards target fishing area       
        else:
            x2, y2 = vmove(x1, fx1, y1, fy1)
            agents.loc[i, 'alert_status'] = "Fishing"
            
        # Fishing vs Traveling vs Alert status
        if (x2 >= FA_X1 and x2 <= FA_X2 and y2 >= FA_Y1 and y2 <= FA_Y2):
            agents.loc[i, 'fishing_status'] = "Fishing"
        else:
            agents.loc[i, 'fishing_status'] = "Traveling"
            
        if (agents['alert_status'][i] == 'Alert'):
            agents.loc[i, 'fishing_status'] = "Alert"
        
        # Move vessel 
        agents['xLoc'][i] = x2
        agents['yLoc'][i] = y2
        
        # Save data 
        indat = pd.DataFrame({'t': [t], 'fishing_status': agents['fishing_status'][i], 'alert_status': agents['alert_status'][i], 'vessel': [i], 'x1': x1, 'y1': y1, 'x2': x2, 'y2': y2, 'fx1': fx1, 'fy1': fy1})
        odat = pd.concat([odat, indat])
    

        
    
    
        
# Vessel data
odat = odat.reset_index(drop=True)
odat.to_feather('data/vessel_dat.feather')

# Illegal Vessel Data
idat = idat.reset_index(drop=True)
idat.to_feather('data/iuu_vessel_dat.feather')



# Todo
# (1) Allow different speeds for fishing vs alert 
# (2) Separation to prevent collisions   (DONE)
# (3) Alert vessels move away from illegal vessel (DONE)
# (4) Change color based on status (DONE)
# (4) Add 99% percentile alert signal min and max
# (6) In figures add hour of IUU in the title

# Sensitivity Analysis
# Change sensitivity of ie

