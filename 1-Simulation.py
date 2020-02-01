import numpy as np
import numpy.matlib
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import random
from math import acos, degrees
import scipy.stats


# Constants
NAGENTS = 10     # number of agents
NTIME = 720    # number of time steps 24-hours * 30 days
IUU_EVENT = 312   # Time of illegal event
tspeed = 0.025    # Traveling speed
ispeed = 0.025    # Speed when close to IUU
fspeed = 0.0025   # Fishing speed


e = 0.001          # separation error
ie = 0.25         # IUU separation error
FA_X1 = 0.6       # Fishing area coords.
FA_X2 = 0.8
FA_Y1 = 0.2
FA_Y2 = 0.8


def dist(x1, x2, y1, y2):
    return ( np.sqrt( (x2 - x1)**2 + (y2 - y1)**2) )



# x_c = x_a - (d_2 * (x_a - x_b))  /d
def solve_dist(x1, x2, y1, y2, max_speed):
    d = dist(x1, x2, y1, y2)
    if d > max_speed:
        d2 = max_speed
        x3 = x1 - ( d2 * (x1 - x2)) / (d)
        y3 = y1 - ( d2 * (y1 - y2)) / (d)
    else:
        x3 = x2
        y3 = y2
    return x3, y3

   
def calc_vmove(x1, x2, y1, y2, inverse_dist=False, random_dir=False, rinterval=0.10, max_speed=0.025):
    '''
    info
    '''
    if random_dir == True:
        # Update location adjusting for sign (inverse)
        randx = np.linspace(x2 - rinterval, x2 + rinterval, 50)
        randy = np.linspace(y2 - rinterval, y2 + rinterval, 50)
        x2 = np.random.choice(randx, 1)
        y2 = np.random.choice(randy, 1)

    # More away target
    if inverse_dist == True:
        if x1 < x2:
            x2 = x1 + (x2 - x1)
        if x1 >= x2:
            x2 = x1 - (x1 - x2)
        if y1 < y2:
            y2 = y1 - (y2 - y1)
        if y1 >= y2:
            y2 = y1 + (y1 - y2)

    # Calculate movement along linear line
    mx2, my2 = solve_dist(x1, x2, y1, y2, max_speed = max_speed)
    return mx2, my2

# Fishing area space
farea = list(((x, y) for x in np.linspace(FA_X1, FA_X2, 100) for y in np.linspace(FA_Y1, FA_Y2, 100)))

# Fishing Area x and y
fxVec = [item[0] for item in farea]
fyVec = [item[1] for item in farea]

# # Agents in random space
# xVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)
# yVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)

# Agents in random fishing area space
xVec = np.random.choice(fxVec, NAGENTS)
yVec = np.random.choice(fyVec, NAGENTS)



# Define all vessels as fishing = 0
agents = pd.DataFrame({'fishing_status': "Traveling",
                             'alert_status': "Not Alert",
                             'fishing_time': 0,
                             'xLoc': xVec,
                             'yLoc': yVec,
                             'fxLoc': random.sample(fxVec, NAGENTS),
                             'fyLoc': random.sample(fyVec, NAGENTS)})

# Illegal vessel location data
ivessel = pd.DataFrame({'xLoc': [0.99], 'yLoc': [0.01], 'fxLoc': [0.70], 'fyLoc': [0.30]})
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
        ivessel['fxLoc'][0] = 2.99
        ivessel['fyLoc'][0] = 2.99
    
    # Assign illegal locations
    ix1 = ivessel['xLoc'][0]
    iy1 = ivessel['yLoc'][0]
    ifx1 = ivessel['fxLoc'][0]
    ify1 = ivessel['fyLoc'][0]

    # Introduce IUU Event 
    if t >= IUU_EVENT:
        # Move IUU vessel
        ix2, iy2 = calc_vmove(ix1, ifx1, iy1, ify1, max_speed = (tspeed/2))
        
        # Update vessel
        ivessel['xLoc'][0] = ix2
        ivessel['yLoc'][0] = iy2
        
    # Save data 
    iindat = pd.DataFrame({'t': [t], 'x1': ix1, 'y1': iy1, 'fx1': ifx1, 'fy1': ify1})
    idat = pd.concat([idat, iindat])
    
    for i in range(NAGENTS):       
        # Vessel Location
        x1 = agents['xLoc'][i]
        y1 = agents['yLoc'][i]

        # Fishing location
        fx1 = agents['fxLoc'][i]
        fy1 = agents['fyLoc'][i]
               
        # If at location fish then find new location
        if (dist(x1, fx1, y1, fy1) <= e):
            # If vessel has been at fishing site for 10 hours
            if agents.loc[i, 'fishing_time'] == 10:
                fx1 = random.sample(fxVec, 1)[0]
                fy1 = random.sample(fyVec, 1)[0]
                agents.loc[i, 'fxLoc'] = fx1
                agents.loc[i, 'fyLoc'] = fy1
                agents.loc[i, 'fishing_time'] = 0
                # If returning to fishing spot, change to not alert
                if agents.loc[i, 'alert_status'] == "After Alert":                
                    agents.loc[i, 'alert_status'] = "Not Alert"                
            # If not, add 1 hour
            else:
                agents.loc[i, 'fishing_time'] += 1
            
        # Calc distances for all vessels
        agents.loc[:, 'dist'] = dist(x1, agents['xLoc'], y1, agents['yLoc'])
        dist_check = agents.sort_values('dist')[1:2]
        dx1 = dist_check['xLoc'].iat[0]
        dy1 = dist_check['yLoc'].iat[0]
        
        # Get distance to IUU Vessel
        idist = round(dist(x1, ix1, y1, iy1), 2)

        # If close to IUU Vessel move away
        # if (idist < ie):
        #     x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed = ispeed, random_dir=True) 
        #     agents.loc[i, 'alert_status'] = "Alert"
        
        # # If outside second margin of IUU don't move
        # if ( (idist >= ie) and (idist <= ie + 0.10) ):
        #     x2, y2 = calc_vmove(x1, dx1, y1, dy1, inverse_dist=True, max_speed = tspeed, random_dir=True) 
        #     fx1 = random.sample(fxVec, 1)[0]
        #     fy1 = random.sample(fyVec, 1)[0]
        #     agents.loc[i, 'fxLoc'] = fx1
        #     agents.loc[i, 'fyLoc'] = fy1
        #     #x2 = x1
        #     #y2 = y1
        #     agents.loc[i, 'alert_status'] = "Alert"
       
        # # Otherwise, move towards target fishing area at fishing speed      
        # elif (agents.loc[i, 'fishing_status'] == "Fishing"):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = fspeed)
        #     agents.loc[i, 'alert_status'] = "Not Alert"
        
        # # # Otherwise, move towards target fishing area at travel speed
        # elif (agents.loc[i, 'fishing_status'] == "Traveling"):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)
        #     agents.loc[i, 'alert_status'] = "Not Alert"

# Old code doesn't work
#-------------------------------------------------------------------------------
        # If close to IUU Vessel move away
        if ( (t >= IUU_EVENT) and  (idist < ie) ):
            #x1 = (x1 + fx1) / 2
            #y1 = (y1 + fy1) / 2
            x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed = ispeed) 
            agents.loc[i, 'alert_status'] = "Alert"
        
        # If outside second margin of IUU don't move
        if ( (t >= IUU_EVENT) and (idist >= ie) and (idist <= ie + 0.20) ):
            x2, y2 = calc_vmove(x1, fx1, y1, fy1, inverse_dist=True, max_speed = tspeed) 
            #x2 = x1
            #y2 = y1
            #fx1 = random.sample(fxVec, 1)[0]
            #fy1 = random.sample(fyVec, 1)[0]
            #agents.loc[i, 'fxLoc'] = fx1
            #agents.loc[i, 'fyLoc'] = fy1
            #agents.loc[i, 'alert_status'] = "After Alert"
        
        # elif ( (agents.loc[i, 'fishing_status'] == "Fishing") and (agents.loc[i, 'alert_status'] == "After Alert") ):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)  

        # elif ( (agents.loc[i, 'fishing_status'] == "Traveling") and (agents.loc[i, 'alert_status'] == "After Alert") ):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)  

        # Otherwise, move towards target fishing area at fishing speed      
        elif ( (agents.loc[i, 'fishing_status'] == "Fishing") and (agents.loc[i, 'alert_status'] == "Not Alert") ):
            x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = fspeed)            
        
        # Otherwise, move towards target fishing area at travel speed
        elif ( (agents.loc[i, 'fishing_status'] == "Traveling") and (agents.loc[i, 'alert_status'] == "Not Alert") ):
            x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)

        #-------------------------------------------------------------------------------            

        # Fishing vs Traveling vs Alert status
        if (x2 >= FA_X1 and x2 <= FA_X2 and y2 >= FA_Y1 and y2 <= FA_Y2):
           agents.loc[i, 'fishing_status'] = "Fishing"
        else:
           agents.loc[i, 'fishing_status'] = "Traveling"       
        # if (idist >= ie + 0.20):
        #     agents.loc[i, 'alert_status'] = "Not Alert"
        # else:
        #     agents.loc[i, 'alert_status'] = "Alert"

        # Move vessel 
        agents.loc[i, 'xLoc'] = x2
        agents.loc[i, 'yLoc'] = y2
        
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
# (4) Add 99% percentile alert signal min and max (DONE)
# (6) In figures add hour of IUU in the title (DONE)
# (7) Random movement when IUU event occurs (In progress)

# Sensitivity Analysis
# Change sensitivity of ie


