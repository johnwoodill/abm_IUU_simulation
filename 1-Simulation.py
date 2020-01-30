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
#v = 0.0625        # velocity 
tspeed = 0.025    # Traveling speed
ispeed = 0.025    # Speed when close to IUU
fspeed = 0.0025   # Fishing speed
#iuuv = 0.0625

e = 0.001          # separation error
ie = 0.25         # IUU separation error
iee = 0.1         # separation error from other vessels if alert
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

   
def calc_vmove(x1, x2, y1, y2, inverse_dist=False, random_dir=False, max_speed=0.025):
    '''
    info
    '''
    if random_dir == True:
        # Update location adjusting for sign (inverse)
        randx = np.linspace(x2 - .10, x2 + .10, 50)
        randy = np.linspace(y2 - .10, y2 + .10, 50)
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



def vmove(x1, x2, y1, y2, inverse_dist=False, random_dir=False):
    '''
    Calculate trig angle and return direction
    inverse_dist: Inverse distance moves away from target
    vmult: velocity multiplier 
    '''
    # Get distance to target
    #ldist = dist(x1, x2, y1, y2)
    #vel = ldist/25
    #print(f"ldist: {ldist} - vel: {vel}")
    #vel=1

    if random_dir == True:
        # Update location adjusting for sign (inverse)
        randx = np.linspace(x2 - .10, x2 + .10, 50)
        randy = np.linspace(y2 - .10, y2 + .10, 50)
        x2 = np.random.choice(randx, 1)
        # y2 = np.random.choice(randy, 1)

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

    vel = fsolve(solve_vel, 0.01, args = (x1, x2, y1, y2, theta, radius, 0.00025))
    # print(vel)

    # More towards target
    if inverse_dist == False:
        # Update location adjusting for sign
        if x1 > x2:
            mx2 = x1 - vmult * vel * (np.cos(theta) * radius)
        if x1 < x2:
            mx2 = x1 + vmult * vel * (np.cos(theta) * radius)
        if x1 == x2:
            mx2 = x2
        if y1 > y2:
            my2 = y1 - vmult * vel * (np.sin(theta) * radius)
        if y1 < y2:
            my2 = y1 + vmult * vel * (np.sin(theta) * radius)
        if y1 == y2:
            my2 = y2
    
    # Move away from target
    if inverse_dist == True:
        # Update location adjusting for sign (inverse)
        if x1 > x2:
            mx2 = x1 + vmult * vel * (np.cos(theta) * radius)
        if x1 < x2:
            mx2 = x1 - vmult * vel * (np.cos(theta) * radius)
        if x1 == x2:
            mx2 = x2
        if y1 > y2:
            my2 = y1 + vmult * vel * (np.sin(theta) * radius)
        if y1 < y2:
            my2 = y1 - vmult * vel * (np.sin(theta) * radius)
        if y1 == y2:
            my2 = y2

    return mx2, my2
       

# Define Fishing area
# x = 0.60, 0.80
# y = 0.20, 0.80
farea = list(((x, y) for x in np.linspace(FA_X1, FA_X2, 100) for y in np.linspace(FA_Y1, FA_Y2, 100)))

# Fishing Area
fxVec = [item[0] for item in farea]
fyVec = [item[1] for item in farea]

# # Agents in random space
xVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)
yVec = np.mod(np.random.uniform(0, 1, NAGENTS), 1)

# Agents in random fishing area space
# xVec = np.random.choice(fxVec, NAGENTS)
# yVec = np.random.choice(fyVec, NAGENTS)



# Define all vessels as fishing = 0
agents = pd.DataFrame({'fishing_status': "Traveling",
                             'alert_status': "Fishing",
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
               
        # If at location find new fishing location
        if (dist(x1, fx1, y1, fy1) <= e):
            if agents.loc[i, 'fishing_time'] == 10:
                fx1 = random.sample(fxVec, 1)[0]
                fy1 = random.sample(fyVec, 1)[0]
                agents.loc[i, 'fxLoc'] = fx1
                agents.loc[i, 'fyLoc'] = fy1
                agents.loc[i, 'fishing_time'] = 0
            else:
                agents.loc[i, 'fishing_time'] += 1
            
        # Calc distances for all vessels
        agents.loc[:, 'dist'] = dist(x1, agents['xLoc'], y1, agents['yLoc'])
        dist_check = agents.sort_values('dist')[1:2]
        dx1 = dist_check['xLoc'].iat[0]
        dy1 = dist_check['yLoc'].iat[0]
        
        # Get distance to IUU Vessel
        idist = round(dist(x1, ix1, y1, iy1), 2)
        
        # If closests vessel is within error (e) move away from vessel
        # if dist_check['dist'].iat[0] <= e:
        #     x2, y2 = calc_vmove(x1, dx1, y1, dy1, inverse_dist=True, max_speed = tspeed) 
        #     agents.loc[i, 'alert_status'] = "Case 3"

        # If close to IUU Vessel move away
        if ( (t >= IUU_EVENT) and (idist < ie) ):
            x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed=ispeed) 
            agents.loc[i, 'alert_status'] = "Case 1"
        
        # If outside second margin of IUU don't move
        if ( (t >= IUU_EVENT) and (idist >= ie) and (idist <= ie + 0.20) ):
            x2, y2 = calc_vmove(x1, dx1, y1, dy1, inverse_dist=True, max_speed = tspeed, random_dir=True) 
            #x2 = x1
            #y2 = y1
            agents.loc[i, 'alert_status'] = "Case 2"
       
        # Otherwise, move towards target fishing area at fishing speed      
        elif (agents.loc[i, 'fishing_status'] == "Fishing"):
            x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = fspeed)
            agents.loc[i, 'alert_status'] = "Case 4"
        
        # # Otherwise, move towards target fishing area at travel speed
        elif (agents.loc[i, 'fishing_status'] == "Traveling"):
            x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)
            agents.loc[i, 'alert_status'] = "Case 5"
        # else:
        #     agents.loc[i, 'alert_status'] = "Case 5"


        # # Separation:
        # if dist_check['dist'].iat[0] <= e:
        #     x2, y2 = calc_vmove(x1, dx1, y1, dy1, inverse_dist=True, max_speed = tspeed) 

        # # If close to IUU Vessel move away
        # elif ( (idist <= ie) and (t >= IUU_EVENT) ):
        #     x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed = ispeed) 
        #     agents.loc[i, 'alert_status'] = "Alert"

        # # If inside IUU event, far away from iuu, but within ie + 0.20 range
        # elif ( (t > IUU_EVENT) and (t <= (IUU_EVENT + 48)) and (idist > ie + .20)  ):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed) 
        
        # else: 
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)

        # If outside second margin of IUU, not close to vessels don't move
        # elif ( (t > IUU_EVENT) and (t <= (IUU_EVENT + 48)) and (idist >= ie) and (idist < ie + 0.20) ):
        #     x2 = x1
        #     y2 = y1
        
        # If closests vessel is within error (e) move away from vessel
        # Otherwise, move towards target fishing area       
        #elif ( (agents.loc[i, 'fishing_status'] == 'Fishing') and (agents.loc[i, 'alert_status'] != 'Alert')):
        #    x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = fspeed)
        #    agents.loc[i, 'fishing_status'] = "Fishing"
        #    agents.loc[i, 'alert_status'] = "Not Alert"
        
        # elif ( (agents.loc[i, 'fishing_status'] == 'Fishing') and (agents.loc[i, 'alert_status'] == 'Alert')):
        #     x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True,  max_speed = ispeed)
        #     agents.loc[i, 'fishing_status'] = "Fishing"
        #     agents.loc[i, 'alert_status'] = "Alert"

        # elif ( (agents.loc[i, 'fishing_status'] == 'Traveling') and (agents.loc[i, 'alert_status'] != 'Alert')):
        #     x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)
        #     agents.loc[i, 'fishing_status'] = "Traveling"
        #     agents.loc[i, 'alert_status'] = "Not Alert"

        # elif ( (agents.loc[i, 'fishing_status'] == 'Traveling') and (agents.loc[i, 'alert_status'] == 'Alert')):
        #     x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed = ispeed)
        #     agents.loc[i, 'fishing_status'] = "Traveling"
        #     agents.loc[i, 'alert_status'] = "Alert"
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


