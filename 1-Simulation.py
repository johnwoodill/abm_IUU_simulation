import numpy as np
import numpy.matlib
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import random
from math import acos, degrees

def dist(x1, x2, y1, y2):
    return (np.sqrt( (x2 - x1)**2 + (y2 - y1)**2))


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
odat = pd.DataFrame()
for t in range(100):
    for i in range(100):        
        
        # Fishing location
        fx1 = agents['fxLoc'][i]
        fy1 = agents['fyLoc'][i]
        
        # Vessel Location
        x1 = agents['xLoc'][i]
        y1 = agents['yLoc'][i]
        
        # Calc angle
        C = dist(x1, fx1, y1, fy1) 
        B = dist(x1, fx1, fy1, fy1) 
        A = dist(x1, x1, y1, fy1) 
        
        radius = C
        
        # Calc C-B angle
        #theta = acos( (B**2 + C**2 - A**2) / (2*B*C) )
        
        # Calc A-C theta
        theta = np.arccos( (A**2 + C**2 - B**2) / (2*A*C) )
        
        # Update location adjusting for sign
        if x1 > fx1:
            x2 = x1 - v * (np.cos(theta) * radius)
        if x1 <= fx1:
            x2 = x1 + v * (np.cos(theta) * radius)
        if y1 > fy1:
            y2 = y1 - v * (np.sin(theta) * radius)
        if y1 <= fy1:
            y2 = y1 + v * (np.sin(theta) * radius)
        
        agents['xLoc'][i] = x2
        agents['yLoc'][i] = y2
        
        # Save data 
        indat = pd.DataFrame({'time': [t], 'vessel': [i], 'x1': x1, 'y1': y1, 'x2': x2, 'y2': y2, 'fx1': fx1, 'fy1': fy1, 'r': radius})
        odat = pd.concat([odat, indat])
        

odat = odat.reset_index(drop=True)
odat.to_feather('data/test_dat.feather')

sns.scatterplot(xVec, yVec)
sns.scatterplot(fxVec, fyVec)
plt.show()


# Direction
thVec = np.random.uniform(0, 1, NAGENTS)*2*np.pi


a = np.floor(NTIME/5)
rIntVec = 0.0665 + 0.0535*np.sin( 2*np.pi*(np.linspace(1, NTIME, NTIME)) / ((NTIME/2) - 2*(np.linspace(1, NTIME, NTIME)/5)) )
rIntVec = rIntVec / 2


v = v/2
# Data Arrays
xData = np.zeros((NTIME, NAGENTS))
yData = np.zeros((NTIME, NAGENTS))
thData = np.zeros((NTIME, NAGENTS))
meanTheta = np.zeros((2, NAGENTS))
neighborVec = np.ones((1, NAGENTS))




mdat = pd.DataFrame()
for t in range(1, NTIME):
    print(f"Time = {t}/{NTIME}")
    rInt = rIntVec[t]

    # Zero out the neighbor vec
    neighborVec = np.ones((1, NAGENTS))
    
    # Compute the mean direction vector for each agent
    meanTheta = np.zeros((2, NAGENTS))
    
    for i in range(2, NAGENTS):
        # Calculate x (cosine(theta)*radius), y (sine(theta) * radius)
        meanTheta[:, i] = np.matrix([np.cos(thVec[i]), np.sin(thVec[i])])
        
        for j in range(1, i):
            logic_check = min(abs(xVec[i] - xVec[j]), 
                              length - abs(xVec[i] - xVec[j]))**2 + min(abs(yVec[i] - yVec[j]), length - abs(yVec[i] - yVec[j]))**2
            
            if logic_check < rInt**2:
                meanTheta[:, i] = meanTheta[:, i] + np.matrix(np.cos(thVec[j]), np.sin(thVec[j]))    
                meanTheta[:, j] = meanTheta[:, j] + np.matrix(np.cos(thVec[i]), np.sin(thVec[i]))    
                neighborVec[:, i] = neighborVec[:, i] + 1
                neighborVec[:, j] = neighborVec[:, j] + 1
                
    # calculate mean angle from mean velocity data
    meanTheta = meanTheta / np.matlib.repmat(neighborVec, 2, 1)
    
    
    
    stable = np.argwhere(abs(meanTheta[0, :]) > abs(meanTheta[1, :]))
    unstable = np.argwhere(abs(meanTheta[0, :]) < abs(meanTheta[1, :]))
        
    # atan2 function takes in (y_values, x_values) and returns theta in
    tanTheta_s = np.arctan2(meanTheta[1, stable], meanTheta[0, stable])
    cotTheta_u = np.arctan2(meanTheta[0, unstable], meanTheta[1, unstable])
    
    angle = np.zeros((NAGENTS))
    angle[stable] = tanTheta_s                # atan(tanTheta_s);
    angle[unstable] = np.pi/2 - cotTheta_u    # atan(cotTheta_u);
    
    # Next step, update positions
    xVec = np.mod(xVec + v * np.cos(thVec), length)
    yVec = np.mod(yVec + v * np.sin(thVec), length)
    
    # Next step, update agent direction
    thVec = np.mod(angle + 2 * np.pi * (np.random.normal(0, 1, 500)) * error, 2 * np.pi)
        
    indat = pd.DataFrame({'id': pd.Series(range(0, 500)), 't': t, 'x': xVec, 'y': yVec, 'theta': thVec})
    mdat = pd.concat([mdat, indat])
    

    
mdat = mdat.reset_index(drop=True)
mdat.to_feather('data/sim_data.feather')



                
                