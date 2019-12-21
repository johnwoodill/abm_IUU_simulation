import numpy as np
import numpy.matlib

NAGENTS = 500     # Number of agents
NTIME = 2000    # Number of time steps
length = 1
v = 0.00625 * length
dt = 1.0     # Time step size
rho = 1.0    # agent density
error = 0.1  # noice

# Generate Simulation Data

# Agents
xVec = np.mod(np.random.uniform(0, 1, NAGENTS)*length, length)
yVec = np.mod(np.random.uniform(0, 1, NAGENTS)*length, length)

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


for t in range(1, NTIME):
    print(f"Time = {t}/NTIME")
    rInt = rIntVec[t]

    # Zero out the neighbor vec
    neighborVec = np.ones((1, NAGENTS))
    
    # First step, compute the mean direction vector for each agent
    meanTheta = np.zeros((2, NAGENTS))
    
    for i in range(1, NAGENTS):
        meanTheta[:, i] = np.matrix([np.cos(thVec[i]), np.sin(thVec[i])])
        
        for j in range(1, (i)):
            
            logic_check = min(abs(xVec[i] - xVec[j]), length - abs(xVec[i] - xVec[j]))**2 + min(abs(yVec[i] - yVec[j]), length - abs(yVec[i] - yVec[j]))**2
            
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
    # correct quadrant
    tanTheta_s = np.arctan2(meanTheta[1, stable], meanTheta[0, stable])
    cotTheta_u = np.arctan2(meanTheta[0, unstable], meanTheta[1, unstable])
    
    angle = np.zeros(neighborVec.shape)
    angle[:, stable] = tanTheta_s                # atan(tanTheta_s);
    angle[:, unstable] = np.pi/2 - cotTheta_u    # atan(cotTheta_u);
    
    # Next step, update positions
    xVec = np.mod(xVec + v * np.cos(thVec), length)
    yVec = np.mod(yVec + v * np.sin(thVec), length)
    
    # Next step, update agent direction
    thVec = np.mod(angle + 2 * np.pi * (np.random.normal(0, 1, 500)) * error, 2 * np.pi)
    
    # store the results
    xData[t,:] = xVec
    yData[t,:] = yVec
    thData[t,:] = thVec

​
rVec=rIntVec
                
                