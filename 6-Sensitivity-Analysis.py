import numpy as np
import numpy.matlib
import pandas as pd
import random
from math import acos, degrees
import scipy.stats
from scipy import stats
from scipy.stats import kurtosis
import multiprocessing


# Todo
# (1) Allow different speeds for fishing vs alert  (Done)
# (2) Separation to prevent collisions   (DONE)
# (3) Alert vessels move away from illegal vessel (DONE)
# (4) Change color based on status (DONE)
# (4) Add 99% percentile alert signal min and max (DONE)
# (6) In figures add hour of IUU in the title (DONE)
# (7) Random movement when IUU event occurs (Done)


def dist(x1, x2, y1, y2):
    return ( np.sqrt( (x2 - x1)**2 + (y2 - y1)**2) )


def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    return c * r


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


def dist_mat(ndat):
    d = []
    for i in range(len(ndat)):
        for j in range(len(ndat)):
            t = ndat['t'].iat[0]
            v1 = ndat['vessel'].iat[i]
            v2 = ndat['vessel'].iat[j] 
            x1 = ndat['x1'].iat[i]
            x2 = ndat['x1'].iat[j]
            y1 = ndat['y1'].iat[i]
            y2 = ndat['y1'].iat[j]
            d += [(v1, v2, dist(x1, x2, y1, y2))]
            #d += [(v1, v2, haversine(y1, x1, y2, x2))]
    distMatrix = pd.DataFrame(d).pivot(index=0, columns=1, values=2)
    distMatrix['t'] = t
    return distMatrix  


def kurt(x):
    n = len(x)
    kk = n * np.sum( (x - np.mean(x))**4 )/(np.sum( (x - np.mean(x))**2 )**2)
    return kk

#test = pd.DataFrame({'x': [np.nan, np.nan, 1, 3, 4, 6, 8]})


def ster(x, stat):
    n = len(x)
    if stat == "kurtosis" and n >= 4:
        # S.E. of Skewness
        ses = np.sqrt( (6*n*(n - 1) ) / ( (n-2)*(n + 1)*(n + 3) ) )
        sek = 2 * ses * np.sqrt( (n**2 - 1) / ( (n - 3) * (n + 5) ) )
        return sek
    if stat == "mean":
        return ( (np.std(x)) / (np.sqrt(n)) )



# Step 1
def abm(NAGENTS, sep_ie):
    # Constants
    # NAGENTS = 10     # number of agents
    NAGENTS = NAGENTS
    NTIME = 720    # number of time steps 24-hours * 30 days
    IUU_EVENT = 312   # Time of illegal event
    tspeed = 0.025    # Traveling speed
    ispeed = 0.025    # Speed when close to IUU
    fspeed = 0.0025   # Fishing speed
    e = 0.001          # separation error
    ie = sep_ie
    # ie = 0.25         # IUU separation error
    FA_X1 = 0.6       # Fishing area coords.
    FA_X2 = 0.8
    FA_Y1 = 0.2
    FA_Y2 = 0.8

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
            if (idist < ie):
                x2, y2 = calc_vmove(x1, ix1, y1, iy1, inverse_dist=True, max_speed = ispeed, random_dir=True) 
                agents.loc[i, 'alert_status'] = "Alert"
            
            # If outside second margin of IUU don't move
            if ( (idist >= ie) and (idist <= ie + 0.10) ):
                x2, y2 = calc_vmove(x1, dx1, y1, dy1, inverse_dist=True, max_speed = tspeed, random_dir=True) 
                fx1 = random.sample(fxVec, 1)[0]
                fy1 = random.sample(fyVec, 1)[0]
                agents.loc[i, 'fxLoc'] = fx1
                agents.loc[i, 'fyLoc'] = fy1
                #x2 = x1
                #y2 = y1
                agents.loc[i, 'alert_status'] = "Alert"
        
            # Otherwise, move towards target fishing area at fishing speed      
            elif (agents.loc[i, 'fishing_status'] == "Fishing"):
                x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = fspeed)
                agents.loc[i, 'alert_status'] = "Not Alert"
            
            # # Otherwise, move towards target fishing area at travel speed
            elif (agents.loc[i, 'fishing_status'] == "Traveling"):
                x2, y2 = calc_vmove(x1, fx1, y1, fy1, max_speed = tspeed)
                agents.loc[i, 'alert_status'] = "Not Alert"

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
    # Output file
    odat = odat.reset_index(drop=True)
    return odat


# Step 2
def calc_dist(dat):
    # ----------------------------------------
    dat2 = dat.groupby('t').apply(lambda x: dist_mat(x))

    # Save
    dat2.columns = dat2.columns.astype(str)
    dat2 = dat2.reset_index(drop=True)
    # dat2.to_feather('data/all_dist_matrix.feather')
    return dat2
    

# Step 3
def calc_ks(idat, ddat):
    i = idat['t'].iat[0]
    # print(i)
    if i == 0 or i == 1:
        return None
    h1 = idat
    h1 = h1.drop(columns='t')
    keep = np.triu(np.ones(h1.shape)).astype('bool').reshape(h1.size)
    rvs1 = h1.stack()[keep].values
    h2 = ddat[(ddat.t < i ) & (ddat.t >= i - 25*8)]
    retdat = pd.DataFrame()
    for j in range(max(h2.t), min(h2.t), - 1):
        # Filter last hour
        h3 = h2[h2.t == j]
        h3 = h3.drop(columns='t')
        keep = np.triu(np.ones(h3.shape)).astype('bool').reshape(h3.size)
        rvs2 = h3.stack()[keep].values
        # Calculate KS statistic
        kss1 = stats.ks_2samp(rvs1, rvs2)
        kss1 
        
        ks_stat1 = kss1[0]
        pvalue1 = kss1[1]
        indat = pd.DataFrame({'t': [i], 'lag': i - j, 'ks': [ks_stat1], 'pvalue': [pvalue1]})
        retdat = pd.concat([retdat, indat])
    return retdat


# Step 4
def calc_anom_det(dat):
    #se_ks_mean = dat.groupby('t')['ks'].apply(lambda x: ster(x, stat="mean"))
    #se_ks_kurt = dat.groupby('t')['ks'].apply(lambda x: ster(x, stat="kurtosis"))
    ks_mean = dat.groupby('t')['ks'].mean().reset_index()
    ks_kurt = dat.groupby('t')['ks'].apply(lambda x: kurt(x))
    mean_95 = ks_mean.ks.quantile(q=0.95)
    kurt_95 = ks_kurt.quantile(q=0.95)
    outdat = pd.DataFrame({'mean_95': [mean_95], 'kurt_95': [kurt_95]})
    return outdat



# For loop for parallel
def main(perms_):
    try:
        NAGENTS = perms_[0]
        sep_ie = round(perms_[1], 2)

        # Step 1: Calculate ABM
        # print("Running Agent-based Model")
        abm_dat = abm(NAGENTS = NAGENTS, sep_ie=sep_ie)

        # print("Calculating Distance Matrix")
        # Step 2: Calculate Distance Matrix
        ddat = calc_dist(abm_dat)

        # print("Calculate JS Statistics")
        # Step 3: Calculate JS
        js_dat = ddat.groupby('t', as_index=False).apply(lambda x: calc_ks(x, ddat))

        # print("Calculate mean and kurtosis of JS")
        # Step 4: Calculate mean and kurtosis
        mk_dat = calc_anom_det(js_dat)

        print(f"Saving data: data/sens/v{NAGENTS}-ie{sep_ie}.csv")
        # Build data frame to store data with
        odat = pd.DataFrame({'nagents': [NAGENTS], 'sep_ie': [sep_ie], 'ks_mean': mk_dat.mean_95, 'ks_kurt': mk_dat.kurt_95})

        # Save data
        odat = odat.reset_index(drop=True)
        odat.to_csv(f"data/sens/v{NAGENTS}-ie{sep_ie}.csv")

        sdat = js_dat.reset_index(drop=True)
        sdat.to_csv(f"data/full_sens/{NAGENTS}-ie{sep_ie}.csv")
        return 0
    except:
        return 1
        



#main(10, 0.25)
#tdat = pd.read_feather('data/v0.50/ks_data.feather')
#tdat

#mk_dat = calc_anom_det(tdat)
#mk_dat

perms = [(x, y) for x in range(2, 100, 1) for y in (np.linspace(0, 50, 50)/100)]

# perms = [(x, y) for x in range(75, 100, 1) for y in (np.linspace(25, 25, 1)/100)]

#perms_ = perms[0]

#-----------------------------------------
# Parallel loop
ncores = 50
pool = multiprocessing.Pool(ncores, maxtasksperchild=1)         
pool.map(main, perms)
pool.close()


