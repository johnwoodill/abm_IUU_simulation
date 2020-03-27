import pandas as pd
import numpy as np
from math import radians, cos, sin, asin, sqrt

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


dat = pd.read_feather(f"data/v0.51/vessel_dat_{NAGENTS}_{ie}.feather")


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


# Get all t
dat2 = dat.groupby('t').apply(lambda x: dist_mat(x))

# Save
dat2.columns = dat2.columns.astype(str)
dat2 = dat2.reset_index(drop=True)
dat2.to_feather(f"data/v0.51/all_dist_matrix_{NAGENTS}_{ie}.feather")


# Check dist formula
# 0-1
dist(dat.loc[0, 'x1'], dat.loc[1, 'x1'], dat.loc[0, 'y1'], dat.loc[1, 'y1'])

 
    
    
