import pandas as pd
import numpy as np

def dist(x1, x2, y1, y2):
    return ( np.sqrt( (x2 - x1)**2 + (y2 - y1)**2) )

dat = pd.read_feather('data/vessel_dat.feather')

dat

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
    distMatrix = pd.DataFrame(d).pivot(index=0, columns=1, values=2)
    distMatrix['t'] = t
    return distMatrix                

# Get all t
dat2 = dat.groupby('t').apply(lambda x: dist_mat(x))

# Save
dat2.columns = dat2.columns.astype(str)
dat2 = dat2.reset_index(drop=True)
dat2.to_feather('data/all_dist_matrix.feather')


# Check dist formula
# 0-1
dist(dat.loc[0, 'x1'], dat.loc[1, 'x1'], dat.loc[0, 'y1'], dat.loc[1, 'y1'])

# 0-2
dist(0.266381, 0.310365, 0.259279, 0.326251)
        
    
    
