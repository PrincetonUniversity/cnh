
#==== Functions
#! moving average filter
def movingaverage(values,window):
    import numpy as np
    weights = np.repeat(1.0, window)/window
    smas = np.convolve(values, weights, 'valid')
    return smas

#! Python implementation of the min/max algorithm
def fnc_maxmin(speed,thresh):
    import numpy as np
    Zdist = np.cumsum(speed - np.mean(speed))
    #thresh = 5
    count = 1
    idx = 1
    Zopt = Zdist[0]
    opttbl = np.empty((0,3))

    if Zdist[0] < np.median(Zdist):
        sg = "max" # max 
    else:
        sg = "min" # min

    i = 0
    while (i < Zdist.shape[0]-1):

        i += 1
        # Is next value greater (max) or less (min)
        if (sg=='min'):
            test = (Zopt > Zdist[i])
        else:
            test = (Zopt < Zdist[i])
        # if so, replace Zopt
        if test:
            Zopt = Zdist[i]
            idx = i
            count = 1
        # if not
        else:
            count += 1
        # if threshold reached
        if count > thresh:
            count = 0
            # save here
            add = (idx,Zopt,sg)
            opttbl = np.vstack((opttbl,add))
            i = idx
            if (sg=='min'):
                sg = 'max'
            else:
                sg = 'min'
    return opttbl


