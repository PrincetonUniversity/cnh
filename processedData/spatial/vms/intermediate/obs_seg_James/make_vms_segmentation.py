

###### Packages
import numpy as np
from numpy import genfromtxt
import datetime as datetime
import matplotlib.pylab as plt
from geopy.distance import vincenty
from scipy.io import netcdf
from scipy.interpolate import griddata
import pandas as pd
import csv
from sub_vms import *
import glob as gb



#==================== DATA =================#
#! VMS data
#! will need to loop through vessels
files = gb.glob("../05_make_obs_to_vms/*.csv")
files = files[0::6] # subset

#! Fishing array that we will fill up (lon,lat,time,metier)
#! and vms segmentation statistics (tr+pr+,tr-pr+,tr+pr-,tr-pr-,metier)
FISHING = np.empty((0,4))
STATS = np.empty((0,5))
tick = 0

#! Add header to arrays to save
FISHING = np.append(FISHING,np.vstack(('lon','lat','time','metier')).T,axis=0)
STATS = np.append(STATS,np.vstack(('tr+pr+','tr-pr+','tr+pr-','tr-pr-','metier')).T,axis=0)

for infile in files:
    #! load vms data
    tick += 1
    print(float(tick) / float(np.shape(files)[0]))
    #print(infile)
    #infile = "./VMS/CSV/v635397.csv"
    my_data = pd.read_csv(infile)
    my_data = my_data.values


#==== Convert all times to ROMS time 
    #! which is seconds after 1900-01-01 00:00:00
    before = datetime.datetime(1900, 01, 01, 00, 00, 00)
    N = my_data.shape[0]
    Lon = np.zeros(N) # lon
    Lat = np.zeros(N) # lat
    Trp = np.zeros(N) # trip id (cut by proximity to land)
    Obs = np.empty(N,dtype=object) # observed or not
    Set = np.empty(N,dtype=object) # was there an observed set
    Met = np.empty(N,dtype=object) # metier (from Emma)
    Time = np.zeros(N) # time

    for i in np.arange(0,N):
        #! space
        Lon[i] = my_data[i][0]
        Lat[i] = my_data[i][1]
        Trp[i] = my_data[i][4]
        Set[i] = np.str(my_data[i][6])
        Obs[i] = np.str(my_data[i][7])
        Met[i] = np.str(my_data[i][5])

        #! time
        yr = np.int(my_data[i][2][0:4])
        mt = np.int(my_data[i][2][5:7])
        dy = np.int(my_data[i][2][8:10])
        hr = np.int(my_data[i][2][11:13])
        mn = np.int(my_data[i][2][14:16])
        sc = np.int(my_data[i][2][17:19])
        after = datetime.datetime(yr,mt,dy,hr,mn,sc)
        dt = after - before
        Time[i] = dt.total_seconds()


#==================== TRIPS ==================#
#==== Cut out long periods of time on land
    ID = np.where(Trp>0)
    Lon_o = Lon[ID]
    Lat_o = Lat[ID]
    Set_o = Set[ID]
    Obs_o = Obs[ID]
    Trp_o = Trp[ID]
    Met_o = Met[ID]
    Tim_o = Time[ID]

#==== Only keep trips longer than X hrs
    X = 10
    for tr in np.unique(Trp_o):
        ID = np.where(Trp_o == tr)[0]
        if ID.shape[0] <= X: # choose cutoff (min # conseq ocean points = a trip)
            Trp_o = np.delete(Trp_o,ID)
            Lon_o = np.delete(Lon_o,ID)
            Lat_o = np.delete(Lat_o,ID)
            Tim_o = np.delete(Tim_o,ID)
            Met_o = np.delete(Met_o,ID)
            Set_o = np.delete(Set_o,ID)
            Obs_o = np.delete(Obs_o,ID)
    trips = np.unique(Trp_o);



#================= Behavioral Segementation =================#
    #tick = 0
    for TR in trips:
    	#tick += 1
    	#print(float(tick) / float(trips.shape[0]))
        ID   =  np.where(Trp_o == TR)[0]
        lon  =  Lon_o[ID]
        lat  =  Lat_o[ID]
        time =  Tim_o[ID]
        sets =  Set_o[ID]
        obs  =  Obs_o[ID]
        met  =  Met_o[ID]

        #! remove any duplicate locations
        dup_id = np.where(np.diff(time)==0.)
        time = np.delete(time,dup_id)
        lon  = np.delete(lon,dup_id)
        lat  = np.delete(lat,dup_id)
        sets = np.delete(sets,dup_id)
        obs  = np.delete(obs,dup_id)
        met  = np.delete(met,dup_id)

        #! only segment if metier assigned and if trip isn't stationary
        if np.any(met != 'nan') and np.unique(lon).shape[0] > 1:

            dist = np.zeros(lon.size-1)
            for i in np.arange(0,lon.size-1):
                dist[i] = vincenty((lon[i],lat[i]),(lon[i+1],lat[i+1])).km
            dt = np.diff(time) / 60 / 60 # hours
            speed = dist / dt; # km per hour

            #! smoothed trajectory (to get over VMS grid)
            #speed_1 = movingaverage(speed,1); # moving average speed
            #lon_1   = movingaverage(lon,1)[0:-1]
            #lat_1   = movingaverage(lat,1)[0:-1]
            #time_1  = movingaverage(time,1)[0:-1]
            #lon_1  = lon[0:-1]
            #lat_1  = lat[0:-1]
            #time_1 = time[0:-1]

            #! maxmin algorithm
            N = np.floor(speed.shape[0]/2) # max window size
            beh = np.zeros(N) # the number of behavioral changes
            for i in np.arange(0,N):
                opttbl = fnc_maxmin(speed,i)
                beh[i] = opttbl.shape[0]

            #! id optimal threshold as the first window size with two consequtive no change in beh
            # i am testing different conditions for finding the "qualitative" signal that 
            # the optimal window size has been found 
            #ID = np.where(np.abs(np.diff(beh))==0.)[0][0]
            #ID = np.where(beh>2)[0][-1]
            #ID = np.where(np.gradient(beh)==0)[0][0]
            #ID  = np.where(beh==1)[0][0]-1 # first window size with > 1 behaviors
            ID = np.where(np.abs(np.diff(beh))<=2.)[0][0]
            OPT = fnc_maxmin(speed,ID)

            #! if first behavioral change in a min, remove
            if (OPT[0][2] == 'min'):
                OPT = OPT[1:]
            if (OPT[-1][2]=='max'):
                OPT = OPT[0:-1,:]

            #! Index fishing as inferred from VMS
            V_FISH = np.zeros(lon.shape)
            for i in np.arange(0,OPT.shape[0],2):
                head = np.int(OPT[i][0])
                tail = np.int(OPT[i+1][0])
                V_FISH[head:tail] = 1

            #! Calculate statistics
            #! only calc stats if trip observed
            if np.any(Obs != '0'):
                # true +ve = number of vms fishing events that were obs as fishing
                # false +ve = number of vms fishing events that were no obs as fishing
                tpos_ppos = np.intersect1d(np.where(sets!='0'),np.where(V_FISH==1)).shape[0]
                tneg_ppos = np.intersect1d(np.where(sets=='0'),np.where(V_FISH==1)).shape[0]
                tpos_pneg = np.intersect1d(np.where(sets!='0'),np.where(V_FISH==0)).shape[0]
                tneg_pneg = np.intersect1d(np.where(sets=='0'),np.where(V_FISH==0)).shape[0]
                STATS = np.append(STATS,np.vstack((tpos_ppos,tneg_ppos,
                                tpos_pneg,tneg_pneg,np.unique(met)[0])).T,axis=0)

            #! Save a fishing array
            ID = np.where(V_FISH==1)[0]
            FISHING = np.append(FISHING,np.vstack((lon[ID],lat[ID],time[ID],met[ID])).T,axis=0)


#============== Save and Exploratory plot ==============#
with open("FISHING.csv", 'wb') as f:
    csv.writer(f).writerows(FISHING)
with open("STATS.csv", 'wb') as f:
    csv.writer(f).writerows(STATS)

#plt.figure()
#plt.plot(lon,lat)
#plt.plot(FISHING[:,0],FISHING[:,1],'ro')



