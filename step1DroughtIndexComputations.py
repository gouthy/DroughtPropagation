from netCDF4 import Dataset  
from glob import glob
import numpy as np
from scipy import stats

def npSI(data,sc=3):
    SI = np.empty((data.shape[0]))
    td = data
    SI[:] = np.nan
    datax=[];
    for i in range(sc):
        datax.append(td[i:len(td)-sc+i+1]);
    dataxarr = np.array(datax)
    dataxarr=np.stack(dataxarr, axis=1)
    sumdatax=dataxarr.sum(axis=1)
    nn=len(sumdatax);
    SI1=np.empty((nn))
    for k in range(12):
        d = sumdatax[k::12]
        nnn = len(d)
        bp=np.empty((nnn))
        bp[:]=0;
        for i in range(nnn):
            bp[i]=np.sum(d<=d[i])
        y=(bp-0.44)/(nnn+0.12)
        SI1[k::12]=y;
    SI1[:]=stats.norm.ppf(SI1[:]);
    SI[(sc-1):(len(td))]=SI1;
    return SI



ncfiles=glob('./*da_*smtws.nc')
ncfiles.sort()
ncfiles = [i.replace('da_', '010_ol_') for i in ncfiles]
ncin=Dataset(ncfiles[0])
lat = np.array(ncin.variables['lat'][:].reshape(-1))
lat[lat<(-200)]=np.nan
lon = np.array(ncin.variables['lon'][:].reshape(-1))
lon[lon<(-200)]=np.nan
ncin.close()

ncfilesda=glob('./*da_*smtws.nc')

tws = []
gws = []
sm = []
pe = []
for i in range(len(ncfiles)):
    ncin=Dataset(ncfiles[i])
    gws.append(np.array(ncin.variables['gws'][:]).reshape(-1))
    tws.append(np.array(ncin.variables['tws'][:]).reshape(-1))
    et = np.array(ncin.variables['evap'][:]).reshape(-1)
    sm.append(np.array(ncin.variables['soil_moisture'][:]).reshape(-1))
    ncin.close()
    ncin=Dataset(ncfilesda[i].replace('da_', 'ol_'))
    pe.append(np.array(ncin.variables['prcp'][:]).reshape(-1) - et)
    ncin.close()
sm = np.stack(sm,axis=1)
pe = np.stack(pe,axis=1)
gws = np.stack(gws,axis=1)
tws = np.stack(tws,axis=1)
smnonan = sm[~np.isnan(lat),:]
smt = smnonan.T
del smnonan

gwsnonan = gws[~np.isnan(lat),:]
gwsft = gwsnonan.T
del gwsnonan

twsnonan = tws[~np.isnan(lat),:]
twsft = twsnonan.T
del twsnonan


penonan = pe[~np.isnan(lat),:]
pet = penonan.T
del penonan
smi=[]
sgi = []
sti = []
spei = []
for i in range(smt.shape[1]):
    smi.append(npSI(smt[:,i]))
    spei.append(npSI(pet[:,i]))
    sgi.append(npSI(gwsft[:,i]))

smx = np.stack(smi,axis=1)
spx = np.stack(spei,axis=1)
gmx = np.stack(sgi,axis=1)
np.savetxt("./smi_ol.txt", smx)
np.savetxt("./sgi_ol.txt", gmx)
np.savetxt("./spei.txt", spx)


ncfiles=glob('./*da_*smtws.nc')
ncfiles.sort()
ncin=Dataset(ncfiles[0])
lat = np.array(ncin.variables['lat'][:].reshape(-1))
lat[lat<(-200)]=np.nan
lon = np.array(ncin.variables['lon'][:].reshape(-1))
lon[lon<(-200)]=np.nan
ncin.close()
gws = []
tws =[]
sm = []
tws = []
pe = []
for i in range(len(ncfiles)):
    ncin=Dataset(ncfiles[i])
    gws.append(np.array(ncin.variables['gws'][:]).reshape(-1))
    tws.append(np.array(ncin.variables['tws'][:]).reshape(-1))
    et = np.array(ncin.variables['evap'][:]).reshape(-1)
    sm.append(np.array(ncin.variables['soil_moisture'][:]).reshape(-1))
    ncin.close()
    ncin=Dataset(ncfiles[i].replace('da_', 'ol_'))
    ncin.close()
sm = np.stack(sm,axis=1)
gws = np.stack(gws,axis=1)
tws = np.stack(tws,axis=1)
smnonan = sm[~np.isnan(lat),:]
smt = smnonan.T
del smnonan

gwsnonan = gws[~np.isnan(lat),:]
gwsft = gwsnonan.T
del gwsnonan

twsnonan = tws[~np.isnan(lat),:]
twsft = twsnonan.T
del twsnonan


smi=[]
sgi = []
sti = []
spei = []
for i in range(smt.shape[1]):
    smi.append(npSI(smt[:,i]))
    sgi.append(npSI(gwsft[:,i]))
    sti.append(npSI(twsft[:,i]))

smx = np.stack(smi,axis=1)
gmx = np.stack(sgi,axis=1)


np.savetxt("./smi_da_new.txt", smx)
np.savetxt("./sgi_da_new.txt", gmx)

