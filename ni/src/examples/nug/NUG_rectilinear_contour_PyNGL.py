"""
  NCL User Guide Python Example:   PyNGL_rectilinear_contour.py

   - filled contour over map plot
   - rectilinear grid (lat/lon)
   - colorbar
   
  2015-06-04  kmf
"""
import Ngl,Nio
import os,sys

#-- define variables
diri   = "./"                                  #-- data directory
fname  = "rectilinear_grid_2D.nc"              #-- data file name

#---Test if file exists
if(not os.path.exists(diri+fname)):
  print("You do not have the necessary file (%s) to run this example." % (diri+fname))
  print("You can get the files from the NCL website at:")
  print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/")
  sys.exit()

minval =  250.                                 #-- minimum contour level
maxval =  315                                  #-- maximum contour level
inc    =    5.                                 #-- contour level spacing

#-- open file and read variables
f      =  Nio.open_file(diri + fname,"r")      #-- open data file
temp   =  f.variables["tsurf"][0,:,:]          #-- first time step
lat    =  f.variables["lat"][:]                #-- all latitudes
lon    =  f.variables["lon"][:]                #-- all longitudes

tempac,lon =  Ngl.add_cyclic(temp,lon)

#-- open a workstation
wks_type               = "png"                 #-- graphics output type
wkres                  =  Ngl.Resources()      #-- generate an res object 
                                               #-- for workstation
wkres.wkWidth          =  2500                 #-- plot res 2500 pixel width
wkres.wkHeight         =  2500                 #-- plot resolution 2500 
wks                    =  Ngl.open_wks(wks_type,"plot_contour_ngl",wkres)  #-- open workstation

#-- set resources
res                    =  Ngl.Resources()      #-- generate an resource object for plot

if hasattr(f.variables["tsurf"],"long_name"):
   res.tiMainString = f.variables["tsurf"].long_name  #-- set main title

res.cnFillOn              =  True              #-- turn on contour fill.
res.cnLinesOn             =  False             #-- turn off contour lines
res.cnLineLabelsOn        =  False             #-- turn off line labels.
res.cnInfoLabelOn         =  False             #-- turn off info label.
res.cnLevelSelectionMode  = "ManualLevels"     #-- select manual level selection mode
res.cnMinLevelValF        =  minval            #-- minimum contour value
res.cnMaxLevelValF        =  maxval            #-- maximum contour value
res.cnLevelSpacingF       =  inc               #-- contour increment
res.cnFillPalette         = "rainbow"          #-- choose color map

res.mpGridSpacingF        =  30                #-- map grid spacing

res.sfXArray              =  lon               #-- longitude locations of data
res.sfYArray              =  lat               #-- latitude locations of data

res.lbOrientation         = "Horizontal"       #-- labelbar orientation

map = Ngl.contour_map(wks,tempac,res)          #-- draw contours over a map.

#-- end
Ngl.end()

