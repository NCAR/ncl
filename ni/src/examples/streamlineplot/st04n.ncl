;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       st04n.ncl
;
;
;   Author: David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:  Wed Apr  12 17:00:55 MST 1996
;
;   Description: This example shows a StreamlinePlot of 500 mb wind 
;                vector data overlaid on a MapPlot. The streamlines
;                are drawn over a VectorPlot of surface winds colored
;                by surface pressure that in turn is drawn over a filled
;                ContourPlot of surface temperature. Different intervals
;                of the "temp1" colormap are used to color the contour
;                levels and the vectors.
;                The data represents 15 days of weather over North
;                America in January, 1996.
;                The data is extracted from NMC forcast data produced 
;                at 12 hour intervals and converted to netcdf format 
;                by Unidata. Most of the time steps in the files
;                extracted from the original data are taken from the 
;                0 and 6 hour forecast times. However, because some of the 
;                original files were lost, certain time steps come from
;                longer range forcasts. Also, several steps had to be
;                excluded from the frame set because the data is 
;                defective. The result is that there is an 
;                apparent discontinuity between some of the frames 
;                when the output is animated.
;

begin

;
; Depending on the value of the TIMESTEPS variable declared below,
; this example example can generate up to 61 frames from the 64
; timesteps in the data files. As shipped, only the first 20 frames 
; are created. To see the complete plot uncomment the second 
; assignment to TIMESTEPS. Some systems may not have enough physical
; memory to allow all frames to be viewed as an animation.
;

TIMESTEPS = 20
;TIMESTEPS = 64

;
; Create an application object.
;

  appid = create "st04" appClass defaultapp
      "appUsrDir" : "./"
      "appDefaultParent" : True
  end create
;
; Default is to display output to an X workstation
;
  wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
      wid = create "st04Work" ncgmWorkstationClass defaultapp
         "wkMetaName" : "./st04n.ncgm"
         "wkColorMap" : "temp1"
      end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "st04Work" windowWorkstationClass defaultapp
       "wkPause" : True
       "wkColorMap" : "temp1"
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "st04Work" psWorkstationClass defaultapp
     "wkPSFileName" : "./st04n.ps"
     "wkColorMap"   : "temp1"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "st04Work" pdfWorkstationClass defaultapp
     "wkPDFFileName" : "./st04n.pdf"
     "wkColorMap"    : "temp1"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "st04Work" documentWorkstationClass defaultapp
     "wkFileName" : "./st04n"
     "wkFormat" : wks_type
     "wkColorMap"    : "temp1"
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "st04Work" imageWorkstationClass defaultapp
     "wkFileName" : "./st04n"
     "wkFormat" : wks_type
     "wkColorMap"    : "temp1"
    end create
  end if

;
; Read the data files 
;

dir = ncargpath("data") + "/cdf/"
uf = addfile(dir+"Ustorm.cdf","r")
vf = addfile(dir+"Vstorm.cdf","r")
pf = addfile(dir+"Pstorm.cdf","r")
tf = addfile(dir+"Tstorm.cdf","r")
u500f = addfile(dir+"U500storm.cdf","r")
v500f = addfile(dir+"V500storm.cdf","r")

; 
; Create a VectorField of the surface wind data
;

vfield = create "VectorField" vectorFieldClass appid
    "vfUDataArray" : uf->u(0,:,:)
    "vfVDataArray" : vf->v(0,:,:)
    "vfXCStartV" : uf->lon(0)
    "vfYCStartV" : uf->lat(0)
    "vfXCEndV" : uf->lon(filevardimsizes(uf,"lon") - 1)
    "vfYCEndV" : uf->lat(filevardimsizes(uf,"lat") - 1)
    "vfMissingUValueV" : -9999.0
end create

;
; Create a VectorField of 500 millibar wind data
;

vfield2 = create "VectorField" vectorFieldClass appid
    "vfUDataArray" : u500f->u(0,:,:)
    "vfVDataArray" : v500f->v(0,:,:)
    "vfXCStartV" : u500f->lon(0)
    "vfYCStartV" : u500f->lat(0)
    "vfXCEndV" : u500f->lon(filevardimsizes(uf,"lon") - 1)
    "vfYCEndV" : u500f->lat(filevardimsizes(uf,"lat") - 1)
    "vfMissingUValueV" : -9999.0
end create

;
; Create a ScalarField of surface pressure 
;

sfield = create "ScalarField" scalarFieldClass appid
    "sfDataArray" : pf->p(0,:,:) / 100.0
    "sfXCStartV" : pf->lon(0)
    "sfYCStartV" : pf->lat(0)
    "sfXCEndV" : pf->lon(filevardimsizes(pf,"lon") - 1)
    "sfYCEndV" : pf->lat(filevardimsizes(pf,"lat") - 1)
    "sfMissingValueV" : -9999.0
end create

;
; Create a ScalarField of surface temperature 
; (convert from Kelvin to Farenheit)
;

sfield2 = create "ScalarField2" scalarFieldClass appid
    "sfDataArray" : (tf->t(0,:,:)  - 273.15)*9.0/5.0 +32.0
    "sfXCStartV" : tf->lon(0)
    "sfYCStartV" : tf->lat(0)
    "sfXCEndV" : tf->lon(filevardimsizes(tf,"lon") - 1)
    "sfYCEndV" : tf->lat(filevardimsizes(tf,"lat") - 1)
    "sfMissingValueV" : -9999.0
end create

;
; Create a ContourPlot with surface temperature data
;
 
cnid = create "contourplot" contourPlotClass wid
    "cnFillOn" : True
    "cnLinesOn" : False
    "cnFillDrawOrder" : "predraw"
    "cnScalarFieldData":      sfield2
end create

;
; Create a VectorPlot with the surface wind and pressure data
;

vcid = create "vectorplot" vectorPlotClass wid
    "vcUseScalarArray" :  True
    "vcVectorFieldData":  vfield
    "vcScalarFieldData":  sfield
end create

;
; Create a StreamlinePlot with 500 mb wind data
;

stid = create "streamlineplot" streamlinePlotClass wid
    "pmTitleDisplayMode" : "always"
    "tiMainFuncCode" : "~"
    "stVectorFieldData":      vfield2
 end create

;
; Create an annotation used to explain the streamline data
;
 
txid = create "streamlineplotanno" textItemClass wid 
end create
amid = NhlAddAnnotation(stid,txid)

;
; Create a map object
;

mapid = create "mapplot" mapPlotClass wid 
;        "vpUseSegments" : True
end create

;
; Overlay everything on the MapPlot. The last object overlaid will
; appear on top
;

overlay(mapid,cnid)
overlay(mapid,vcid)
overlay(mapid,stid)

;
; Variables for manipulating the title string
;

time = vf->timestep
hour = new(1,string)
day = new(1,string)

do i = 0, TIMESTEPS - 1
	if (i.ne.17 .and. i.ne.36 .and. i.ne.37) then

;
; Figure out the hour and day from the timestep, convert to strings
; and build the title string
;
		d = time(i) / 24 + 5
		h = time(i) % 24
		if (h .gt. 9) then
			hour = h
		else 	
			hour = "0" + h
		end if
		if (d .gt. 9) then
			day = d
		else 
			day = "0" + d
		end if
		mainstring = vf->reftime(0:7) + day + " " + hour + ":00"
		print(mainstring)
;
; Set the new title string
;
		setvalues stid
		    "tiMainString" : mainstring
		end setvalues	
;
; Modify the data objects with data for the current time step
;
		setvalues vfield
		    "vfUDataArray" : uf->u(i,:,:)
		    "vfVDataArray" : vf->v(i,:,:)	
		end setvalues	
		setvalues vfield2
		    "vfUDataArray" : u500f->u(i,:,:)
		    "vfVDataArray" : v500f->v(i,:,:)	
		end setvalues	
		setvalues sfield
		    "sfDataArray" : pf->p(i,:,:) / 100.0
		end setvalues	
		setvalues sfield2
		    "sfDataArray" : (tf->t(i,:,:) - 273.15)*9.0/5.0 +32.0
		end setvalues
; 
; Draw the plot
;		
		draw(mapid)
		frame(wid)
	end if
end do

;
; Destroy the workstation object and exit.
;
  delete(wid)
end
