;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc05n.ncl
;
;   Author: Lynn Hermanson
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
; 
;   Date:       Mar  27, 1996
;
;   Description:     Given a simple mathematically generated data set,
;                    which looks like the intersection of two vector fields
;                    with opposite directions,this program produces three
;                    plot frames which demonstrate the use of VectorPlot;1st-
;                    default resources, 2nd- overlayed on a world map, 3rd-
;                    overlayed on an orthographic projection of the globe
;                    centered on the north pole covering 90 to 40 degrees
;                    north latitude.
;
;  Begin by generating the data set called "increasingvectors".
;  The values in the array "U" are defined to be the magnitude of the x 
;  component of the vectors.  The values in the array "V" are defined to be
;  the magnitude of the y component of the vectors. The location of the 
;  vectors corresponds to the indices of the arrays. Each index pair will
;  eventually correspond to a latitude,longitude location on a map. 

begin

M=25
N=25

U = onedtond(fspan(0,M-1,M),(/N,M/))
U!0 = "x"
U!1 = "y"
V = 2.5*(U(y | :,x | :) + 2)/1.4142
U = 2.5*(U + 2)/1.4142

; Next,
; create an application context. Set the app dir to the current directory
; so the application looks for a resource file (vc05.res) in the working
; directory. The necessary resources
; will be set in this ncl script as each object is created.
;
appid = create "vc05" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

; Next, choose the type of output workstation for all 3 frames.
; Choose to display output to an X11 workstation. Change the output from the
; default colorMap (random sequence of colors) to a  sequence
; of colors so that vectors of increasing magnitude will move from cooler
; to warmer colors.

wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "vc05Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc05n.ncgm"
    "wkColorMap" : "temp1"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "vc05Work" windowWorkstationClass defaultapp
    "wkPause" : True
;;;*******see resource file vc05.res ******
;;;     "wkColorMap" : "temp1"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc05Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./vc05n.ps"
    "wkColorMap" : "temp1"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc05Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./vc05n.pdf"
    "wkColorMap" : "temp1"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc05Work" documentWorkstationClass defaultapp
    "wkFileName" : "./vc05n"
    "wkFormat" : wks_type
    "wkColorMap" : "temp1"
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc05Work" imageWorkstationClass defaultapp
    "wkFileName" : "./vc05n"
    "wkFormat" : wks_type
    "wkColorMap" : "temp1"
  end create
end if

; BEGIN CREATING 1st FRAME

; Create the 1st VectorField data object using the data set generated above.

dataid = create "increasingvectors" vectorFieldClass appid
        "vfUDataArray" : U
        "vfVDataArray" : V
end create

; Create the 1st VectorPlot object, supplying the 1st VectorField object as data.
; Turn off black and white arrows and use the temp1 colors spread between
; the lowest and highest vector magnitudes. The default vectors appear as
; solid lines with arrowheads in a linear coordinate system with the
; origin at the lower left. Tickmarks with labels show the data coordinate
; range, and an informational label at the lower right gives the reference 
; vector value.

vcid = create "VectorPlot1" vectorPlotClass wid
    "vcVectorFieldData" : dataid
;;;********* see resource file vc05.res ******
;;;   "vcMonoLineArrowColor" : False
;;;   "vcMonoFillArrowFillColor" : False
;;;   "vcMaxLevelCount" : 61
end create

; Draw the 1st frame, signify end of 1st frame.

draw(vcid)
frame(wid)

; Destroy the objects created for the 1st frame. 
; Keep wid and appid for 2nd frame.

delete(dataid)
delete(vcid)

; BEGIN CREATING 2nd FRAME

; Create the 2nd VectorField data object using the same data.
; Map the domain and range of the vector locations to the latitude
; and longitude ranges of the world map on which the vectors
; will be overlayed. Thus, cause the x axis locations to cover -180
; to 180 degrees longitude, and cause the y axis locations to cover
; -90 to 90 degrees latitude.

dataid = create "increasingvectors2" vectorFieldClass appid
        "vfUDataArray" : U
        "vfVDataArray" : V

;;;******** see resource file vc05.res ******
;;; "vfXCStartV" : -180
;;;     "vfXCEndV" : 180
;;;     "vfYCStartV" : -90
;;;     "vfYCEndV" : 90
end create


; Create the 2nd VectorPlot object, as in the 1st frame.
; Make the minimum vector length 1/3 of the reference vector length.

vcid = create "VectorPlot2" vectorPlotClass wid
    "vcVectorFieldData" : dataid
;**********see file vc05.res************
;;; "vcMonoLineArrowColor" : False
;;; "vcMonoFillArrowFillColor" : False
;;; "vcMinFracLengthF" : .33
;;;     "vcMaxLevelCount" : 61  
end create

; Create a mapPlot object consisting of, the default, gridded world map.
; Add a title object which says "vc05 frame 2".
; Turn off the grid lines. 

mapid = create "Map1" mapPlotClass wid
        "pmTitleDisplayMode" : "always"
        "tiMainString" : "vc05 frame 2"
;;;******** see vc05.res ********
;;; "mpGridAndLimbOn" : False
end create

; Overlay the VectorPlot2 VectorPlot object onto the Map1 MapPlot object.

overlay(mapid, vcid)

; Draw the 2nd frame. Signify end of 2nd frame.

draw(mapid)
frame(wid)

; Destroy the objects created for 2nd frame. Keep wid and appid for 3rd frame.

delete(dataid)
delete(vcid)
delete(mapid)


; BEGIN CREATING 3rd FRAME

; Create a data object from the same data.
; Cause the x axis data locations to cover -180 to 180 degrees longitude.
; Cause the y axis data locations to cover 50 degrees latitude up to the
; north pole.


dataid = create "increasingvectors3" vectorFieldClass appid
        "vfUDataArray" : U
        "vfVDataArray" : V
;;;*********see file vc05.res********
;;;        "vfXCStartV" : -180
;;;        "vfXCEndV" : 180

;;;        "vfYCStartV" : 50
;;;        "vfYCEndV" : 90
end create

; Create a VectorPlot object, supplying the VectorField object as data,
; as in frame 2. 
; Double the default vector length in NDC. The default was to dynamically
; calulate it by dividing viewport width by the number of data values.
; Display a horizontal label bar (at the bottom of the plot)
; showing the color levels and their corresponding magnitudes.
; Decrease the height of the label bar to .1 NDC so that it fits into the
; default viewPort.

vcid = create "VectorPlot3" vectorPlotClass wid
    "vcVectorFieldData" : dataid
;;;******* see file vc05.res *********
;;; "vcMonoLineArrowColor" : False
;;; "vcMonoFillArrowFillColor" : False
;;;     "vcMaxLevelCount" : 61
;;; "vcMinFracLengthF" : .33  
;;; "vcRefLengthF" : .05
;;; "pmLabelBarDisplayMode" : "always"
;;; "pmLabelBarSide" : "bottom"
;;; "lbOrientation"  : "horizontal"
;;; "pmLabelBarHeightF" : .11
;;; "pmLabelBarWidthF" : .6

end create


; Create an orthographic map of the world, centered at the north pole,
; making the points at all four of the frame edges be 50 degrees north
; latitude. Make the center of the top edge of the frame be 180 degrees 
; longitude, the center of the bottom edge of the frame correspond to
; 0 degrees longitude, the center of the left edge of the frame correspond
; to -90 degrees longitude,and the center of right edge of the frame
; correspond to +90 degrees longitude.
; Create a title object which says "vc05 frame 3".
; Turn off the grid lines.

mapid = create "Map2" mapPlotClass wid

        "pmTitleDisplayMode" : "always"
        "tiMainString" : "vc05 frame 3"
;;;******* see file vc05.res *******
;;; "mpProjection" : "Orthographic"
;;; "mpCenterLatF" : 90.0 
;;; "mpCenterLonF" : 0.0 

;;;        "mpLimitMode"  : "points"
;;;        "mpBottomPointLatF" : 50
;;;        "mpTopPointLatF" : 50
;;;        "mpLeftPointLatF" : 50
;;;        "mpRightPointLatF" : 50
;;;        "mpBottomPointLonF" : 0
;;;        "mpTopPointLonF" : 180
;;;        "mpLeftPointLonF" : -90
;;;        "mpRightPointLonF" : 90

;;; "mpGridAndLimbOn" : False
end create

; Overlay the VectorPlot3 VectorPlot object onto the Map2 MapPlot object.

overlay(mapid, vcid)

; Draw the 3rd frame. Signify end of 3rd frame.
    
draw(mapid)
frame(wid)

; Destroy all of the objects created, close the HLU library and exit.

delete(dataid)
delete(vcid)
delete(mapid)
delete(wid)
delete(appid)

end
