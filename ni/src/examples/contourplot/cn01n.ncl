;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn01n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Tue Oct  4 18:48:08 MDT 1994
;
;   Description:     Given a simple mathematically generated data set,
;            demonstrates ContourPlot with all resources (other than
;            cnScalarFieldData) set to their default value.
;

begin

;
; create a simple bull's eye pattern test data set
;

M=25
N=25
 
T = new((/N,M/),float)
len_dims = new((/2/),integer)
 
;
; create a simple bull's eye pattern test data set
;
PI = 3.14159
 
jspn = ispan(-M/2,M/2,1)^2
ispn = ispan(-N/2,N/2,1)^2
do i = 0, dimsizes(ispn)-1
        T(i,:) = ispn(i) + jspn
end do
T = 100.0 - sqrt(8^2 * T)


;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "cn01" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm")
;
; Create an NCGM workstation.
;
  wid = create "cn01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "cn01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "cn01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./cn01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "cn01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./cn01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wid = create "cn01Work" documentWorkstationClass defaultapp
    "wkFileName" : "./cn01n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wid = create "cn01Work" imageWorkstationClass defaultapp
    "wkFileName" : "./cn01n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a ScalarField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

dataid = create "bullseye" scalarFieldClass appid
        "sfDataArray" : T
end create

;
; Create a ContourPlot object, supplying the ScalarField object as data
;
cnid = create "ContourPlot1" contourPlotClass wid
    "cnScalarFieldData" : dataid
end create

;
; Draw a plot illustrating the basic default behavior of
; the ContourPlot object. The contours appear as solid lines with unboxed
; labels in a linear coordinate system with the origin at the lower left. 
; Tickmarks with labels show the data coordinate range, and an 
; informational label at the lower right gives the minimum and maximum
; data values and the contour interval spacing.
;

draw(cnid)
frame(wid)
    
;
; Destroy the objects created, close the HLU library and exit.
;
delete(dataid)
delete(cnid)
delete(wid)
delete(appid)

end
