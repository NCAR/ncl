;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       st01n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description:   Basic StreamlinePlot example
;

begin

N=30
M=25
PI=3.14159

;
; Generate vector data arrays
;
 
 
V = 10.0 * cos(onedtond((2.0 * PI / M) * ispan(0,M-1,1),(/N,M/)))
U = 10.0 * cos(onedtond((2.0 * PI / N) * ispan(0,N-1,1),(/M,N/)))
 
;
; We will need to transpose U so dimension names will be needed
;
 
U!0 = "x"
U!1 = "y"

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
appid = create "st01" appClass defaultapp
    "appUsrDir" : "./"
        "appDefaultParent" : True
end create

;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

if (NCGM.eq.1) then
;
; Create an NCGM workstation.
;
  wid = create "st01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./st01n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "st01Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "st01Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./st01n.ps"
      end create
    end if
  end if
end if

;
; Create a VectorField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vectorfield" vectorFieldClass appid
        "vfUDataArray" : U( y | :, x | :)  ; Transpose of U
	"vfVDataArray" : V
end create

;
; Create a StreamlinePlot object, supplying the VectorField object as data.
;

stid = create "streamlineplot" streamlinePlotClass wid
    "tiMainString" : "Basic StreamlinePlot Example"
    "stVectorFieldData" : vfid
end create

draw(stid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
