;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       st02n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description:   Given a simple mathematically generated data set,
;		   demonstrates line-drawn streamline arrows and the use
;                  of some basic StreamlinePlot resources
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
appid = create "st02" appClass defaultapp
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
  wid = create "st02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./st02n.ncgm"
  end create
else
  if (X11.eq.1) then
;
; Create an X workstation.
;
    wid = create "st02Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PS workstation.
;
      wid = create "st02Work" psWorkstationClass defaultapp
        "wkPSFileName" : "./st02n.ps"
      end create
    end if
  end if
end if

;
; Create a VectorField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vfield" vectorFieldClass appid
        "vfUDataArray" : U( y | :, x | :)
	"vfVDataArray" : V
end create

;
; Create a StreamlinePlot object, supplying the VectorField object as data
;

stid = create "streamlineplot" streamlinePlotClass wid
    "tiMainString" : "Modifying StreamlinePlot resources"
    "stVectorFieldData" : vfid
end create

draw(stid)
frame(wid)

; 
; Get the values of several resources that are set dynamically based
; on the assumed NDC size of a grid cell. Each of this will be separately
; modified in the course of this example to illustrate their effect.
;

getvalues stid
  "stStepSizeF" : stepsize
  "stArrowLengthF" : arrowlength
  "stMinLineSpacingF" : spacing
end getvalues

; 
; Increase the step size 
; 

setvalues stid
   "tiMainString" : "Larger Step Size"
   "stStepSizeF" : stepsize * 4.0
end setvalues

draw(stid)
frame(wid)

; 
; Decrease the step size 
; 

setvalues stid
   "tiMainString" : "Smaller Step Size"
   "stStepSizeF" : stepsize * 0.25
end setvalues

draw(stid)
frame(wid)

; 
; Increase the minimum line spacing 
; 

 
setvalues stid
   "tiMainString" : "Larger Minimum Line Spacing"
   "stMinLineSpacingF" : spacing * 4.0
   "stStepSizeF" : stepsize
end setvalues

draw(stid)
frame(wid)

; 
; Decrease the minimum line spacing
; 

setvalues stid
   "tiMainString" : "Smaller Minimum Line Spacing"
   "stMinLineSpacingF" : spacing * 0.25
end setvalues

draw(stid)
frame(wid)

; 
; Increase the line starting grid stride 
; 

setvalues stid
   "tiMainString" : "Larger Line Starting Grid Stride"
   "stMinLineSpacingF" : spacing
   "stLineStartStride" : 3
end setvalues

draw(stid)
frame(wid)


; 
; Decrease the line starting grid stride
; 

setvalues stid
   "tiMainString" : "Smaller Line Starting Grid Stride"
   "stLineStartStride" : 1
end setvalues

draw(stid)
frame(wid)

;
; Increase the arrow size
; 

setvalues stid
   "tiMainString" : "Larger Arrows"
   "stLineStartStride" : 2
   "stArrowLengthF" : arrowlength * 2.0
end setvalues

draw(stid)
frame(wid)

    
;
; Clean up
;

delete(appid)

end
