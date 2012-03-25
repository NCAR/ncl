;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc02n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description:   Given a simple mathematically generated data set,
;		   demonstrates line-drawn vector arrows and the use
;                  of some basic VectorPlot resources
;

begin

M=30
N=25
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
appid = create "vc02" appClass defaultapp
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
  wid = create "vc02Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc02n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "vc02Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc02Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./vc02n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc02Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./vc02n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc02Work" documentWorkstationClass defaultapp
    "wkFileName" : "./vc02n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc02Work" imageWorkstationClass defaultapp
    "wkFileName" : "./vc02n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a VectorField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vfield" vectorFieldClass appid
        "vfUDataArray" : U(y | :, x | :)
	"vfVDataArray" : V
end create

;
; Create a VectorPlot object, supplying the VectorField object as data
; Setting vcMonoLineArrowColor False causes VectorPlot to color the
; vector arrows individually, based, by default, on each vector's magnitude.
;

vcid = create "vectorplot" vectorPlotClass wid
    "vcVectorFieldData" : vfid
    "vcMonoLineArrowColor" : False
    "tiMainString" : "Line-Drawn Vectors (colored by magnitude)"
end create

draw(vcid)
frame(wid)

;
; All the vector arrows are scaled in length based on their magnitude
; relative to a reference magnitude (the maximum magnitude by default)
; Setting the arrow length of the reference magnitude, therefore, adjusts
; the length of all the vector arrows in the plot. In addition, you can
; specify a length for the smallest vector in the plot, as a fraction of
; the length of the reference vector. In this case, the remaining arrows
; are scaled based on both these lengths.
; Since the initial reference length is established dynamically, the 
; simplest way to adjust the overall arrow length is to 
; retrieve the initially set value of 'vcRefLengthF' and then multiply 
; by the desired factor.
;

getvalues vcid
  "vcRefLengthF" : reflen
end getvalues
reflen = reflen * 1.5    

setvalues vcid
   "tiMainString" : "Adjusting the Reference and Minimum Length"
   "vcRefLengthF" : reflen
   "vcMinFracLengthF" : 0.3
end setvalues

draw(vcid)
frame(wid)

; 
; Note that setting the reference magnitude also affects the length
; of the arrows. In this case it is an inverse relationship.
;

setvalues vcid
   "tiMainString" : "Adjusting the Reference Magnitude"
   "vcRefMagnitudeF" : 20.0
end setvalues

draw(vcid)
frame(wid)


;
; There are two vector annotations known as the reference vector 
; annotation and the minimum vector annotation. Their resources are prefixed
; respectively by "vcRefAnno" and "vcMinAnno". Only the reference vector
; annotation is displayed by default. Here, the minimum vector annotation
; is turned on, and some resources of each annotation are modified.
;

setvalues vcid
   "tiMainString" : "Modifying the Vector Annotations"
   "vcRefAnnoFontHeightF" : 0.015
   "vcRefAnnoString1" : "$VMG$ meters/sec"
   "vcRefAnnoString2On" : False
   "vcMinAnnoOn" : True
   "vcMinAnnoString1" : "$VMG$ meters/sec"
   "vcMinAnnoString2On" : False
end setvalues

draw(vcid)
frame(wid)

;
; Line-drawn arrowheads are sized proportionally to the arrow length 
; unless the resulting size would be outside the limits defined by 
; the arrowhead minimum and maximum size resources. Setting the 
; minimum and maximum sizes to the same value causes all the
;

setvalues vcid
   "tiMainString" : "Uniformly-sized Arrow Heads"
   "vcLineArrowHeadMinSizeF" : 0.01
   "vcLineArrowHeadMaxSizeF" : 0.01
end setvalues

draw(vcid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
