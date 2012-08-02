;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1993                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       vc01n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr  3 17:00:55 MST 1996
;
;   Description:   Basic VectorPlot example
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
appid = create "vc01" appClass defaultapp
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
  wid = create "vc01Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./vc01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wid = create "vc01Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wid = create "vc01Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./vc01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wid = create "vc01Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./vc01n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  wid = create "vc01Work" documentWorkstationClass defaultapp
    "wkFileName" : "./vc01n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  wid = create "vc01Work" imageWorkstationClass defaultapp
    "wkFileName" : "./vc01n"
    "wkFormat" : wks_type
  end create
end if

;
; Create a VectorField data object using the data set defined above.
; By default the array bounds will define the data boundaries (zero-based,
; as in C language conventions)
;

vfid = create "vectorfield" vectorFieldClass appid
        "vfUDataArray" : U(y | :, x |:) 	; transpose of U
	"vfVDataArray" : V
end create

;
; Create a VectorPlot object, supplying the VectorField object as data.
;

vcid = create "vectorplot" vectorPlotClass wid
    "tiMainString" : "Basic VectorPlot Example"
    "vcVectorFieldData" : vfid
end create

draw(vcid)
frame(wid)
    
;
; Clean up
;

delete(appid)

end
