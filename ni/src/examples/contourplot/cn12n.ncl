;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn12n.ncl
;
;   Author:     Mary Haley
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Wed Oct 11 16:18:15 MDT 1995
;
;   Description:  This example emulates LLU example "cpex08".  It
;                 draws a filled map with filled contours appearing in
;                 Africa. In order to mask Africa from the map fill, we
;                 use the mpMaskAreaSpecifiers resource and mask all of
;                 the countries in Africa.
;

begin
  N = 40
  len_dims = (/N,N/)
;
; These are the geographical areas we want to fill.
;
  fill_specs = (/"water","land"/)
;
; These are the geographical areas we want to mask.
;
  mask_specs = (/\
  "algeria","angola","angola-exclave-called-cabinda","benin","botswana",\
  "burundi","cameroon","central-african-republic","chad","congo","djibouti",\
  "egypt","equatorial-guinea","ethiopia","gabon","gambia","ghana","guinea",\
  "guinea-bissau","ivory-coast","kenya","lesotho","liberia","libya",\
  "madagascar","malawi","mali","mauritania","mauritius","morocco",\
  "mozambique","namibia","niger","nigeria","rwanda","senegal","sierra-leone",\
  "somalia","south-africa","sudan","swaziland","tanzania","togo","tunisia",\
  "uganda","upper-volta","western-sahara","zaire","zambia","zimbabwe"/)
;
; Create an application object.
;
  appid = create "cn12" appClass defaultapp
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
  wid = create "cn12Work" ncgmWorkstationClass defaultapp
     "wkMetaName" : "./cn12n.ncgm"
  end create
end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "cn12Work" windowWorkstationClass defaultapp
     "wkPause" : True
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "cn12Work" psWorkstationClass defaultapp
      "wkPSFileName" : "./cn12n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "cn12Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "./cn12n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "cn12Work" documentWorkstationClass defaultapp
      "wkFileName" : "./cn12n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "cn12Work" imageWorkstationClass defaultapp
      "wkFileName" : "./cn12n"
      "wkFormat" : wks_type
    end create
  end if
;
; Read the input data and create a data object.
;
  z = asciiread("cn12n.asc",len_dims,"float")

  field1 = create "DataPlot" scalarFieldClass appid
    "sfDataArray" : z(:,:)
    "sfXCStartV" : -18.
    "sfXCEndV": 52.
    "sfYCStartV" : -35.
    "sfYCEndV": 38.
  end create
;
; Create a ContourPlot object using the above data field.
;
  conid = create "con1" contourPlotClass wid
    "cnScalarFieldData":      field1
  end create
;
; Create a map object, specifying the areas we want filled and masked.
; Also draw a LabelBar.
;
  mapid = create "map" mapPlotClass wid 
    "vpXF":                   .1
    "mpMaskAreaSpecifiers":   mask_specs
    "mpFillAreaSpecifiers":   fill_specs
    "pmLabelBarDisplayMode":  "always"
  end create
  overlay(mapid,conid)
  draw(mapid)
  frame(wid)
;
; Destroy the workstation object and exit.
;
  delete(wid)
end
