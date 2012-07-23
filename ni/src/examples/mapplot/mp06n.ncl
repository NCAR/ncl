;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1999                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp06n.ncl
;
;   Author:     Mary Haley
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 13 13:44:10 MST 1999
;
;   Description:  Shows how to draw county lines in the US.
;

begin
  appid = create "mp06" appClass defaultapp
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
      wid = create "mp06Work" ncgmWorkstationClass defaultapp
         "wkMetaName" : "./mp06n.ncgm"
      end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "mp06Work" windowWorkstationClass defaultapp
        "wkPause" : True
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "mp06Work" psWorkstationClass defaultapp
     "wkPSFileName" : "./mp06n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "mp06Work" pdfWorkstationClass defaultapp
     "wkPDFFileName" : "./mp06n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "mp06Work" documentWorkstationClass defaultapp
     "wkFileName" : "./mp06n"
     "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "mp06Work" imageWorkstationClass defaultapp
     "wkFileName" : "./mp06n"
     "wkFormat" : wks_type
    end create
  end if

  mapid = create "map"  mapPlotClass wid
    "vpWidthF"             : 0.90 ; Make map larger in view port.
    "vpHeightF"            : 0.90
    "vpXF"                 : 0.05
    "vpYF"                 : 0.95

    "mpOutlineBoundarySets": "AllBoundaries" ; Draw county lines.

    "mpMinLatF"            :   25.
    "mpMaxLatF"            :   50.
    "mpMinLonF"            : -130.
    "mpMaxLonF"            :  -60. 

    "tiMainString"         : ":F22:US with all counties outlined"
  end create

  draw(mapid)
  frame(wid)

;
; Draw all counties in the United States that have the name "Adams".
;
  setvalues mapid
    "mpOutlineBoundarySets" : "GeophysicalAndUSStates"
    "mpOutlineSpecifiers"   : "Adams"
    "tiMainString"          : ":F22:US with Adams counties outlined"
  end setvalues

  draw(mapid)
  frame(wid)

;
; List of Florida counties.
;
  florida_counties = \
             (/"Alachua", "Baker", "Bay", "Bradford", "Brevard", \
               "Broward", "Calhoun", "Charlotte", "Citrus", "Clay", \
               "Collier", "Columbia", "De Soto", "Dixie", "Duval", \
               "Escambia", "Flagler", "Franklin", "Gadsden", \
               "Gilchrist", "Glades", "Gulf", "Hamilton", "Hardee", \
               "Hendry", "Hernando", "Highlands", "Hillsborough", \
               "Holmes", "Indian River", "Jackson", "Jefferson", \
               "Keys", "Lafayette", "Lake", "Lee", "Leon", "Levy", \
               "Liberty", "Madison", "Manatee", "Marion", "Martin", \
               "Miami-Dade", "Monroe", "Nassau", "Okaloosa", \
               "Okeechobee", "Orange", "Osceola", "Palm Beach", \
               "Pasco", "Pinellas", "Polk", "Putnam", "Saint Johns", \
               "Saint Lucie", "Saint Vincent Island", "Santa Rosa", \
               "Sarasota", "Seminole", "Sumter", "Suwannee", "Taylor", \
               "Union", "Volusia", "Wakulla", "Walton", "Washington" /)

;
; By putting the string "Florida . " in front of each county name, only
; those counties in Florida will get drawn. Otherwise, if any of these
; counties existed in other states, those counties would get drawn as well.
;
  setvalues mapid
    "vpWidthF"             : 0.90 ; Resize map.
    "vpHeightF"            : 0.90
    "vpXF"                 : 0.05
    "vpYF"                 : 0.95

    "mpOutlineSpecifiers" : "Florida . " + florida_counties
    "mpMinLatF"            :  25.
    "mpMaxLatF"            :  32.
    "mpMinLonF"            : -90.
    "mpMaxLonF"            : -80. 

    "tiMainString"          : ":F22:Florida and its counties outlined"
  end setvalues

  draw(mapid)
  frame(wid)

end
