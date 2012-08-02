;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       mp04n.ncl
;
;   Author:     David Brown
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Translated to NCL by Mary Haley
;
;   Date:       Fri Sep 29 07:37:18 MDT 1995
;
;   Description:    Illustrates use of Annotation objects.
;

begin

;
; Define enough frames for a fairly smooth animation.
;
    FRAME_COUNT=36

    anno_names = (/"Los Angeles","Seattle","Toronto","New York","Miami",\
"Mexico City","London","Jakarta","Moscow","New Delhi","Rio de Janeiro",\
"Cairo","Buenos Aires","Beijing","Tokyo","Lagos","Nairobi","Sydney","Bogota",\
"Lima","Cape Town","Calcutta","Shanghai","Bombay","Denver"/)

    anno_lat = (/34.0,47.6,43.7,40.67,25.75,19.417,51.32,-6.13,55.75,28.37,\
-22.883,30.05, -34.67,39.917,35.67,6.45,-1.283,-33.9167,4.633,-12.1,-33.933,\
22.583,31.217,18.93,39.716/)

    anno_lon = (/-118.28,-122.33,-79.4167,-73.83,-80.25,-99.167,-0.1,106.75,\
37.7,77.217,-43.283,31.25,-58.4167,116.4167,139.67,3.28,36.833,151.167,\
-74.083,-77.05,18.4667,88.35,121.4167,72.85,-105.017/)
    
;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
;
    appid = create "mp04" appClass defaultapp
        "appUsrDir" : "./"
        "appDefaultParent" : True
    end create
;
; Default is to display output to an NCGM
;
    wks_type = "ncgm"

    if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
        wid = create "mp04Work" ncgmWorkstationClass defaultapp
           "wkMetaName" : "./mp04n.ncgm"
        end create
    end if
    if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
      wid = create "mp04Work" windowWorkstationClass defaultapp
          "wkPause" : True
      end create
    end if
    if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
      wid = create "mp04Work" psWorkstationClass defaultapp
       "wkPSFileName" : "./mp04n.ps"
      end create
    end if
    if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
      wid = create "mp04Work" pdfWorkstationClass defaultapp
       "wkPDFFileName" : "./mp04n.pdf"
      end create
    end if
    if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
      wid = create "mp04Work" documentWorkstationClass defaultapp
       "wkFileName" : "./mp04n"
       "wkFormat" : wks_type
      end create
    end if
    if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
      wid = create "mp04Work" imageWorkstationClass defaultapp
       "wkFileName" : "./mp04n"
       "wkFormat" : wks_type
      end create
    end if
;
; AnnoManager objects allow the PlotManager to manipulate any View class
; object as an annotation a uniform fashion. They allow
; the user to set the View object's size and location relative to
; the viewport of a Plot. They may be located relative to one
; of the viewport sides, or, as in this example, aligned with the plot's 
; data space (amTrackData is set True in the resource file).
;
; Create a TextItem for each place name to be included on the map.
; Collect the object ids into an array.
;
    anno_len = dimsizes(anno_names)
    text_ids = new(anno_len,graphic)

    do i = 0,anno_len-1
      text_ids(i) = create anno_names(i) textItemClass wid
        "txString" : anno_names(i)
      end create
    end do
;
; Since the MapPlot object is by default a PlotManager, you can
; make each TextItem View object into an annotation simply by setting the 
; pmAnnoViews resource with the array of TextItem ids. 
;
    mapid = create "Map0" mapPlotClass wid
        "pmAnnoViews" : text_ids
    end create
;
; Retrieve the ids of the AnnoManager objects created by the PlotManager and
; then set their location in data coordinate space. The AnnoManager objects
; are arranged in the same order as the TextItems in the pmAnnoViews
; resource.
;
    getvalues mapid
      "pmAnnoManagers": am_ids
    end getvalues
    num_am_ids = dimsizes(am_ids)

    do i=0,num_am_ids-1
      setvalues am_ids(i)
        "amDataXF" : anno_lon(i)
        "amDataYF" : anno_lat(i)
      end setvalues
    end do
;
; Create FRAME_COUNT plots, varying the center longitude by an equal
; amount each time.
;
    do i = FRAME_COUNT,1,1
      setvalues mapid
        "mpCenterLonF":  i * 360./FRAME_COUNT
      end setvalues
      draw(mapid)
      frame(wid)
    end do
;
; Destroy the objects created, close the HLU library and exit.
;
    delete(wid)
    delete(appid)
end
