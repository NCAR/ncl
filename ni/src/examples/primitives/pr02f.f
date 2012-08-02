C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
C                                                                        ;
C;                Copyright (C)  1996                                    ;
C;        University Corporation for Atmospheric Research                ;
C;                All Rights Reserved                                    ;
C;                                                                       ;
C;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
C 
C    File:       pr02f.f
C 
C    Author: Lynn Hermanson
C            National Center for Atmospheric Research
C            PO 3000, Boulder, Colorado
C  
C    Date:       May 8, 1996
C 
C    Description:     Given an array of xaxis values and 
C                     an array of yaxis values, where each 
C                     x,y pair corresponds to one vertex or
C                     the location of one point, draw a
C                     square (a polyline), four points (asterisks)
C                     at the corners of a square (a polymarker), and
C                     a rectangular filled area (a polygon),on to
C                     a blank canvas (an empty default logLinPlot).

C                     Polylines, polymarkers, and polygons, are
C                     refered to as primitives.

C

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFMapPlotClass
      external NhlFVectorFieldClass
      external NhlFLogLinPlotClass
      external NhlFGraphicStyleClass

      integer appid,wid,cid,gsid_pl,gsid_pm, gsid_pg
      integer rlist, ierr
      parameter(N=5)
      parameter(M=4)
      parameter(K=5)
      real X(N),Y(N)
      real U(M),V(M)
      real PX(K),PY(K)
      character*7  wks_type
C
C Define the workstation type
C
      wks_type = "x11"

C
C These are the polyline points /*a square*/
C

      X(1) = .25
      X(2) = .25
      X(3) = .45
      X(4) = .45
      X(5) = .25
      Y(1) = .25
      Y(2) = .45
      Y(3) = .45
      Y(4) = .25
      Y(5) = .25

C
C These are the polymarker points /*four corner points*/
C
      U(1) = .35
      U(2) = .35
      U(3) = .55
      U(4) = .55
      V(1) = .35
      V(2) = .55
      V(3) = .55
      V(4) = .35

C
C These are the polygon points /*a filled rectangle*/ 
C
      PX(1) = .6
      PX(2) = .7
      PX(3) = .7
      PX(4) = .8
      PX(5) = .6
      PY(1) = .2
      PY(2) = .8
      PY(3) = .8
      PY(4) = .2
      PY(5) = .2

C
C  Initialize the high level utility library
C   
      call NhlFInitialize()

C Next,
C create an application context. Set the app dir to the current directory
C so the application looks for a resource file in the working directory.
C ***See resource file pr02.res***  
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'pr02',NhlFappClass,
     1     0,rlist,ierr)

C  Choose to display output to an X11 workstation.(set above)*/

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C  Create a meta file workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkMetaName','./pr02f.ncgm',ierr)
      call NhlFCreate(wid,'pr02Work',NhlFncgmWorkstationClass,
     1    appid,rlist,ierr)
    
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C  Create an X workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'wkPause','true',ierr)
      call NhlFCreate(wid,'pr02Work',
     1            NhlFCairoWindowWorkstationClass,
     1            appid,rlist,ierr)
    

      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C  Create an older-style PS workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPSFileName','./pr02f.ps',ierr)
      call NhlFCreate(wid,'pr02Work',NhlFpsWorkstationClass,
     1    appid,rlist,ierr)

      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C  Create an older-style PDF workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkPDFFileName','./pr02f.pdf',ierr)
      call NhlFCreate(wid,'pr02Work',NhlFpdfWorkstationClass,
     1    appid,rlist,ierr)

      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C  Create a cairo PS/PDF workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFRLSetString(rlist,'wkFileName','./pr02f',ierr)
      call NhlFCreate(wid,'pr02Work',
     1    NhlFCairoPSpdfWorkstationClass,appid,rlist,ierr)

      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C  Create a cairo PNG workstation.
C   
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
      call NhlFRLSetString(rlist,'wkFileName','./pr02f',ierr)
      call NhlFCreate(wid,'pr02Work',
     1    NhlFCairoImageWorkstationClass,appid,rlist,ierr)

      endif    

C  Create a blank plot object to draw the primitives on.*/

      call NhlFRLClear(rlist)
      call NhlFCreate(cid,'canvas',NhlFlogLinPlotClass,
     1    wid,rlist,ierr)

C  Create a graphicPlot object consisting of a thick blue square, large
C  red filled circular corner points,and a dashed red triangle
C  filled with green stripes.

C  The graphicStyle object is NOT a stand-alone plot object. There
C    must exist some kind of plot object which has already been drawn
C    to place the primitives on.

C make polyline */

      call NhlFRLClear(rlist)

C **** See resource file pr02.res ****
C       call NhlFRLSetInteger(rlist,'gsLineColor', 4,ierr)
C       call NhlFRLSetInteger(rlist,'gsLineThicknessF', 3,ierr)

      call NhlFCreate(gsid_pl,'pr02polyline',NhlFgraphicStyleClass,
     1    wid,rlist,ierr)

C make polymarker */

      call NhlFRLClear(rlist)

C **** See resource file pr02.res ****
C       call NhlFRLSetInteger(rlist,'gsMarkerIndex', 16,ierr)
C       call NhlFRLSetFloat(rlist,'gsMarkerSizeF', .02,ierr)
C       call NhlFRLSetInteger(rlist,'gsMarkerColor', 2,ierr)

      call NhlFCreate(gsid_pm,'pr02polymarker',NhlFgraphicStyleClass,
     1    wid,rlist,ierr)

C make polygon */

      call NhlFRLClear(rlist)

C **** See resource file pr02.res ****
C       call  NhlFRLSetInteger(rlist,'gsFillIndex', 1,ierr)
C       call  NhlFRLSetInteger(rlist,'gsFillColor', 3,ierr)
C       call  NhlFRLSetInteger(rlist,'gsEdgesOn', 'true',ierr)
C       call  NhlFRLSetInteger(rlist,'gsEdgeColor', 2,ierr)
C       call  NhlFRLSetInteger(rlist,'gsEdgeThicknessF', 3,ierr)
C       call  NhlFRLSetInteger(rlist,'gsEdgeDashPattern', 1,ierr)

      call NhlFCreate(gsid_pg,'pr02polygon',NhlFgraphicStyleClass,
     1    wid,rlist,ierr)


C  Draw the shapes onto the blank logLinPlot object.
C  Signify end of frame.*/

      call NhlFDraw(cid,ierr)

C The last paramenter is the number of points required*/

      call NhlFNDCPolyline(cid, gsid_pl, X, Y,5,ierr)
      call NhlFNDCPolymarker(cid, gsid_pm, U, V,4,ierr)
      call NhlFNDCPolygon(cid, gsid_pg, PX, PY,5,ierr)
      call NhlFFrame(wid,ierr)

C  Destroy all of the objects created, close the HLU library and exit.*/

      call NhlFDestroy(cid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose  

      stop
      end


