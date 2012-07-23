CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1995                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:         basic06f.f
C
C   Author:       Fred Clare
C                 National Center for Atmospheric Research
C                 PO 3000, Boulder, Colorado
C
C
C   Date:         Wed May 24 12:54:47 MDT 1995
C
C   Description:  This Fortran program demonstrates how to position 
C                 objects on an output device and how to change 
C                 their sizes.  A simple color table is also defined 
C                 and used for changing the color of a curve in an 
C                 XyPlot.  The script begins with two procedures - 
C                 one for drawing plot objects and one for drawing 
C                 text objects.
C
      program basic06
      implicit none

      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass
      external NhlFTickMarkClass
      external NhlFTextItemClass

      integer appid,rlist
      integer xwork_id,text_id,box_id,data_id
      integer dataspec
      CHARACTER*7  wks_type
      integer i,ierr

      character*5 text
      data text /'Box  '/

      real xdra(9),ydra(9)
      real xpos,ypos

      data xdra / 0.0, 0.1, 0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0 /
      data ydra / 0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0, 0.1, 0.5 /
C
C Define a simple color map (index 0 defines the background color).
C
      real cmap(3,4)
      data cmap / 1.0, 1.0, 1.0,
     1      0.0, 0.0, 1.0,
     2      0.0, 1.0, 0.0,
     3      1.0, 0.0, 0.0 /

      integer dims(2) 
      data dims / 3,4 /
C
C Define the workstation type
C
      wks_type = "x11"
C
C Initialize the high level utility library and create application.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'basic06',NhlFappClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create a meta file workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName',
     1         './basic06f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)
         call NhlFCreate(xwork_id,'simple',
     1         NhlFncgmWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then

C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)
         call NhlFCreate(xwork_id,'simple',
     1         NhlFCairoWindowWorkstationClass,
     1         0,rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkPSFileName','./basic06f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)

        call NhlFCreate(xwork_id,'simple',
     1        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
        call NhlFRLClear(rlist)

        call NhlFRLSetstring(rlist,'wkPDFFileName','./basic06f.pdf',
     1        ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)

        call NhlFCreate(xwork_id,'simple',
     1        NhlFPDFWorkstationClass,0,rlist,ierr)
      endif
      if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     &    wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName',
     &        './basic06f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)
         call NhlFCreate(xwork_id,'simple',
     &        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      endif
      if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName',
     &        './basic06f',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',
     1         cmap,2,dims,ierr)
         call NhlFCreate(xwork_id,'simple',
     &        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C
C Create data object for an XyPlot
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caXArray',xdra,
     1      9,ierr)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra,
     1      9,ierr)
      call NhlFCreate(data_id,'xyData',NhlFcoordArraysClass,
     1      0,rlist,ierr)
C
C Create a simple XyPlot object with no labels or borders.  The
C parent for this object is xwork_id, hence it will be sent to
C the workstation identified by xwork_id when the draw procedure
C is invoked on it.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'tmXBBorderOn','False',ierr)
      call NhlFRLSetString(rlist,'tmXTBorderOn','False',ierr)
      call NhlFRLSetString(rlist,'tmYLBorderOn','False',ierr)
      call NhlFRLSetString(rlist,'tmYRBorderOn','False',ierr)
      call NhlFRLSetString(rlist,'tmXBOn','False',ierr)
      call NhlFRLSetString(rlist,'tmXTOn','False',ierr)
      call NhlFRLSetString(rlist,'tmYLOn','False',ierr)
      call NhlFRLSetString(rlist,'tmYROn','False',ierr)
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFCreate(box_id,'Box',NhlFxyPlotClass,xwork_id,
     1    rlist,ierr)
C
C Create a TextItem object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',0.5,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',0.5,ierr)
      call NhlFRLSetInteger(rlist,'txFont',26,ierr)
      call NhlFCreate(text_id,'Text',NhlFtextItemClass,xwork_id,
     1      rlist,ierr)
C
C Add the data identified by data_id to the XyPlot.
C
      call NhlFAddData(box_id,'xyCoordData',data_id,dataspec)
C
C Draw three labeled boxes at different sizes and in different positions
C and with different colors.
C
      do 10 i=1,3
         xpos = -0.05*i*i + 0.5*i - 0.20
         ypos = 1.0-xpos
         text(5:5) = char(ichar('1')+i-1)
C
C Specify a text string and its color.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'txString',text,ierr)
         call NhlFRLSetInteger(rlist,'txFontColor',4-i,ierr)
         call NhlFSetValues(text_id,rlist,ierr)
C
C Set the XyPlot curve color.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'xyMonoLineColor','True',ierr)
         call NhlFRLSetInteger(rlist,'xyLineColor',i,ierr)
         call NhlFSetValues(dataspec,rlist,ierr)
C
C Draw box and text.
C
         call draw_plot(box_id, xpos, ypos, 0.36-0.09*(i-1),ierr)
         call draw_text(text_id, xpos, ypos, 0.08-0.02*(i-1),ierr)
 10   continue

      call NhlFFrame(xwork_id,ierr)

      call NhlFDestroy(xwork_id,ierr)
      call NhlFClose

      stop
      end

      subroutine draw_plot(id,x,y,scale,ierr)
C
C This procedure takes the plot object with identifier 'id' and 
C draws it centered at coordinate (x,y) and scaled by 'scale'.  
C The original plot object is returned unchanged.
C
      integer id
      real x,y,scale

      integer rlist,grlist,ierr
      real x_ref,y_ref,width_ref,height_ref

      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')

      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vpXF',x_ref,ierr)
      call NhlFRLGetFloat(grlist,'vpYF',y_ref,ierr)
      call NhlFRLGetFloat(grlist,'vpWidthF',width_ref,ierr)
      call NhlFRLGetFloat(grlist,'vpHeightF',height_ref,ierr)
      call NhlFGetValues(id,grlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',x - 0.5*width_ref*scale,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',y + 0.5*height_ref*scale,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',width_ref*scale,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',height_ref*scale,ierr)
      call NhlFSetValues(id,rlist,ierr)

      call NhlFDraw(id,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',x_ref,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',y_ref,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',width_ref,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',height_ref,ierr)
      call NhlFSetValues(id,rlist,ierr)

      return
      end

      subroutine draw_text(id,x,y,height,ierr)
C
C This procedure takes the text string in the object identified by 'id'
C and draws it centered at coordinate (x,y) with a height of 'height'.
C
      integer id
      real x,y,height

      integer rlist,grlist,ierr
      real xpos,ypos,fheight

      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')

      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'txPosXF',xpos,ierr)
      call NhlFRLGetFloat(grlist,'txPosYF',ypos,ierr)
      call NhlFRLGetFloat(grlist,'txFontHeightF',fheight,ierr)
      call NhlFGetValues(id,grlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',x,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',y,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',height,ierr)
      call NhlFSetValues(id,rlist,ierr)

      call NhlFDraw(id,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',xpos,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',ypos,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',fheight,ierr)
      call NhlFSetValues(id,rlist,ierr)

      return 
      end
