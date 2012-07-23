CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       C
C                Copyright (C)  1995                                    C
C        University Corporation for Atmospheric Research                C
C                All Rights Reserved                                    C
C                                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File:         basic08f.f
C
C   Author:       David Brown (translated to Fortran by David Younghans)
C                 National Center for Atmospheric Research
C                 Boulder, Colorado  80303
C
C
C   Date:         Wed August 23, 1995
C
C   Description:  This Fortran program demonstrates how to draw
C                 a plot object including any annotations
C                 outside its viewport within a predefined 
C                 bounded area of NDC space. The subroutine 
C                 drbdplt is intended to be useful in any
C                 context where it is desired to keep an entire plot
C                 within predetermined boundaries.
C
C
      program basic08f
      implicit none
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFLogLinPlotClass
      external NhlFContourPlotClass
      
      integer appid,rlist,grlist
      integer wid,gid,ll_id,cn_id
      character*7  wks_type
      integer ierr

      real x(5),y(5)
C
C Define the workstation type
C
      wks_type = "x11"
C
C Initialize the high level utility library and create application.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLCreate(grlist,'GETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'basic08',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create a metafile workstation with the default colormap.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName',
     &        './basic08f.ncgm',ierr)
         call NhlFCreate(wid,'simple',
     &        NhlfNcgmWorkstationClass,0,rlist,ierr)
      endif

      if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.         
C      
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'simple',
     +        NhlFCairoWindowWorkstationClass,
     &        0,rlist,ierr)
      endif

      if (wks_type.eq."oldps".or.wks_type.eq."oldPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName',
     &        './basic08f.ps',ierr)
         call NhlFCreate(wid,'simple',NhlFPSWorkstationClass,
     &        0,rlist,ierr)
      endif
      if (wks_type.eq."oldpdf".or.wks_type.eq."oldPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName',
     &        './basic08f.pdf',ierr)
         call NhlFCreate(wid,'simple_pdf',NhlFPDFWorkstationClass,
     &        0,rlist,ierr)
      endif
      if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     &    wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./basic08c',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'simple_cairo',
     +                   NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      end if
      if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFileName','./basic08c',ierr)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFCreate(wid,'simple_cairo',
     +                   NhlFcairoImageWorkstationClass,0,rlist,ierr)
      end if
C
C Create a LogLinPlot with a viewport that fills the viewspace. This will
C be used for drawing immediate mode polylines indicating the intended
C boundary of each plot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFCreate(ll_id,'loglin',NhlFLogLinPlotClass,
     &     wid,rlist,ierr)
C
C Set GraphicStyle resources to modify the immediate mode line
C attributes.
C
      call NhlFRLClear(grlist)
      call NhlFRLGetInteger(grlist,'wkDefGraphicStyleId',gid,ierr)
      call NhlFGetValues(wid,grlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'gsLineColor',2,ierr)
      call NhlFRLSetInteger(rlist,'gsLineDashPattern',1,ierr)
      call NhlFSetValues(gid,rlist,ierr)
C
C Create an empty ContourPlot with a Title, a LableBar, and a Legend.
C Note that the viewport is square and covers the whole NDC space,
C meaning that if the plot were drawn as created, all annotations exterior
C to the viewport would be outside the viewspace and therefore clipped.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'pmLabelBarDisplayMode',
     &     'always',ierr)
      call NhlFRLSetString(rlist,'pmLegendDisplayMode',
     &     'always',ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     &     'bounded contour plot',ierr)
      call NhlFRLSetFloat(rlist,'vpXF',0.0,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFCreate(cn_id,'contour',NhlFContourPlotClass,
     &     wid,rlist,ierr)
C
C The first frame illustrates drawing the plot with a 5% margin around
C the viewable area. Draw an immediate mode line indicating the boundary
C that defines the margin.
C
      x(1) = 0.05
      x(2) = 0.95
      x(3) = 0.95
      x(4) = 0.05
      x(5) = 0.05
      
      y(1) = 0.05
      y(2) = 0.05
      y(3) = 0.95
      y(4) = 0.95
      y(5) = 0.05

      call NhlFNDCPolyline(ll_id,gid,x,y,5,ierr)
C
C Draw the plot with the desired boundary parameters.
C
      call drbdplt(cn_id,.true.,0.05,0.95,0.05,0.95,rlist)
      call NhlFFrame(wid,ierr)
C
C The second frame illustrates use of the drbdplt subroutine
C to place several plots with varying aspect ratios in a single frame.
C
      x(1) = 0.025
      x(2) = 0.475
      x(3) = 0.475
      x(4) = 0.025
      x(5) = 0.025

      y(1) = 0.525
      y(2) = 0.525
      y(3) = 0.975
      y(4) = 0.975
      y(5) = 0.525

      call NhlFNDCPolyline(ll_id,gid,x,y,5,ierr)
C
C Set the ContourPlot viewport so that the width is twice the height.
C (The absolute numbers are not important here, only the ratio matters.)
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.6,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',0.3,ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     &     'width is limiting dimension',ierr)
      call NhlFsetValues(cn_id,rlist,ierr)
C
C Draw the plot with the desired boundary parameters.
C
      call drbdplt(cn_id,.true., 0.025, 0.475, 0.525, 0.975,
     &     rlist)
C
C Draw an immediate mode line indicating the desired boundary of the
C second plot.
C      
      x(1) = 0.525
      x(2) = 0.975
      x(3) = 0.975
      x(4) = 0.525
      x(5) = 0.525

      y(1) = 0.525
      y(2) = 0.525
      y(3) = 0.975
      y(4) = 0.975
      y(5) = 0.525

      call NhlFNDCPolyline(ll_id,gid,x,y,5,ierr)
C
C Set the ContourPlot viewport so that the height is twice the width.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpWidthF',0.3,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',0.6,ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     &     'height is limiting dimension',ierr)
      call NhlFSetValues(cn_id,rlist,ierr)
C
C Draw the plot with the desired boundary parameters.
C
      call drbdplt(cn_id,.true., 0.525, 0.975, 0.525, 0.975,
     &     rlist)
C
C Draw an immediate mode line indicating the desired boundary of the
C third plot.
C
      x(1) = 0.125
      x(2) = 0.875
      x(3) = 0.875
      x(4) = 0.125
      x(5) = 0.125

      y(1) = 0.1
      y(2) = 0.1
      y(3) = 0.4
      y(4) = 0.4
      y(5) = 0.1

      call NhlFNDCPolyline(ll_id,gid,x,y,5,ierr)
C
C For this plot the aspect ratio is distorted in order to fill as much as
C possible of the desired area. Note that the space is not completely filled.
C This is because a number of factors affecting the final aspect ratio,
C such as the text size used for titles, are determined based on only
C one of the viewport's dimensions.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpHeightF',1.0,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',1.0,ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     &     'distort aspect ratio to fill area',ierr)
      call NhlFSetValues(cn_id,rlist,ierr)
C
C Draw the plot with the desired boundary parameters.
C
      call drbdplt(cn_id,.false., 0.125, 0.875, 0.1, 0.4,
     &     rlist)
      call NhlFFrame(wid,ierr)
C
C Clean up
C
      call NhlFDestroy(wid,ierr)
      call NhlFClose

      stop
      
      end
C
C
C
      subroutine drbdplt(id,keep_aspect,left,right,
     &     bottom,top,rlist)
C
C This subroutine takes the plot object with identifier "id" and
C draws it within the NDC boundaries represented by left, right,
C top, and bottom. If keep_aspect is .true, the aspect ratio of the plot
C is preserved: the plot fills the extent of the limiting dimension and
C is centered within the extent of the other dimension. If keep_aspect is
C .false., the aspect ratio is distorted in order to fill as much of the
C space as possible, given certain limitations in the ability of some
C HLU object to distort themselves to any arbitrary aspect ratio.
C

      implicit none

      integer id
      logical keep_aspect
      real left,right,bottom,top

      integer grlist,rlist,ierr
      real x_save,y_save,width_save,height_save,
     &     bb_top,bb_bottom,bb_left,bb_right,bb_height,bb_width,
     &     frame_height,frame_width,
     &     factor,
     &     x_off,y_off,x,y,width,height

      call NhlFRLCreate(grlist,'GETRL')
      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vpXF',x_save,ierr)
      call NhlFRLGetFloat(grlist,'vpYF',y_save,ierr)
      call NhlFRLGetFloat(grlist,'vpWidthF',width_save,ierr)
      call NhlFRLGetFloat(grlist,'vpHeightF',height_save,ierr)
      call NhlFGetValues(id,grlist,ierr)

      call NhlFGetBB(id,bb_top,bb_bottom,bb_left,bb_right,ierr)

      bb_height = bb_top - bb_bottom
      bb_width = bb_right - bb_left
      frame_height = top - bottom
      frame_width = right - left
      x = x_save
      y = y_save
      height = height_save
      width = width_save

      if (.not. keep_aspect) then
         factor = frame_width / bb_width
         width = width * factor
         x_off = (x - bb_left) * factor
         x = left + x_off
         factor = frame_height / bb_height
         height = height * factor
         y_off = (y - bb_top) * factor
         y = top + y_off

         call NhlFRLClear(rlist)
         call NhlFRLSetFloat(rlist,'vpXF',x,ierr)
         call NhlFRLSetFloat(rlist,'vpYF',y,ierr)
         call NhlFRLSetFloat(rlist,'vpWidthF',width,ierr)
         call NhlFRLSetFloat(rlist,'vpHeightF',height,ierr)
         call NhlFSetValues(id,rlist,ierr)

         call NhlFRLClear(grlist)
         call NhlFRLGetFloat(grlist,'vpXF',x,ierr)
         call NhlFRLGetFloat(grlist,'vpYF',y,ierr)
         call NhlFRLGetFloat(grlist,'vpWidthF',width,ierr)
         call NhlFRLGetFloat(grlist,'vpHeightF',height,ierr)
         call NhlFGetValues(id,grlist,ierr)

         call NhlFGetBB(id,bb_top,bb_bottom,bb_left,bb_right,ierr)

         bb_height = bb_top - bb_bottom
         bb_width = bb_right - bb_left

      endif

      if (bb_height / bb_width .lt. frame_height / frame_width) then
C
C Width is the limiting dimension.
C
         factor = frame_width / bb_width
         width = width * factor
         height = height * factor
         x_off = (x - bb_left) * factor
         y_off = (y - bb_top) * factor
         x = left + x_off
         bb_height = bb_height * factor
         y = top + y_off - 0.5 * (frame_height - bb_height)

      else
C
C Height is the limiting dimension.
C
         factor = frame_height / bb_height
         height = height * factor
         width = width * factor
         x_off = (x - bb_left) * factor
         y_off = (y - bb_top) * factor
         bb_width = bb_width * factor
         x = left + x_off + 0.5 * (frame_width - bb_width)
         y = top + y_off
      endif
         
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',x,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',y,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',width,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',height,ierr)
      call NhlFSetValues(id,rlist,ierr)

      call NhlFDraw(id,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpXF',x_save,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',y_save,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',width_save,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',height_save,ierr)
      call NhlFSetValues(id,rlist,ierr)

      return
      
      end



