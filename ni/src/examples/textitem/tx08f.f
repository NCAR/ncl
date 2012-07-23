CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                all rights reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tx08f.f
C
C      Author:         Jeff Boote
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jun  9 11:57:32 MDT 1995
C
C      Description:    Simple annotation example.
C
      integer list

      external NhlFAppClass
      external NhlFCairoWindowWorkstationClass
      external NhlFNCGMWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFXyPlotClass
      external NhlFTextItemClass
      character*7  wks_type
C
C Default is output to an X11 window.
C
      wks_type = "x11"
C
C Create application.
C
      call NhlFOpen
      call NhlFRLCreate(list,'setrl')
      call NhlFRLClear(list)
      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLSetString(list,'wkMetaName','./tx08f.ncgm',ierr)
         call NhlFCreate(ixwk,'tx08Work',NhlFNcgmWorkstationClass,0,
     1        list,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X workstation.
C
         call NhlFCreate(ixwk,'tx08Work',
     +        NhlFCairoWindowWorkstationClass,
     +        0,list,ier)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PS workstation.
C
         call NhlFRLSetString(list,'wkPSFileName','./tx08f.ps',ierr)
         call NhlFCreate(ixwk,'tx08Work',NhlFPSWorkstationClass,0,
     1        list,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLSetString(list,'wkPDFFileName','./tx08f.pdf',ierr)
         call NhlFCreate(ixwk,'tx08Work',NhlFPDFWorkstationClass,0,
     1        list,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF workstation.
C
         call NhlFRLSetString(list,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(list,'wkFileName','./tx08f',ierr)
         call NhlFCreate(ixwk,'tx08Work',
     1        NhlFCairoPSPDFWorkstationClass,0,list,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG workstation.
C
         call NhlFRLSetString(list,'wkFormat',wks_type,ierr)
         call NhlFRLSetString(list,'wkFileName','./tx08f',ierr)
         call NhlFCreate(ixwk,'tx08Work',
     1        NhlFCairoImageWorkstationClass,0,list,ierr)
      endif
C
C Create Plot object - no data, just illustration annotations.
C
      call NhlFRLClear(list)
      call NhlFRLSetFloat(list,'vpXF',0.2,ier)
      call NhlFRLSetFloat(list,'vpYF',.7,ier)
      call NhlFRLSetFloat(list,'vpWidthF',.3,ier)
      call NhlFRLSetFloat(list,'vpHeightF',.3,ier)
      call NhlFCreate(ixyplot,'xyPlot',NhlFXyPlotClass,ixwk,
     +  list,ier)

      call NhlFRLClear(list)
      call NhlFRLSetString(list,'txString','Second Line',ier)
      call NhlFCreate(itx,'txItem',NhlFTextItemClass,ixwk,
     +  list,ier)
C
C There is currently a bug in the HLU library - you have to specify
C ier - it should be returned in ianno if there is an error, but
C this is what you currently have to use.
C
      call NhlFAddAnnotation(ixyplot,itx,ianno,ier)
C
C Just set the "zone" to something fairly large, so it is outside
C of all "PlotManager" defined annotations.
C Set the "side" to top - 'amJust' defaults to centercenter - but
C setting 'amSide' to top makes that effectively bottomcenter.  So,
C the textitem would be drawn so its bottomcenter is placed at the
C top of the viewport of the plot, on the left side.  To get the
C text centered over the plot - set 'amParallelPosF' to .5 to move
C the textitem over .5 the width of the plot.
C Set 'amOrthogonalPosF' to .1 to give a little bit of spacing
C in the y direction from the plot (the previous zone).
C
      call NhlFRLClear(list)
      call NhlFRLSetInteger(list,'amZone',10,ier)
      call NhlFRLSetString(list,'amSide','top',ier)
      call NhlFRLSetString(list,'amJust','bottomcenter',ier)
      call NhlFRLSetFloat(list,'amParallelPosF',0.5,ier)
      call NhlFRLSetFloat(list,'amOrthogonalPosF',0.1,ier)
      call NhlFSetValues(ianno,list,ier)

      call NhlFRLClear(list)
      call NhlFRLSetString(list,'txString','First Line',ier)
      call NhlFCreate(itx,'txItem',NhlFTextItemClass,ixwk,
     +  list,ier)

      call NhlFAddAnnotation(ixyplot,itx,ianno,ier)
C
C Add this textitem as an annotation with the same charactoristics
C as the first one, but make the zone one higher - so it is just
C outside of the first annotation.  (With a .1 distance away due
C to the 'amOrthogonalPosF'.
C
      call NhlFRLClear(list)
      call NhlFRLSetInteger(list,'amZone',11,ier)
      call NhlFRLSetString(list,'amSide','top',ier)
      call NhlFRLSetString(list,'amJust','bottomcenter',ier)
      call NhlFRLSetFloat(list,'amParallelPosF',0.5,ier)
      call NhlFRLSetFloat(list,'amOrthogonalPosF',0.1,ier)
      call NhlFSetValues(ianno,list,ier)
C
C Draw and advance frame.
C   Notice that drawing the main plot automatically draw's the textitem
C   since it is now a "member plot" of the xyplot.  In fact, you can
C   no-longer draw the textitem indepentently.
C
      call NhlFDraw(ixyplot,ier)
      call NhlFFrame(ixwk,ier)
C
C Now, if we move the base plot, the annoation stays with the plot.
C It is drawn in its relative position to the xyplot.
C
      call NhlFRLClear(list)
      call NhlFRLSetFloat(list,'vpXF',0.5,ier)
      call NhlFRLSetFloat(list,'vpYF',.4,ier)
      call NhlFSetValues(ixyplot,list,ier)

      call NhlFDraw(ixyplot,ier)
      call NhlFFrame(ixwk,ier)
C
C Close automatically destroys all hlu objects that currently exist,
C and destroys all "res-list's" that currently exist as well.
C It is really only important to explicitly destroy those things
C for long running applications, so memory use doesn't grow too
C large.
C
      call NhlFClose
      stop
      end
