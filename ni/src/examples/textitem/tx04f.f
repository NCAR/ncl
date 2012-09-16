CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                            Copyright (C)  1995                       C
C                 University Corporation for Atmospheric Research      C
C                            All Rights Reserved                       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:            tx04f.f
C
C      Author:          Jeff Boote (converted by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:28:20 MST 1995
C
C      Description:     Demonstrates TextItem object.
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCairoPSPDFWorkstationClass
      external NhlFCairoImageWorkstationClass
      external NhlFCairoWindowWorkstationClass
      external NhlFMapPlotClass
      external NhlFTextItemClass
      
      integer i,appid,text_item_id,wid,rlist,ierr
      character*7  wks_type
C
C Default output is to an X workstation.
C
      wks_type = "x11"
C     
C  Initialize the high level utility library.
C     
      call NhlFInitialize
C   
C Create an application context.  Set the app dir to the current
C directory so the application looks for the resource file in the
C execution directory.
C   
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid, 'tx04',NhlFAppClass,0,rlist,ierr)

      if (wks_type.eq."ncgm".or.wks_type.eq."NCGM") then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./tx04f.ncgm',ierr)
         call NhlFCreate(wid,'tx04Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."x11".or.wks_type.eq."X11") then
C
C Create an X Workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'tx04Work',
     1        NhlFCairoWindowWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldps".or.wks_type.eq."OLDPS") then
C
C Create an older-style PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./tx04f.ps',ierr)
         call NhlFCreate(wid,'tx04Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."oldpdf".or.wks_type.eq."OLDPDF") then
C
C Create an older-style PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./tx04f.pdf',ierr)
         call NhlFCreate(wid,'tx04Work',NhlFPDFWorkstationClass,0,
     1        rlist,ierr)
      else if (wks_type.eq."pdf".or.wks_type.eq."PDF".or.
     +         wks_type.eq."ps".or.wks_type.eq."PS") then
C
C Create a cairo PS/PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./tx04f',ierr)
         call NhlFCreate(wid,'tx04Work',
     1        NhlFCairoPSPDFWorkstationClass,0,rlist,ierr)
      else if (wks_type.eq."png".or.wks_type.eq."PNG") then
C
C Create a cairo PNG object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkFormat',wks_type,ierr)
         call NhlFRLSetstring(rlist,'wkFileName','./tx04f',ierr)
         call NhlFCreate(wid,'tx04Work',
     1        NhlFCairoImageWorkstationClass,0,rlist,ierr)
      endif
C  
C This is the only creation of a text object for this entire program.
C  
      call NhlFRLClear(rlist)
      call NhlFRLSetinteger(rlist,'txJust',4,ierr)
      call NhlFRLSetinteger(rlist,'txFont',25,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.03,ierr)
      call NhlFRLSetstring(rlist,'txString',
     1     'TextItem - VARIOUS CAPABILITIES',ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95,ierr)
      call NhlFCreate(text_item_id,'Nhl05TextItem',
     1     NhlFTextItemClass,wid,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.015,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.7,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.86,ierr)
      call NhlFRLSetstring(rlist,'txString',
     1     'HIGH-QUALITY CHARACTERS USED BELOW',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.7,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.5,ierr)  
      call NhlFRLSetstring(rlist,'txString',
     1     'MEDIUM-QUALITY CHARACTERS USED BELOW',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.58,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.81,ierr)   
      call NhlFRLSetstring(rlist,'txString','txString',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosYF',.46,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.82,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.81,ierr)        
      call NhlFRLSetstring(rlist,'txString','RESULT',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosYF',.46,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.82,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.78,ierr)
      call NhlFRLSetstring(rlist,'txString','------',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosYF',.43,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.58,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.78,ierr)
      call NhlFRLSetstring(rlist,'txString','------------',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosYF',.43,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
C  
C  Below are examples using the function control characters. In each
C  iteration of the loop a new string is set using call NhlFSetValues.
C  In order to demonstrate what the value of 'txString' is, the
C  function code is temporarily changed causing the entire contents of
C  'txString' to be drawn.
C
      do 10 i=0,11
         if (i .eq. 0) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','~L~A',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 1) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','~IGL~A',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 2) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString',
     1           'A~S~2~N~+B~S~2~N~',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 3) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','A~S~B',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 4) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','A~SPU~B',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 5) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','~GIU~+',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 6) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','~1045~',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 7) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString','10~S~10~S~100',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 8) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString',
     1           'X~B1~2~S1~3',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 9) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString',
     1           'X~B1~2~S~3~N~Y~S~2',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 10) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString',
     1           'X~S~A~B~1~NN~ABC',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         else if (i .eq. 11) then
            call NhlFRLClear(rlist)
            call NhlFRLSetstring(rlist,'txString',
     1           '1.3648~L1~410~S~-13',ierr)
            call NhlFSetValues(text_item_id,rlist,ierr)
         end if
C  
C  Change function code so full string will be drawn.
C  
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txFuncCode','$',ierr)
         call NhlFRLSetfloat(rlist,'txFontHeightF',.01,ierr)
         call NhlFRLSetfloat(rlist,'txPosXF',.58,ierr)
         call NhlFRLSetfloat(rlist,'txPosYF',.78-(i+1)*.02,ierr)
         call NhlFSetValues(text_item_id,rlist,ierr)
         
         call NhlFDraw(text_item_id,ierr)
C  
C  Change function code back so result string will be drawn.
C  
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txFuncCode','~',ierr)
         call NhlFRLSetfloat(rlist,'txPosXF',.82,ierr)
         call NhlFSetValues(text_item_id,rlist,ierr)
         
         call NhlFDraw(text_item_id,ierr)
C  
C Change to medium font quality and set function code so full string
C will be drawn.
C  
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txFontQuality','MEDIUM',ierr)
         call NhlFRLSetstring(rlist,'txFuncCode','$',ierr)
         call NhlFRLSetfloat(rlist,'txPosXF',.58,ierr)
         call NhlFRLSetfloat(rlist,'txPosYF',.42-(i+1)*.02,ierr)
         call NhlFSetValues(text_item_id,rlist,ierr)
         
         call NhlFDraw(text_item_id,ierr)
C  
C  Change function code back to get result string to draw.
C  
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txFuncCode','~',ierr)
         call NhlFRLSetfloat(rlist,'txPosXF',.82,ierr)
         call NhlFSetValues(text_item_id,rlist,ierr)
         
         call NhlFDraw(text_item_id,ierr)
         
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'txFontQuality','HIGH',ierr)
         call NhlFSetValues(text_item_id,rlist,ierr)
 10   continue
C  
C  Examples of setting font height.
C  
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.86,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.01,ierr)
      call NhlFRLSetstring(rlist,'txString','txFontHeightF = .01',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.84,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.015,ierr)
      call NhlFRLSetstring(rlist,'txString','txFontHeightF = .015',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.80,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.02,ierr)
      call NhlFRLSetstring(rlist,'txString','txFontHeightF = .02',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.75,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.025,ierr)
      call NhlFRLSetstring(rlist,'txString','txFontHeightF = .025',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
C  
C  Examples setting the rotation angle of the text.
C  
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.26,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.47,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.015,ierr)
      call NhlFRLSetinteger(rlist,'txJust',1,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 0.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',0.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.26,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.5,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 45.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',45.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetfloat(rlist,'txPosXF',.23,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.50,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 90.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',90.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txPosXF',.20,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.5,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 135.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',135.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txPosXF',.20,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.47,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 180.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',180.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txPosXF',.20,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.44,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 225.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',225.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txPosXF',.23,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.44,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 270.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',270.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txPosXF',.26,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.44,ierr)
      call NhlFRLSetstring(rlist,'txString','txAngleF = 315.0',ierr)
      call NhlFRLSetfloat(rlist,'txAngleF',315.0,ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txAngleF',0.0,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.15,ierr)
      call NhlFRLSetinteger(rlist,'txJust', 4,ierr)
      call NhlFRLSetstring(rlist,'txString','NhlNtxJust = 4',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txAngleF',0.0,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.18,ierr)
      call NhlFRLSetinteger(rlist,'txJust', 1,ierr)
      call NhlFRLSetstring(rlist,'txString','NhltxJust = 1',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetfloat(rlist,'txAngleF',0.0,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.25,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.12,ierr)
      call NhlFRLSetinteger(rlist,'txJust', 7,ierr)
      call NhlFRLSetstring(rlist,'txString','NhlNtxJust = 7',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
C  
C  End of first frame.
C  
      call NhlFFrame(wid,ierr)
C  
C  Remainder of calls demonstrate the various fonts available for use
C  with the text item. Note that the 'txString' values demonstrate a
C  mid-string font change
C  
      i = 0
      
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'txFuncCode','$',ierr)
      call NhlFRLSetinteger(rlist,'txFont', 0,ierr)
      call NhlFRLSetinteger(rlist,'txJust', 4,ierr)
      call NhlFRLSetfloat(rlist,'txFontHeightF',.02,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95-i*.027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'pwritx database : $F21$txFont = 0',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 1,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95-i*.027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'default : $F21$txFont = 1',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 2,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'cartographic roman : $F21$txFont = 2',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 3,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'cartographic greek : $F21$txFont = 3',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 4,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'simplex roman: $F21$txFont = 4',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 5,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'simplex greek: $F21$txFont = 5',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 6,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'simplex script: $F21$txFont = 6',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 7,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'complex roman: $F21$txFont = 7',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 8,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'complex greek: $F21$txFont = 8',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 9,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'complex script: $F21$txFont = 9',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 10,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'complex script: $F21$txFont = 10',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 11,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'complex cyrillic: $F21$txFont = 11',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 12,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'duplex roman: $F21$txFont = 12',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 13,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'triplex roman: $F21$txFont = 13',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 14,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'triplex italic: $F21$txFont = 14',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 15,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'gothic german: $F21$txFont = 15',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 16,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'gothic english: $F21$txFont = 16',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 17,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'gothic italian: $F21$txFont = 17',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 18,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'math symbols: $F21$txFont = 18',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 19,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'symbols set1: $F21$txFont = 19',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 20,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'symbols set2: $F21$txFont = 20',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 21,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'helvetica: $F21$txFont = 21',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 22,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'helvetica bold: $F21$txFont = 22',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 25,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'times-roman : $F21$txFont = 25',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 26,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'times-roman bold : $F21$txFont = 26',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 29,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'courier : $F21$txFont = 29',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 30,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'courier bold : $F21$txFont = 30',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 33,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'greek : $F21$txFont = 33',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 34,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'math-symbols : $F21$txFont = 34',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 35,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,
     1     'txString','text-symbols : $F21$txFont = 35',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      
      call NhlFRLSetinteger(rlist,'txFont', 36,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'weather1 : $F21$txFont = 36',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      
      call NhlFDraw(text_item_id,ierr)
      
      call NhlFRLClear(rlist)
      call NhlFRLSetinteger(rlist,'txFont', 37,ierr)
      call NhlFRLSetfloat(rlist,'txPosXF',.5,ierr)
      call NhlFRLSetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call NhlFRLSetstring(rlist,'txString',
     1     'weather2 : $F21$txFont = 37',ierr)
      call NhlFSetValues(text_item_id,rlist,ierr)
      
      call NhlFDraw(text_item_id,ierr)
C  
C  End of second and final frame.
C  
      call NhlFFrame(wid,ierr)
C  
C  Clean up and close hlu library.
C  
      call NhlFDestroy(text_item_id,ierr)
      call NhlFClose
      
      end
