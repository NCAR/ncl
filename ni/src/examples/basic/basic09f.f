CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                             Copyright (C)  1996                      C
C               University Corporation for Atmospheric Research        C
C                             All Rights Reserved                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    
C   File:         basic09f.f
C
C   Author:       Mary Haley
C                 National Center for Atmospheric Research
C                 PO 3000, Boulder, Colorado  80303
C
C   Date:         Wed Jul 10 09:16:08 MDT 1996
C
C   Description: This example displays all the available fonts
C
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFXWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFTextItemClass
C
C  List of available fonts (by number and name)
C
      character*2 font_nums(31)
      character*18 font_names(31)

      data font_nums/'1','2','3','4','5','6','7','8','9','10','11',
     +     '12','13','14','15','16','17','18','19','20','21','22',
     +     '25','26','29','30','33','34','35','36','37'/

      data font_names/'default','cartographic_roman',
     +     'cartographic_greek','simplex_roman','simplex_greek',
     +     'simplex_script','complex_roman','complex_greek',
     +     'complex_script','complex_italic','complex_cyrillic',
     +     'duplex_roman','triplex_roman','triplex_italic',
     +     'gothic_german','gothic_english','gothic_italian',
     +     'math_symbols','symbol_set1','symbol_set2','helvetica',
     +     'helvetica-bold','times-roman','times-bold','courier',
     +     'courier-bold','greek','math-symbols','text-symbols',
     +     'weather1','weather2'/

      integer appid,wid,txid1,txid2,txid3,txid4,rlist
      integer num_fonts, num_lines
      integer i, j, div1, mod1
      real k
      character*50 string

      integer NCGM,X11,PS
      num_fonts = 31
      num_lines = 18
C
C Set the display. Default is to display output to an X workstation.
C
      NCGM=0
      X11=1
      PS=0
C
C Initialize the high level utility library and create application.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'SETRL')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'basic09',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create a metafile workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName',
     &        './basic09f.ncgm',ierr)
         call NhlFCreate(wid,'wks',
     &        NhlFNcgmWorkstationClass,0,rlist,ierr)
      endif

      if (X11 .eq. 1) then
C
C Create an X workstation.         
C      
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'wks',NhlFXWorkstationClass,
     &        0,rlist,ierr)
      endif

      if (PS .eq. 1) then
C
C Create a PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName',
     &        './basic09f.ps',ierr)
         call NhlFCreate(wid,'wks',NhlFPSWorkstationClass,
     &        0,rlist,ierr)
      endif
C
C We only want num_lins lines on a page
C
      div1 = num_fonts / num_lines
      mod1 = mod(num_fonts,num_lines)
      if (mod1.ne.0) div1 = div1 + 1
C
C Create header to put on all frames
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'txFuncCode','*',ierr)
      call NhlFRLSetFloat(rlist,'txPosXF',0.32,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',0.98,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',0.02,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor',3,ierr)
      call NhlFRLSetString(rlist,'txString','font number : font name',
     +     ierr)
      call NhlFCreate(txid1,'TextItem1',NhlFtextItemClass,wid,rlist,
     +     ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'txPosXF',0.72,ierr)
      call NhlFRLSetFloat(rlist,'txPosYF',0.98,ierr)
      call NhlFRLSetFloat(rlist,'txFontHeightF',0.02,ierr)
      call NhlFRLSetInteger(rlist,'txFontColor',5,ierr)
      call NhlFRLSetString(rlist,'txString','font example',ierr)
      call NhlFCreate(txid2,'TextItem2',NhlFtextItemClass,wid,rlist,
     +     ierr)
      do 40 j = 1,div1
         k = 0.90
         call NhlFDraw(txid1,ierr)
         call NhlFDraw(txid2,ierr)
C
C Loop over each font
C
         do 30 i=(j-1)*num_lines,min(num_fonts-1,j*num_lines-1)
            string = font_nums(i+1) // ' : ' // font_names(i+1)
C
C Font number and name
C
            call NhlFRLClear(rlist)
            call NhlFRLSetInteger(rlist,'txJust',2,ierr)
            call NhlFRLSetString(rlist,'txFuncCode','*',ierr)
            call NhlFRLSetFloat(rlist,'txPosXF',0.1,ierr)
            call NhlFRLSetFloat(rlist,'txPosYF',k,ierr)
            call NhlFRLSetFloat(rlist,'txFontHeightF',0.02,ierr)
            call NhlFRLSetInteger(rlist,'txFontColor',3,ierr)
            call NhlFRLSetString(rlist,'txString',string,ierr)
            call NhlFCreate(txid3,'TextItem3',NhlFtextItemClass,wid,
     +           rlist,ierr)
C
C Actual font drawn using the words 'NCAR Graphics'
C
            call NhlFRLClear(rlist)
            call NhlFRLSetInteger(rlist,'txJust',2,ierr)
            call NhlFRLSetString(rlist,'txFont',font_names(i+1),ierr)
            call NhlFRLSetFloat(rlist,'txPosXF',0.6,ierr)
            call NhlFRLSetFloat(rlist,'txPosYF',k,ierr)
            call NhlFRLSetFloat(rlist,'txFontHeightF',0.02,ierr)
            call NhlFRLSetInteger(rlist,'txFontColor',5,ierr)
            call NhlFRLSetString(rlist,'txString','NCAR Graphics',ierr)
            call NhlFCreate(txid4,'TextItem4',NhlFtextItemClass,wid,
     +           rlist,ierr)
            call NhlFDraw(txid3,ierr)
            call NhlFDraw(txid4,ierr)
            call NhlFDestroy(txid3,ierr)
            call NhlFDestroy(txid4,ierr)
            k = k - 0.05
 30      continue
         call NhlFFrame(wid,ierr)
 40   continue
C
C Clean up.
C
      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end
