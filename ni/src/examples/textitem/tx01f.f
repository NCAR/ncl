C
C     $Id: tx01f.f,v 1.1 1995-01-26 16:43:35 haley Exp $
C
C************************************************************************
C                                                                       *
C                            Copyright (C)  1995                        *
C                 University Corporation for Atmospheric Research       *
C                            All Rights Reserved                        *
C                                                                       *
C************************************************************************
C
C      File:            ti01f.f
C
C      Author:          Jeff Boote (converted to Fortran by Ed Stautler)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 10:28:20 MST 1995
C
C      Description:     Demonstrates TextItem object.
C
      program ti01f
      implicit none
C
C  This program is the equivalent of nhl05.c.
C
      external nhlfapplayerclass
      external nhlfncgmworkstationlayerclass
      external nhlfmapplotlayerclass
      external nhlftextitemlayerclass
      
      integer i,appid,text_item_id,wid,rlist,ierr
      
      real oparent
C     
C  Initialize the high level utility library.
C     
      call nhlfinitialize
C   
C  Create an application context.  Set the app dir to the current directory
C  so the application looks for the resource file in the execution 
C  directory.
C   
      call nhlfrlcreate(rlist,'setrl')
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'appUsrDir','./',ierr)
      call nhlfcreate(appid, 'ti01',nhlfapplayerclass,0,0,ierr)
C  
C  Create a meta file workstation
C  
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'wkMetaName','./ti01f.ncgm',ierr)
      call nhlfcreate(wid,'ti01Work',nhlfncgmworkstationlayerclass,0,
     1     rlist,ierr)
C  
C  This is the only creation of a text object for this program.
C  
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'txJust',4,ierr)
      call nhlfrlsetinteger(rlist,'txFont',25,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.03,ierr)
      call nhlfrlsetstring(rlist,'txString',
     1     'TextItem - VARIOUS CAPABILITIES',ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95,ierr)
      call nhlfcreate(text_item_id,'Nhl05TextItem',
     1     nhlftextItemLayerClass,wid,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.015,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.7,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.86,ierr)
      call nhlfrlsetstring(rlist,'txString',
     1     'HIGH-QUALITY CHARACTERS USED BELOW',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.7,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.5,ierr)  
      call nhlfrlsetstring(rlist,'txString',
     1     'MEDIUM-QUALITY CHARACTERS USED BELOW',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.58,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.81,ierr)   
      call nhlfrlsetstring(rlist,'txString','txString',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosYF',.46,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.82,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.81,ierr)        
      call nhlfrlsetstring(rlist,'txString','RESULT',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosYF',.46,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.82,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.78,ierr)
      call nhlfrlsetstring(rlist,'txString','------',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosYF',.43,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.58,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.78,ierr)
      call nhlfrlsetstring(rlist,'txString','------------',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosYF',.43,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
C  
C  Below are examples using the function control characters. In each iteration
C  of the loop a new string is set using call nhlfsetvalues. In order to
C  demonstrate what the value of 'txString' is, the function code is 
C  temporarily changed causing the entire contents of 'txString' to be drawn.
C  
      do 10 i=0,11
         if (i .eq. 0) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',':L:A',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 1) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',':IGL:A',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 2) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',
     1           'A:S:2:N:+B:S:2:N:',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 3) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString','A:S:B',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 4) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString','A:SPU:B',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 5) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',':GIU:+',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 6) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',':1045:',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 7) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString','10:S:10:S:100',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 8) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',
     1           'X:B1:2:S1:3',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 9) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',
     1           'X:B1:2:S:3:N:Y:S:2',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 10) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',
     1           'X:S:A:B:1:NN:ABC',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         else if (i .eq. 11) then
            call nhlfrlclear(rlist)
            call nhlfrlsetstring(rlist,'txString',
     1           '1.3648:L1:410:S:-13',ierr)
            call nhlfsetvalues(text_item_id,rlist,ierr)
         end if
C  
C  Change function code so full string will be drawn.
C  
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txFuncCode','$',ierr)
         call nhlfrlsetfloat(rlist,'txFontHeightF',.01,ierr)
         call nhlfrlsetfloat(rlist,'txPosXF',.58,ierr)
         call nhlfrlsetfloat(rlist,'txPosYF',.78-(i+1)*.02,ierr)
         call nhlfsetvalues(text_item_id,rlist,ierr)
         
         call nhlfdraw(text_item_id,ierr)
C  
C  Change function code back so result string will be drawn.
C  
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txFuncCode',':',ierr)
         call nhlfrlsetfloat(rlist,'txPosXF',.82,ierr)
         call nhlfsetvalues(text_item_id,rlist,ierr)
         
         call nhlfdraw(text_item_id,ierr)
C  
C  Change to medium font quality and set function code so full string will be
C  drawn.
C  
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txFontQuality','MEDIUM',ierr)
         call nhlfrlsetstring(rlist,'txFuncCode','$',ierr)
         call nhlfrlsetfloat(rlist,'txPosXF',.58,ierr)
         call nhlfrlsetfloat(rlist,'txPosYF',.42-(i+1)*.02,ierr)
         call nhlfsetvalues(text_item_id,rlist,ierr)
         
         call nhlfdraw(text_item_id,ierr)
C  
C  Change function code back to get result string to draw.
C  
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txFuncCode',':',ierr)
         call nhlfrlsetfloat(rlist,'txPosXF',.82,ierr)
         call nhlfsetvalues(text_item_id,rlist,ierr)
         
         call nhlfdraw(text_item_id,ierr)
         
         call nhlfrlclear(rlist)
         call nhlfrlsetstring(rlist,'txFontQuality','HIGH',ierr)
         call nhlfsetvalues(text_item_id,rlist,ierr)
 10   continue
C  
C  Examples of setting font height.
C  
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.86,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.01,ierr)
      call nhlfrlsetstring(rlist,'txString','txFontHeightF = .01',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.84,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.015,ierr)
      call nhlfrlsetstring(rlist,'txString','txFontHeightF = .015',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.80,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.02,ierr)
      call nhlfrlsetstring(rlist,'txString','txFontHeightF = .02',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.75,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.025,ierr)
      call nhlfrlsetstring(rlist,'txString','txFontHeightF = .025',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
C  
C  Examples setting the rotation angle of the text.
C  
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.26,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.47,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.015,ierr)
      call nhlfrlsetinteger(rlist,'txJust',1,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 0.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',0.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.26,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.5,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 45.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',45.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetfloat(rlist,'txPosXF',.23,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.50,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 90.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',90.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txPosXF',.20,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.5,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 135.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',135.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txPosXF',.20,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.47,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 180.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',180.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txPosXF',.20,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.44,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 225.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',225.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txPosXF',.23,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.44,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 270.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',270.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txPosXF',.26,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.44,ierr)
      call nhlfrlsetstring(rlist,'txString','txAngleF = 315.0',ierr)
      call nhlfrlsetfloat(rlist,'txAngleF',315.0,ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txAngleF',0.0,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.15,ierr)
      call nhlfrlsetinteger(rlist,'txJust', 4,ierr)
      call nhlfrlsetstring(rlist,'txString','NhlNtxJust = 4',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txAngleF',0.0,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.18,ierr)
      call nhlfrlsetinteger(rlist,'txJust', 1,ierr)
      call nhlfrlsetstring(rlist,'txString','NhltxJust = 1',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetfloat(rlist,'txAngleF',0.0,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.25,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.12,ierr)
      call nhlfrlsetinteger(rlist,'txJust', 7,ierr)
      call nhlfrlsetstring(rlist,'txString','NhlNtxJust = 7',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
C  
C  End of first frame.
C  
      call nhlfframe(wid,ierr)
C  
C  Remainder of calls demonstrate the various fonts available for use
C  with the text item. Note that the 'txString' values demonstrate a
C  mid-string font change
C  
      i = 0
      
      call nhlfrlclear(rlist)
      call nhlfrlsetstring(rlist,'txFuncCode','$',ierr)
      call nhlfrlsetinteger(rlist,'txFont', 0,ierr)
      call nhlfrlsetinteger(rlist,'txJust', 4,ierr)
      call nhlfrlsetfloat(rlist,'txFontHeightF',.02,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95-i*.027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'pwritx database : $F21$txFont = 0',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 1,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95-i*.027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'default : $F21$txFont = 1',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 2,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'cartographic roman : $F21$txFont = 2',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 3,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'cartographic greek : $F21$txFont = 3',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 4,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'simplex roman: $F21$txFont = 4',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 5,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'simplex greek: $F21$txFont = 5',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 6,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'simplex script: $F21$txFont = 6',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 7,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'complex roman: $F21$txFont = 7',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 8,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'complex greek: $F21$txFont = 8',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 9,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'complex script: $F21$txFont = 9',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 10,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'complex script: $F21$txFont = 10',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 11,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'complex cyrillic: $F21$txFont = 11',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 12,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'duplex roman: $F21$txFont = 12',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 13,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'triplex roman: $F21$txFont = 13',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 14,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'triplex italic: $F21$txFont = 14',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 15,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'gothic german: $F21$txFont = 15',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 16,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'gothic english: $F21$txFont = 16',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 17,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'gothic italian: $F21$txFont = 17',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 18,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'math symbols: $F21$txFont = 18',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 19,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'symbols set1: $F21$txFont = 19',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 20,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'symbols set2: $F21$txFont = 20',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 21,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'helvetica: $F21$txFont = 21',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 22,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'helvetica bold: $F21$txFont = 22',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 25,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'times-roman : $F21$txFont = 25',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 26,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'times-roman bold : $F21$txFont = 26',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 29,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'courier : $F21$txFont = 29',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 30,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'courier bold : $F21$txFont = 30',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 33,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'greek : $F21$txFont = 33',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 34,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'math-symbols : $F21$txFont = 34',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 35,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,
     1     'txString','text-symbols : $F21$txFont = 35',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      
      call nhlfrlsetinteger(rlist,'txFont', 36,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'weather1 : $F21$txFont = 36',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      
      call nhlfdraw(text_item_id,ierr)
      
      call nhlfrlclear(rlist)
      call nhlfrlsetinteger(rlist,'txFont', 37,ierr)
      call nhlfrlsetfloat(rlist,'txPosXF',.5,ierr)
      call nhlfrlsetfloat(rlist,'txPosYF',.95 - i * .027,ierr)
      i = i + 1
      call nhlfrlsetstring(rlist,'txString',
     1     'weather2 : $F21$txFont = 37',ierr)
      call nhlfsetvalues(text_item_id,rlist,ierr)
      
      call nhlfdraw(text_item_id,ierr)
C  
C  End of second and final frame.
C  
      call nhlfframe(wid,ierr)
C  
C  Clean up and close hlu library.
C  
      call nhlfdestroy(text_item_id,ierr)
      call nhlfclose
      
      end
