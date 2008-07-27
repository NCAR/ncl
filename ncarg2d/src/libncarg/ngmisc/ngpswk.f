C
C	$Id: ngpswk.f,v 1.7 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION NGPSWK(PSTYPE, ORIENT, COLOR)
C
C  This function returns the workstation type for the PostScript
C  workstation, in NCAR GKS, whose attributes are specified in the 
C  arguments:
C
C    PSTYPE - of type CHARACTER and specifies the type of the PostScript 
C             file.  It can be one of:
C
C               'PS'   - regular PostScript
C               'EPS'  - Encapsulated PostScript (can have only a 
C                        single picture).
C               'EPSI' - Encapsulated PostScript Interchange format
C                        (an EPS file containing a preview bitmap).
C
C             The value 'PS' is used and a warning is issued if the 
C             argument is unrecognized.
C
C    ORIENT - of type CHARACTER and specifies the orientation of the
C             output plot.  It can be one of:
C  
C               'PORTRAIT'  - portrait orietation (long side of page
C                             is vertical).
C               'LANDSCAPE' - landscape orientation (long side of
C                             the page is horizontal).
C
C             The value 'PORTRAIT' is used and a warning is issued if the 
C             argument is unrecognized.
C
C    COLOR  - of type CHARACTER and specifies whether the plot will be
C             color or monochrome.  It can be one of:
C  
C               'MONOCHROME' - plot with be plotted with all graphics
C                              objects being plotted using the foreground
C                              color.
C               'COLOR'      - plot will be plotted using color, if
C                              available, or intensities obtained from
C                              the NTSC color to black and white intensity
C                              mapping if gray scale is available.
C
C             The value 'COLOR' is used and a warning is issued if the 
C             argument is unrecognized.
C
C  In specifying the arguments, only enough characters need be entered
C  so that the values can be differentiated.  For example, to
C  specify color, using 'C' as the third argument would be sufficient.
C  Either upper case or lower case is accepted.
C
      CHARACTER PSTYPE*(*), ORIENT*(*), COLOR*(*) 
      CHARACTER IFMT*80
C
      DIMENSION ITYPES(3,2,2)
      DATA ITYPES/20, 21, 22, 26, 27, 28, 23, 24, 25, 29, 30, 31/
C
C  Check for an uncleared prior error.
C
      IF (ICFELL('NGPSWK - Uncleared prior error',1) .NE. 0) RETURN
C
C  Handle the PostScript type argument.
C
      ILEN = LEN(PSTYPE)
      IF (ILEN .LT. 1) THEN
        CALL SETER (
     +   'NGPSWK - first argument too short, defaulted to ''PS''', 
     +   1, 1)
        IPST = 1
        GO TO 200
      ENDIF
      IF (PSTYPE(1:1).EQ.'P' .OR. PSTYPE(1:1).EQ.'p') THEN
        IPST = 1
        GO TO 200
      ELSE IF (ILEN .EQ. 3) THEN
        IF (PSTYPE(1:3).EQ.'EPS' .OR. PSTYPE(1:3).EQ.'eps') THEN
          IPST = 2
          GO TO 200
        ELSE
          WRITE (I1MACH(4),500) PSTYPE(1:3)
  500     FORMAT(
     +      'NGPSWK - ''',A3,''' unrecognized, defaulted to ''PS''')
          IPST = 1
          GO TO 200
        ENDIF
      ELSE IF (ILEN .GT. 3) THEN
        IF (PSTYPE(1:4).EQ.'EPS ' .OR. PSTYPE(1:4).EQ.'eps ') THEN 
          IPST = 2
          GO TO 200
        ELSE IF (PSTYPE(1:4).EQ.'EPSI' .OR. PSTYPE(1:4).EQ.'epsi') THEN
          IPST = 3
          GO TO 200
        ELSE
          ILEN = LEN(PSTYPE)
          WRITE(IFMT,660) ILEN
  660     FORMAT('(''NGPSWK - '''''',A',I3,','''''' unrecognized, defaul
     +ted to ''''PS'''''')')
          WRITE (I1MACH(4),IFMT) PSTYPE(1:ILEN)
          IPST = 1
          GO TO 200
        ENDIF
      ELSE
        ILEN = LEN(PSTYPE)
        WRITE(IFMT,650) ILEN
  650   FORMAT('(''NGPSWK - '''''',A',I3,','''''' unrecognized, defaulte
     +d to ''''PS'''''')')
        WRITE (I1MACH(4),IFMT) PSTYPE(1:ILEN)
        IPST = 1
        GO TO 200
      ENDIF
  200 CONTINUE
C
C  Handle the orientation argument.
C
      IF (ORIENT(1:1).EQ.'P' .OR. ORIENT(1:1).EQ.'p') THEN
        IORN = 1
        GO TO 210
      ELSE IF (ORIENT(1:1).EQ.'L' .OR. ORIENT(1:1).EQ.'l') THEN 
        IORN = 2
        GO TO 210
      ELSE
        ILEN = LEN(ORIENT)
        WRITE(IFMT,670) ILEN
  670   FORMAT('(''NGPSWK - '''''',A',I3,','''''' unrecognized, defaulte
     +d to ''''PORTRAIT'''''')')
        WRITE (I1MACH(4),IFMT) ORIENT(1:ILEN)
        IORN = 1
        GO TO 210
      ENDIF
  210 CONTINUE
C
C  Handle the color argument.
C
      IF (COLOR(1:1).EQ.'C' .OR. COLOR(1:1).EQ.'c') THEN
        ICLR = 1
        GO TO 220
      ELSE IF (COLOR(1:1).EQ.'M' .OR. COLOR(1:1).EQ.'m') THEN 
        ICLR = 2
        GO TO 220
      ELSE
        ILEN = LEN(COLOR)
        WRITE(IFMT,680) ILEN
  680   FORMAT('(''NGPSWK - '''''',A',I3,','''''' unrecognized, defaulte
     +d to ''''COLOR'''''')')
        WRITE (I1MACH(4),IFMT) COLOR(1:ILEN)
        ICLR = 1
        GO TO 220
      ENDIF
  220 CONTINUE
C
      NGPSWK = ITYPES(IPST, IORN, ICLR)
      RETURN
      END
