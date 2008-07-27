C
C $Id: nglogo.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2002
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGLOGO(IWK,X,Y,SIZE,ITYPE,ICOL1,ICOL2)
C
C  Draws a logo on a workstation.  The arguments are:
C
C     IWK (integer)
C       The workstation identifier for the workstation you 
C       want the logo plotted to.
C 
C     X,Y (real)
C       Normalized device coordinates specifying the center position
C       where you want the logo.  Normalized device coordinates are
C       between 0. and 1. and extend over the entire viewport.
C
C     SIZE (real)
C       The desired height of the logo, expressed in normalized device
C       coordinates.
C
C     ITYPE (integer)
C       The logo type.  There are three types:
C         1 - An NCAR logo.  This logo will be drawn in a single
C             color if IWK is not a PostScript workstation.  A full
C             color logo will be put to a PostScript workstation.
C         2 - A UCAR logo (just the UCAR star symbol).
C         3 - "NCAR" in Bell Gothic Black font.
C         4 - "UCAR" in Bell Gothic Black font, 
C         5 - UCAR star logo, plus "UCAR" in Bell Gothic Black font at
C             half the height of the star.  In this case, the 
C             coordinate (X,Y) specifies the center of the star part 
C             of the logo.
C             
C     ICOL1 (integer)
C       The color index to be used for the logo color.  For the
C       NCAR logo on PostScript output, this argument is ignored.
C
C     ICOL2 (integer)
C       A secondary color index used only for logo type 5.  For that
C       type, the UCAR star logo is drawn using color index ICOL1 and
C       the text string "UCAR" is drawn using color index ICOL2.
C 
      SAVE
C
      include 'ngcomn.h'
C
      PARAMETER (MAXACT=50)
      DIMENSION CR(4),IACTWKS(MAXACT)
C
C  Input and output data records for ESCAPE function calls.
C
      CHARACTER*80 IDR,CDUM,CTM
      CHARACTER*1  SCH
C
C  Check for an uncleared prior error.
C
      IDR = ' '
      IF (ICFELL('NGLOGO - Uncleared prior error',1) .NE. 0) RETURN
C
C  Turn clipping off.
C
      CALL GQCLIP(IER,IOLDCLIP,CR)
      CALL GSCLIP(0)
C
C  Get the number of the current transformation so it can be
C  restored at the end.
C
      CALL GQCNTN(IER,NTNR)
C
C  Determine the number of active workstations.
C
      CALL GQACWK(1,IER,NUMACT,IDM)
C
C  Check to see if IWK is among the active workstations.
C
      NOTIN = 1
      DO 10 I=1,NUMACT
        CALL GQACWK(I,IER,NUMACT,IACTWKS(I))
        IF (IACTWKS(I) .EQ. IWK) NOTIN = 0
   10 CONTINUE
      IF (NOTIN .EQ. 1) THEN 
        CTM(1:47) = 'NGLOGO - argument 1, workstation is not active.'
        RETURN
      ENDIF
C
C  Deactivate all workstations and reactivate IWK.
C
      DO 15 I=1,NUMACT
        CALL GDAWK(IACTWKS(I))
   15 CONTINUE
      CALL GACWK(IWK)
C
C  Determine some original settings for Plotchar to restore
C  at the end.
C
      CALL PCGETI('CC',ICOLOR)
      CALL PCGETI('FN',IFONT)
      CALL PCGETI('TE',ITEOLD)
      CALL PCGETC('FC',SCH)
C
      XN = X
      YN = Y
      SN = SIZE
C
C  Use the identity transformation.
C
      CALL GETSET(VX1,VX2,VY1,VY2,WX1,WX2,WY1,WY2,ISC)
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETC('FC',':')
C
C  NCAR logo.
C
      IF (ITYPE .EQ. 1) THEN
C
C  Handle the case for PostScript workstations where the
C  full color logo is put out.
C
        CALL GQWKC(IWK,IER,ICONID,IWTYP)
        IF (IWTYP.GE.20.AND.IWTYP.LE.31) THEN
C
          WRITE(IDR( 1: 5),501) IWK
          WRITE(IDR( 6:13),500) XN
          WRITE(IDR(14:21),500) YN
          WRITE(IDR(22:29),500) SN
C
  500     FORMAT(F8.4)
  501     FORMAT(I5)
C
          CALL GESC(-1527,1,IDR,1,IDUM,CDUM)
C
C  Handle the case for non-PostScript workstations.
C
        ELSE
          CALL PCSETI('CC',ICOL1)
          CALL PLCHHQ(X,Y,':F35:t',0.85*SN,0.,0.)
        ENDIF
C
C  UCAR logo - star only.
C
      ELSE IF (ITYPE .EQ. 2) THEN
        CALL PCSETI('CC',ICOL1)
C
C  "x" is digitized to look better at smaller sizes.
C
        IF (SN .GE. 0.125) THEN
          CALL PLCHHQ(XN,YN,':F35:u',0.85*SN,0.,0.)
        ELSE
          CALL PLCHHQ(XN,YN,':F35:x',0.85*SN,0.,0.)
        ENDIF
C
C  "NCAR" text only.
C
      ELSE IF (ITYPE .EQ. 3) THEN
        CALL PCSETI('CC',ICOL1)
        CALL PLCHHQ(XN,YN,':F35:v',0.85*SN,0.,0.)
C
C  "UCAR" text only.
C
      ELSE IF (ITYPE .EQ. 4) THEN
        CALL PCSETI('CC',ICOL1)
        CALL PLCHHQ(XN,YN,':F35:w',0.85*SN,0.,0.)
C
C  UCAR star logo with "UCAR" text.
C
      ELSE IF (ITYPE .EQ. 5) THEN
        CALL PCSETI('TE',1)
        CALL PCSETI('CC',ICOL1)
C
C  "x" is digitized to look better at smaller sizes.
C
        IF (SN .GE. 0.125) THEN
          CALL PLCHHQ(XN,YN,':F35:u',0.85*SN,0.,0.)
        ELSE
          CALL PLCHHQ(XN,YN,':F35:x',0.85*SN,0.,0.)
        ENDIF
        CALL PCGETR ('XE',XE)
        CALL PCSETI('CC',ICOL2)
        CALL PLCHHQ(XE,YN,':F35:w',0.425*SN,0.,-1.)
C
      ENDIF
C
C  Reactivate all deactivated workstations.
C
      DO 20 I=1,NUMACT
        IF (IACTWKS(I) .NE. IWK) CALL GACWK(IACTWKS(I))
   20 CONTINUE
C
C  Restore original settings.
C
      CALL SET(VX1,VX2,VY1,VY2,WX1,WX2,WY1,WY2,ISC)
      CALL GSELNT(NTNR)
      CALL GSCLIP(IOLDCLIP)
      CALL PCSETI('CC',ICOLOR)
      CALL PCSETI('FN',IFONT)
      CALL PCSETI('TE',ITEOLD)
      CALL PCSETC('FC',SCH)
C
      RETURN
      END
