
      PROGRAM CLBFIL
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)

      CHARACTER*10 LAB1(4)
      INTEGER IFILL1(4)

      DATA LAB1 /'Four','Different','Fill','Styles'/
      DATA IFILL1 /1,2,3,4/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Draw a horizontal bar
C
      CALL LBLBAR(0,.05,.95,.05,.95,4,1.,.3,IFILL1,2,LAB1,4,1)
C
C Advance frame
C
      CALL FRAME
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END

      SUBROUTINE LBFILL(IFTP,XRA,YRA,NRA,INDX)
C
C Declare required dimensioned arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(500),IND(500)
      DIMENSION ID1(8,8),ID2(8,8),ID3(8,8)
C
C Define three different dot patterns.
C
      DATA ID1 / 1,1,0,0,0,0,1,1,
     +      1,1,0,1,1,0,1,1,
     +      0,0,0,1,1,0,0,0,
     +      0,1,1,1,1,1,1,0,
     +      0,1,1,1,1,1,1,0,
     +      0,0,0,1,1,0,0,0,
     +      1,1,0,1,1,0,1,1,
     +      1,1,0,0,0,0,1,1/
      DATA ID2 / 0,0,0,0,0,0,0,0,
     +      0,1,1,1,1,1,1,0,
     +      0,1,1,1,1,1,1,0,
     +      0,1,1,0,0,1,1,0,
     +      0,1,1,0,0,1,1,0,
     +      0,1,1,1,1,1,1,0,
     +      0,1,1,1,1,1,1,0,
     +      0,0,0,0,0,0,0,0/
      DATA ID3 / 0,0,0,0,0,0,0,0,
     +      0,1,1,0,0,1,1,1,
     +      0,1,1,0,0,1,1,0,
     +      0,1,1,0,1,1,0,0,
     +      0,1,1,1,1,0,0,0,
     +      0,1,1,0,1,1,0,0,
     +      0,1,1,0,0,1,1,0,
     +      0,1,1,0,0,1,1,1/
C
C Double the size of the GKS dot.
C
      CALL GSMKSC (2.)
C
C Fill the first box with a combination of lines and dots.
C
      IF (INDX.EQ.1) THEN
         CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
         CALL SFSETI ('DO - DOT-FILL FLAG',0)
         CALL SFWRLD (XRA,YRA,NRA,DST,500,IND,500)
         CALL SFSETR ('SP - SPACING OF FILL LINES',.006)
         CALL SFSETI ('DO - DOT-FILL FLAG',1)
         CALL SFNORM (XRA,YRA,NRA,DST,500,IND,500)
      ENDIF
C
C Fill the second box with a specified dot pattern.
C
      IF (INDX.EQ.2) THEN
         CALL SFSETR ('SP - SPACING OF FILL LINES',.004)
         CALL SFSETP (ID1)
         CALL SFWRLD (XRA,YRA,NRA,DST,500,IND,500)
      ENDIF
C
C Fill the third box with a different dot pattern, tilted at an
C angle.
C
      IF (INDX.EQ.3) THEN
         CALL SFSETI ('AN - ANGLE OF FILL LINES',45)
         CALL SFSETP (ID2)
         CALL SFWRLD (XRA,YRA,NRA,DST,500,IND,500)
      ENDIF
C
C Fill the last box with K's, both large and small.
C
      IF (INDX.EQ.4) THEN
         CALL GSCHH  (.008)
         CALL SFSETR ('SP - SPACING OF FILL LINES',.012)
         CALL SFSETC ('CH - CHARACTER SPECIFIER','K')
         CALL SFSETP (ID3)
         CALL SFWRLD (XRA,YRA,NRA,DST,500,IND,500)
      ENDIF
C
C Done.
C
      RETURN
C
      END
