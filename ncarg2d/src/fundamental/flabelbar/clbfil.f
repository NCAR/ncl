      PROGRAM CLBFIL
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)

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
    1      1,1,0,1,1,0,1,1,
    1      0,0,0,1,1,0,0,0,
    2      0,1,1,1,1,1,1,0,
    3      0,1,1,1,1,1,1,0,
    4      0,0,0,1,1,0,0,0,
    5      1,1,0,1,1,0,1,1,
    6      1,1,0,0,0,0,1,1/
      DATA ID2 / 0,0,0,0,0,0,0,0,
    1      0,1,1,1,1,1,1,0,
    2      0,1,1,1,1,1,1,0,
    3      0,1,1,0,0,1,1,0,
    4      0,1,1,0,0,1,1,0,
    5      0,1,1,1,1,1,1,0,
    6      0,1,1,1,1,1,1,0,
    7      0,0,0,0,0,0,0,0/
      DATA ID3 / 0,0,0,0,0,0,0,0,
    1      0,1,1,0,0,1,1,1,
    2      0,1,1,0,0,1,1,0,
    3      0,1,1,0,1,1,0,0,
    4      0,1,1,1,1,0,0,0,
    5      0,1,1,0,1,1,0,0,
    6      0,1,1,0,0,1,1,0,
    7      0,1,1,0,0,1,1,1/
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
