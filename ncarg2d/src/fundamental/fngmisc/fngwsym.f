
      PROGRAM FNGWSYM
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
C
C  Plots tables of all the weather symbols.
C
      CHARACTER*2 CFNT(7)
      CHARACTER*25 YLAB(2,7)
      INTEGER YLEN(2,7)
      DATA CFNT/'a ','N ','W ','CH','CM','CL','C '/
      DATA YLAB/ 'a','Pressure Tendency ',
     +           'N','Sky Cover         ',
     +           'W','Past Weather      ',
     +           'CH','High Clouds      ',
     +           'CM','Medium Clouds    ',
     +           'CL','Low Clouds       ',
     +           'C','Cloud Types       '/
      DATA YLEN/1,17,
     +          1, 9,
     +          1,12,
     +          2,11,
     +          2,13,
     +          2,10,
     +          1,11/ 
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSLWSC(1.)
C
C  Font WW -- Present Weather table.
C
C  Table width.
C
      X1 = .025
      X2 = .975
C
C  Table heights.
C  
      Y1 = 0.
      Y2 = .95
      Y3 = 1.
C
C  Number of columns and rows.
C
      NC = 10
      NR = 10
C
      CALL GSPLCI(1)
      CALL GSTXCI(1)
      CALL DTABLE('WW',NR,NC,X1,Y1,X2,Y2,Y3,1,1,1)
      SIZE = .4*(Y3-Y2)
      YY = .5*(Y2+Y3)
      CALL NPUTS(YY,'WW','Present Weather',SIZE)
      CALL FRAME
C
      NR = 1
      NC = 10
      R = 4.5/12.
      DY = .095
      SPC = 1./7.-(1.+R)*DY
      DO 10 NF=1,7
         YB = (NF-1)*((1.+R)*DY+SPC)
         Y1 = YB
         Y2 = YB+DY
         Y3 = YB+(1+R)*DY
         CALL DTABLE(CFNT(NF),NR,NC,X1,Y1,X2,Y2,Y3,1,1,1)
         YY = .5*(Y2+Y3)
         CALL NPUTS(YY,YLAB(1,NF)(1:YLEN(1,NF)),
     +        YLAB(2,NF)(1:YLEN(2,NF)),.5*(Y3-Y2))
 10   CONTINUE
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END

      SUBROUTINE DTABLE(FONT,NR,NC,X1,Y1,X2,Y2,Y3,ITX1,ITX2,ILNC)
C
C  Draw table of characters from font FONT with NR rows and NC columns.
C  The boundary of the table in world coordinates is (X1,Y1) to (X2,Y2).
C  The line color index is ILNC and the text color index for the label
C  characters is ITX1 and for the displayed characters is ITX2.
C
      DIMENSION XX(2),YY(2),XC(10),YC(10),XLC(10),YLC(10)
      CHARACTER*(*) FONT
      CHARACTER*2 NLAB
C
C  Change the flag for Plotchar function codes to a non-printing
C  ASCII character since function codes will not be  
C  used and all printing ASCII characters will be used.
C
      CALL PCSETC('FC',CHAR(30))
C
C  Draw table grid and compute the centers of the grid boxes.
C
      CALL GSLWSC(2.)
      CALL GSPLCI(ILNC)
      YINC = (Y2-Y1)/REAL(NR)
      DO 10 J=1,NR+1
         YY(1) = Y1+(J-1)*YINC
         YY(2) = YY(1)
         XX(1) = X1
         XX(2) = X2
         CALL GPL(2,XX,YY)
         IF (J .LE. NR) THEN
            YC(J) = YY(1)+.4*YINC
            YLC(J) = YY(1)+.8*YINC
         ENDIF
 10   CONTINUE
C     
      XINC = (X2-X1)/REAL(NC)
      DO 20 I=1,NC+1
         YY(1) = Y1
         YY(2) = Y2
         XX(1) = X1+(I-1)*XINC
         XX(2) = XX(1)
         CALL GPL(2,XX,YY)
         IF (I .LE. NC) THEN
            XC(I) = XX(1)+.6*XINC
            XLC(I) = XX(1)+.2*XINC
         ENDIF
 20   CONTINUE
      CALL GSLWSC(1.)
C
C  Draw the characters in the boxes.
C
      CALL GSTXAL(2,3)
      CALL GSTXFP(-36,2)
C    
      CALL GSTXCI(ITX2)
      DO 30 J=1,NR
         DO 40 I=1,NC
            NUM = (NR-J)*NC+I-1
            CALL NGWSYM(FONT,NUM,XC(I),YC(J),.43*YINC,1,0)
 40      CONTINUE
 30   CONTINUE
C    
C  Draw the character number in the upper left of the box.
C
      CHGT = .14*YINC
      DO 50 J=1,NR
         DO 60 I=1,NC
            NUM = (NR-J)*NC+I-1
            WRITE(NLAB,500) NUM
 500        FORMAT(I2)
            CALL PCSETI('FN',26)
            CALL PCSETI('CC',ITX1)
            CALL PLCHHQ(XLC(I),YLC(J),NLAB,6.*CHGT/7.,0.,0.)
 60      CONTINUE
 50   CONTINUE
C
      RETURN
      END

      SUBROUTINE NPUTS(YY,STR1,STR2,SIZE)
C
C  Put out STR1, then a dash, then STR2 at size SIZE.
C
      CHARACTER*(*) STR1,STR2
      DIMENSION TX(4),TY(4)
C
      CALL GSTXFP(-22,2)
      CALL GSTXAL(1,3)
      CALL GSCHH(SIZE)
      CALL GQTXX(1,0.,YY,STR1,IER,CPX,CPY,TX,TY)
      DX1 = CPX
      CALL GQTXX(1,0.,YY,STR2,IER,CPX,CPY,TX,TY)
      DX3 = CPX
      CALL GSTXFP(-34,2)
      CALL GQTXX(1,0.,YY,' > ',IER,CPX,CPY,TX,TY)
      DX2 = CPX
      DX = DX1+DX2+DX3
      STRTX = .5-.5*DX
      CALL PCSETI('FN',22)
      CALL PLCHHQ(STRTX,YY,STR1,6.*SIZE/7.,0.,-1.)
      CALL PLCHHQ(STRTX+DX1+DX2,YY,STR2,6.*SIZE/7.,0.,-1.)
      CALL GSTXFP(-34,2)
      CALL PCSETI('FN',34)
      CALL PLCHHQ(STRTX+DX1,YY,' > ',6.*SIZE/7.,0.,-1.)
C
      RETURN
      END
