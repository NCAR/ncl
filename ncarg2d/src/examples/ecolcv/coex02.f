
      PROGRAM COEX02
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
C  Number of squares in the X and Y directions.
C
      PARAMETER (NX=4, NY=4)
C
C  Size of each square; spacing between squares.
C
      PARAMETER (SZX=.235, SZY=.135, Y0=.10, Y1=.88)
      PARAMETER (SPX=(1.-NX*SZX)/(NX+1), SPY=(Y1-Y0-NY*SZY)/(NY-1) )
C
C  Arrays to store labels.
C
      CHARACTER*25 TLAB(NX,NY)
      CHARACTER*20 BLAB(NX,NY)
C
C  Color value array.
C
      DIMENSION RGB(3,NX,NY)
C
      DATA RGB(1,1,1),RGB(2,1,1),RGB(3,1,1)/ 0.86, 0.58, 0.44/
      DATA RGB(1,2,1),RGB(2,2,1),RGB(3,2,1)/ 0.65, 0.16, 0.16/
      DATA RGB(1,3,1),RGB(2,3,1),RGB(3,3,1)/ 1.00, 0.50, 0.00/
      DATA RGB(1,4,1),RGB(2,4,1),RGB(3,4,1)/ 1.00, 0.00, 0.00/
      DATA RGB(1,1,2),RGB(2,1,2),RGB(3,1,2)/ 1.00, 1.00, 0.00/
      DATA RGB(1,2,2),RGB(2,2,2),RGB(3,2,2)/ 0.00, 1.00, 0.00/
      DATA RGB(1,3,2),RGB(2,3,2),RGB(3,3,2)/ 0.14, 0.56, 0.14/
      DATA RGB(1,4,2),RGB(2,4,2),RGB(3,4,2)/ 0.00, 1.00, 1.00/
      DATA RGB(1,1,3),RGB(2,1,3),RGB(3,1,3)/ 0.20, 0.56, 0.80/
      DATA RGB(1,2,3),RGB(2,2,3),RGB(3,2,3)/ 0.00, 0.00, 1.00/
      DATA RGB(1,3,3),RGB(2,3,3),RGB(3,3,3)/ 0.50, 0.00, 1.00/
      DATA RGB(1,4,3),RGB(2,4,3),RGB(3,4,3)/ 1.00, 0.00, 1.00/
      DATA RGB(1,1,4),RGB(2,1,4),RGB(3,1,4)/ 1.00, 1.00, 1.00/
      DATA RGB(1,2,4),RGB(2,2,4),RGB(3,2,4)/ 0.66, 0.66, 0.66/
      DATA RGB(1,3,4),RGB(2,3,4),RGB(3,3,4)/ 0.40, 0.40, 0.40/
      DATA RGB(1,4,4),RGB(2,4,4),RGB(3,4,4)/ 0.00, 0.00, 0.00/
C
      DATA TLAB(1,1)/'Tan'/
      DATA TLAB(2,1)/'Brown'/
      DATA TLAB(3,1)/'Orange'/
      DATA TLAB(4,1)/'Red'/
      DATA TLAB(1,2)/'Yellow'/
      DATA TLAB(2,2)/'Green'/
      DATA TLAB(3,2)/'Forest Green'/
      DATA TLAB(4,2)/'Cyan'/
      DATA TLAB(1,3)/'Sky Blue'/
      DATA TLAB(2,3)/'Blue'/
      DATA TLAB(3,3)/'Blue Magenta'/
      DATA TLAB(4,3)/'Magenta'/
      DATA TLAB(1,4)/'White'/
      DATA TLAB(2,4)/'Light Gray'/
      DATA TLAB(3,4)/'Dark Gray'/
      DATA TLAB(4,4)/'Black'/
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Use the Duplex character set of PLOTCHAR.
C
      CALL PCSETI('CD',1)
C
C  Define color indices and RGB labels..
C
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,1.,1.)
      DO 10 J=1,NY
      DO 20 I=1,NX
      CALL GSCR(IWKID,NX*(J-1)+I+1,RGB(1,I,J),RGB(2,I,J),RGB(3,I,J))
      WRITE(BLAB(I,J),500) RGB(1,I,J),RGB(2,I,J),RGB(3,I,J)
  500 FORMAT('R=',F4.2,' G=',F4.2,' B=',F4.2)
   20 CONTINUE
   10 CONTINUE
C
C  Draw the color squares and titles.
C
      DO 40 J=1,NY
      Y = Y0+(J-1)*(SPY+SZY)
      DO 30 I=1,NX
      X = SPX+(I-1)*(SPX+SZX)
      CALL DRBOX(X,Y,SZX,Y0,SZY,TLAB(I,J),BLAB(I,J),NX*(J-1)+I+1,IWKID)
   30 CONTINUE
   40 CONTINUE
C
C  Plot labels.
C
      CALL PLCHHQ(.5,.04,'The titles below each box indicate Red, Green
     1and Blue intensity values.',.012,0.,0.)
      CALL PCSETI('CD',1)
      CALL PLCHHQ(.5,.96,'Sixteen Sample Colors',
     1 .02,0.,0.)
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE DRBOX (X,Y,SZX,Y0,SZY,TLAB,BLAB,INDX,IWKID)
C
C  Draw a color square with lower left corner (X,Y)
C
      CHARACTER*(*) TLAB,BLAB
      DIMENSION  A(5),B(5)
C
      CALL GSFACI(INDX)
      CALL GSFAIS(1)
      A(1) = X
      B(1) = Y
      A(2) = X+SZX
      B(2) = Y
      A(3) = A(2)
      B(3) = Y+SZY
      A(4) = X
      B(4) = B(3)
      A(5) = A(1)
      B(5) = B(1)
      CALL GFA(4,A,B)
C
C  If the color is black, draw a boundary.
C
      CALL GQCR(IWKID,INDX,0,IER,CR,CG,CB)
      IF (CR.EQ.0. .AND. CG.EQ.0. .AND. CB.EQ.0.) THEN
        CALL GSPLCI(1)
        CALL GPL(5,A,B)
      ENDIF
C
      ILEN = LEN(TLAB)
      DO 10 K=ILEN,1,-1
      IF (TLAB(K:K) .NE. ' ') THEN
        ITLEN = K
        GO TO 20
      ENDIF
   10 CONTINUE
   20 CONTINUE
C
      ILEN = LEN(BLAB)
      DO 30 K=ILEN,1,-1
      IF (BLAB(K:K) .NE. ' ') THEN
        IBLEN = K
        GO TO 40
      ENDIF
   30 CONTINUE
   40 CONTINUE
C
      CALL GSPLCI(1)
      CALL PLCHHQ(X+.5*SZX,Y-.015,BLAB(1:IBLEN),.0098,0.,0.)
      CALL PLCHHQ(X+.5*SZX,Y+SZY+.017,TLAB(1:ITLEN),.012,0.,0.)
C
      RETURN
      END
