C
C	$Id: ardbda.f,v 1.1.1.1 1992-04-17 22:32:11 ncargd Exp $
C
C
C The subroutine ARDBDA.
C --- ---------- -------
C
      SUBROUTINE ARDBDA (X1,Y1,X2,Y2,IL,IR)
C
C The routine ARDBDA is called by ARDBPA, below, to draw an arrow from
C the point (X1,Y1) to the point (X2,Y2), in the fractional coordinate
C system.  The left and right area identifiers IL and IR are written
C in the proper positions relative to the arrow.  In order to prevent
C too many arrowheads from appearing, we keep track of the cumulative
C distance along edges being drawn (in DT).
C
      COMMON /ARCOM1/ DT
C
C Define character variables required to write the area identifiers.
C
      CHARACTER*6 CS
      CHARACTER*1 IC
C
C Draw the body of the arrow.
C
      CALL PLOTIF(X1,Y1,0)
      CALL PLOTIF(X2,Y2,1)
C
C Compute the length of the arrow.  If it's zero, quit.
C
      DX=X2-X1
      DY=Y2-Y1
      DP=SQRT(DX*DX+DY*DY)
C
      IF (DP.EQ.0.) RETURN
C
C If the area identifiers are in a reasonable range (less than 100,000
C in absolute value), write them on either side of the arrow.
C
      IF (.NOT.(ABS(IL).LT.100000.AND.ABS(IR).LT.100000)) GO TO 10001
C
        XC=.5*(X1+X2)
        YC=.5*(Y1+Y2)
        XL=XC-.004*DY/DP
        YL=YC+.004*DX/DP
        WRITE (CS,'(I6)') IL
        NC=0
        DO 101 I=1,6
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10002
          NC=NC+1
          CS(NC:NC)=IC
10002   CONTINUE
  101   CONTINUE
        CALL PLCHLQ (XL,YL,CS(1:NC),.001,0.,0.)
C
        XR=XC+.004*DY/DP
        YR=YC-.004*DX/DP
        WRITE (CS,'(I6)') IR
        NC=0
        DO 102 I=1,6
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10003
          NC=NC+1
          CS(NC:NC)=IC
10003   CONTINUE
  102   CONTINUE
        CALL PLCHLQ (XR,YR,CS(1:NC),.001,0.,0.)
C
10001 CONTINUE
C
C If the cumulative length of the edge being drawn is too little,
C quit; otherwise, put out an arrowhead.
C
      DT=DT+DP
      IF(DT.LE..008) RETURN
C
      DT=0.
      B=(DP-.008)/DP
      A=1.0-B
      XT=A*X1+B*X2
      YT=A*Y1+B*Y2
      X3=XT-.002*DY/DP
      Y3=YT+.002*DX/DP
      X4=XT+.002*DY/DP
      Y4=YT-.002*DX/DP
      CALL PLOTIF (X3,Y3,0)
      CALL PLOTIF (X2,Y2,1)
      CALL PLOTIF (X4,Y4,1)
C
C Done.
C
      RETURN
C
      END
