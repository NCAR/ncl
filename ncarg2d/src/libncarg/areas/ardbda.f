C
C $Id: ardbda.f,v 1.6 1994-03-16 23:11:11 kennison Exp $
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
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C Declare a local common block used to communicate with ARDBPA.
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
      IF (ICFELL('ARDBDA',1).NE.0) RETURN
      CALL PLOTIF(X2,Y2,1)
      IF (ICFELL('ARDBDA',2).NE.0) RETURN
C
C Compute the length of the arrow.  If it's zero, quit.
C
      DX=X2-X1
      DY=Y2-Y1
      DP=SQRT(DX*DX+DY*DY)
C
      IF (DP.EQ.0.) RETURN
C
C If area identifiers are to be written and they are in a reasonable
C range (less than 100,000 in absolute value), write them on either
C side of the arrow.
C
      IF (.NOT.(RDI.GT.0..AND.RSI.GT.0.AND.ABS(IL).LT.100000.AND.ABS(IR)
     +.LT.100000)) GO TO 10001
C
        XC=.5*(X1+X2)
        YC=.5*(Y1+Y2)
        XL=XC-RDI*DY/DP
        YL=YC+RDI*DX/DP
        WRITE (CS,'(I6)') IL
        NC=0
        DO 101 I=1,6
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10002
          NC=NC+1
          CS(NC:NC)=IC
10002   CONTINUE
  101   CONTINUE
        CALL PLCHLQ (XL,YL,CS(1:NC),RSI,0.,0.)
        IF (ICFELL('ARDBDA',3).NE.0) RETURN
C
        XR=XC+RDI*DY/DP
        YR=YC-RDI*DX/DP
        WRITE (CS,'(I6)') IR
        NC=0
        DO 102 I=1,6
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10003
          NC=NC+1
          CS(NC:NC)=IC
10003   CONTINUE
  102   CONTINUE
        CALL PLCHLQ (XR,YR,CS(1:NC),RSI,0.,0.)
        IF (ICFELL('ARDBDA',4).NE.0) RETURN
C
10001 CONTINUE
C
C If an arrowhead is to be drawn, do that now, making sure that the
C cumulative length of the edge being drawn is great enough.
C
      IF (.NOT.(RLA.GT.0..AND.RWA.GT.0.)) GO TO 10004
        DT=DT+DP
        IF(DT.LE.RLA) RETURN
        DT=0.
        B=(DP-RLA)/DP
        A=1.-B
        XT=A*X1+B*X2
        YT=A*Y1+B*Y2
        X3=XT-RWA*DY/DP
        Y3=YT+RWA*DX/DP
        X4=XT+RWA*DY/DP
        Y4=YT-RWA*DX/DP
        CALL PLOTIF (X3,Y3,0)
        IF (ICFELL('ARDBDA',5).NE.0) RETURN
        CALL PLOTIF (X2,Y2,1)
        IF (ICFELL('ARDBDA',6).NE.0) RETURN
        CALL PLOTIF (X4,Y4,1)
        IF (ICFELL('ARDBDA',7).NE.0) RETURN
10004 CONTINUE
C
C Done.
C
      RETURN
C
      END
