C
C $Id: set.f,v 1.3 1994-03-17 01:44:15 kennison Exp $
C
      SUBROUTINE SET (VL,VR,VB,VT,WL,WR,WB,WT,LF)
C
C SET allows the user to change the current values of the parameters
C defining the mapping from the user system to the fractional system
C (in GKS terminology, the mapping from world coordinates to normalized
C device coordinates).
C
C VL, VR, VB, and VT define the viewport (in the fractional system), WL,
C WR, WB, and WT the window (in the user system), and LF the nature of
C the mapping, according to the following table:
C
C    1  -  x linear, y linear
C    2  -  x linear, y logarithmic
C    3  -  x logarithmic, y linear
C    4  -  x logarithmic, y logarithmic
C
C Declare the common block containing the linear-log and mirror-imaging
C flags.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Set the GKS viewport for transformation 1.
C
      CALL GSVP (1,VL,VR,VB,VT)
C
C Set the utility state variable controlling linear-log mapping.
C
      LL=MAX0(1,MIN0(4,LF))
C
C Set the GKS window for transformation 1.
C
      IF (WL.LT.WR) THEN
        MI=1
        QL=WL
        QR=WR
      ELSE
        MI=3
        QL=WR
        QR=WL
      END IF
C
      IF (WB.LT.WT) THEN
        QB=WB
        QT=WT
      ELSE
        MI=MI+1
        QB=WT
        QT=WB
      END IF
C
      IF (LL.EQ.1) THEN
        CALL GSWN (1,QL,QR,QB,QT)
      ELSE IF (LL.EQ.2) THEN
        CALL GSWN (1,QL,QR,ALOG10(QB),ALOG10(QT))
      ELSE IF (LL.EQ.3) THEN
        CALL GSWN (1,ALOG10(QL),ALOG10(QR),QB,QT)
      ELSE
        CALL GSWN (1,ALOG10(QL),ALOG10(QR),ALOG10(QB),ALOG10(QT))
      END IF
C
C Select transformation 1 as the current one.
C
      CALL GSELNT (1)
C
      RETURN
C
      END
