C
C	$Id: tspps.f,v 1.1.1.1 1992-04-17 22:34:35 ncargd Exp $
C
      PROGRAM TSPPS
C
C PURPOSE                To provide a simple demonstration of the more
C                        commonly used SPPS entries.
C
C I/O                    If the test is successful, the message
C
C              SPPS TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 9
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      SPPS
C
C REQUIRED GKS LEVEL     0A
C
      COMMON /BLOCK1/MESG,IDUMMY(500)
C
      IERR=0
C
C Use the SPPS entry to open GKS.
C
      CALL OPNGKS
C
C Check the non-plotting entries.
C
C
C SET and GETSET (SET is also tested below.)
C
      CALL SET   ( 0., 1., 0., 1.,0.,1.,10.,100.,    1)
      CALL GETSET(PXA,PXB,PYA,PYB,XC,XD, YC,  YD,LTYPE)
      IF (PXA.NE.0.0   .OR. PXB.NE.1.0 .OR.
     -    PYA.NE.0.0   .OR. PYB.NE.1.0 .OR.
     -    XC.NE.0.  .OR.  XD.NE.1.   .OR.
     -    YC.NE.10. .OR.  YD.NE.100. .OR. LTYPE.NE.1)
     -    GO TO 200
      WRITE (MESG,500)
      GO TO 210
  200 CONTINUE
      WRITE (MESG,510)
      STOP
  210 CONTINUE
C
C FL2INT
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL FL2INT(0.,0.,MX,MY)
      IF (MX.NE.0 .OR. MY.NE.0) GO TO 221
C
      CALL FL2INT(1.,1.,MX,MY)
      IF (MX.NE.32767 .OR. MY.NE.32767) GO TO 221
C
      CALL FL2INT(.5,.5,MX,MY)
      IF (MX.NE.16383 .OR. MY.NE.16383) GO TO 221
C
      WRITE(MESG,520)
      GO TO 220
  221 CONTINUE
C
      WRITE(MESG,530)
      IERR=1
  220 CONTINUE
C
C SETI and GETSI
C
      CALL SETI(13,5)
      CALL GETSI(LX,LY)
C
      IF (LX.NE.13 .OR. LY.NE.5) THEN
      WRITE(MESG,550)
      IERR=1
      GO TO 230
      END IF
C
      WRITE(MESG,540)
      CALL SETI(10,10)
  230 CONTINUE
C
C FRSTPT and MXMY
C
      CALL FRSTPT(.5,.5)
      CALL MXMY(MX,MY)
C
      IF (MX.NE.512 .OR. MY.NE.512) THEN
      WRITE(MESG,570)
      IERR=1
      GO TO 240
      END IF
C
      WRITE(MESG,560)
  240 CONTINUE
C
C Test plotting entries.
C
      CALL TWTSTR
      CALL TPLOTI
      CALL TLINE
      CALL TCURVE
      CALL TVECTO
      CALL TSET
      CALL TPOINT
      CALL TPNTS
C
C Use the SPPS entry to close GKS.
C
      CALL CLSGKS
C
      WRITE(6,600)
  600 FORMAT(' SPPS TEST EXECUTED--SEE PLOTS TO CERTIFY')
      STOP
C
  500 FORMAT(' SET AND GETSET TEST  SUCCESSFUL')
  510 FORMAT(' SET AND GETSET TEST  NOT SUCCESSFUL')
  520 FORMAT(' FL2INT TEST          SUCCESSFUL')
  530 FORMAT(' FL2INT TEST          NOT SUCCESSFUL')
  540 FORMAT(' SETI AND GETSI TEST  SUCCESSFUL')
  550 FORMAT(' SETI AND GETSI TEST  NOT SUCCESSFUL')
  560 FORMAT(' FRSTPT AND MXMY TEST SUCCESSFUL')
  570 FORMAT(' FRSTPT AND MXMY TEST NOT SUCCESSFUL')
C
      END
