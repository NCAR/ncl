C
C	$Id: gztowk.f,v 1.20 2010-02-08 06:01:29 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZTOWK
C
C  This subroutine invokes the workstation drivers depending
C  on the current GKS operating state. 
C
      include 'gkscom.h'
C
      PARAMETER (IADIM=1024)
      INTEGER FCODEO,CONTO,XID,ADESTR(IADIM),ICNTX(31)
      REAL    RCNTX(19)
      SAVE
C
      CALL GQOPS(IST)
C
C  Invoke workstation drivers for GKCL functions and OPEN WORKSTATION.
C
      IF (IST .GE. GGKCL) THEN
C
C  Picture name.
C
        IF (FCODE .EQ. 92) THEN
          CALL G01WDR(ID(1),GFNAME)
          RETURN
        ELSE IF (FCODE.EQ.-3 .OR. FCODE.EQ.-4) THEN
          IF (ID(3) .EQ. GCGM) THEN
            CALL G01WDR(ID(1),GFNAME)
            RETURN
          ELSE IF (ID(3).EQ.GXWC   .OR.   ID(3).EQ.GXWE    .OR. 
     +             ID(3).EQ.GDMP   .OR.   ID(3).EQ.GPIX    .OR.
     +            (ID(3).GE.GPSMIN .AND.  ID(3).LE.GPSMAX) .OR.
     +             ID(3).EQ.GPDFP  .OR.   ID(3).EQ.GPDFL   .OR.
     +            (ID(3).GE.GCROMIN .AND.  ID(3).LE.GCROMAX) )THEN
            DO 200 J=1,IADIM
              ADESTR(J) = 0
  200       CONTINUE
            INMLEN = LEN(GFNAME)
            DO 80 J=1,INMLEN
              ADESTR(J) = ICHAR(GFNAME(J:J))
   80       CONTINUE
            STRL1 = INMLEN
            STRL2 = INMLEN
            IID   = ID(1)
            ID(1) = ID(2)
            ID(2) = ID(3)
            ID(3) = ID(4)
            ID(4) = ID(5)
            ID(5) = ID(6)
            ID(6) = ID(7)
            ID(7) = ID(8)
            ID(8) = ID(9)
            ID(9) = ID(10)
            ID(10) = ID(11)
            ID(11) = ID(12)
            ID(12) = ID(13)
            ID(13) = ID(14)
            ID(14) = ID(15)
            NUMP  = 12
            CALL GGKWDR(IID,FCODE,0,NUMP,IL2,ID,IC1,IC2,IC,
     -                  RL1,RL2,RX,RY,STRL1,STRL2,ADESTR,RERR,XERMSG)
            IF (RERR .LE. -100) RETURN
          ELSE IF (ID(3) .EQ. GWSS) THEN
            CALL GZSRAT(3,ICNTX,RCNTX)
            CALL GWIWDR(ICNTX,RCNTX)
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
C  Return if no workstations are open.
C
      IF (IST .EQ. GGKOP) RETURN
C
C  Treat the WSOP functions when there is at least one open workstation.
C
      IF (IST .GE. GWSOP) THEN
        DO 10 I=1,NOPWK
C
C  Put out new picture initialization if metafile and the picture 
C  is empty.
C
          IF (SWKTP(I) .EQ. GCGM) THEN 
C
C  If CUFLAG is set, make the interface call only for the specific
C  workstation.
C
            IF (CUFLAG.GE.0 .AND. SOPWK(I).NE.CUFLAG) GO TO 100
C           IF (IST.GE.2  .AND. NOPICT.LE.0 .AND. 
C    -        (FCODE.EQ.-2 .OR.  FCODE.EQ.56  .OR. FCODE.EQ.61  .OR.
C    -         FCODE.EQ.82 .OR. (FCODE.GE.11 .AND. FCODE.LE.16) .OR.
C    -         (FCODE.GE.21.AND.FCODE.LE.43))) THEN
            IF (IST.GE.2  .AND. NOPICT.LE.0 .AND. 
     -          (FCODE.EQ.82 .OR. (FCODE.GE.11 .AND. FCODE.LE.16))) THEN       
              FCODEO = FCODE
              CONTO  = CONT
              FCODE  = 91
              CONT   =  0
              CALL G01WDR(SOPWK(I),' ')
              FCODE  = FCODEO
              CONT   = CONTO
              NOPICT = 1
            ENDIF
  100       CONTINUE
          ENDIF
C
C  Put functions out to the open workstations.
C
          IF ( 
     +     (FCODE.EQ. -6 .OR. FCODE.EQ. -5 .OR.
     +      FCODE.EQ. -2 .OR. FCODE.EQ.  0 .OR. FCODE.EQ.  1 .OR.
     +      FCODE.EQ.  3 .OR. FCODE.EQ.  6 .OR. FCODE.EQ. 61 .OR.
     +      FCODE.EQ. 56 .OR. FCODE.EQ. 71 .OR. FCODE.EQ. 72 .OR.
     +      FCODE.EQ. 79 .OR. FCODE.EQ. 82 .OR. FCODE.EQ. 84 .OR. 
     +      FCODE.EQ.102 .OR. FCODE.EQ.103 .OR. FCODE.EQ.104 .OR. 
     +      FCODE.EQ. 92 .OR. 
     +     (FCODE.GE. 21.AND. FCODE.LE.43))) THEN
            IF (SWKTP(I) .EQ. GCGM) THEN
C
C  If CUFLAG is set, make the interface call only for the specific
C  workstation.  FCODE of 79 applies only to WISS.
C
              IF (FCODE .EQ. 79) GO TO 10
              IF (CUFLAG.GE.0 .AND. SOPWK(I).NE.CUFLAG) GO TO 50
              CALL G01WDR(SOPWK(I),' ')
   50         CONTINUE
            ELSE IF (SWKTP(I).EQ.GXWC  .OR. SWKTP(I).EQ.GXWE .OR.
     +               SWKTP(I).EQ.GDMP  .OR. SWKTP(I).EQ.GPIX .OR.
     +              (SWKTP(I).GE.GPSMIN .AND. SWKTP(I).LE.GPSMAX) .OR.
     +               SWKTP(I).EQ.GPDFP .OR. SWKTP(I).EQ.GPDFL .OR.
     +         (SWKTP(I).GE.GCROMIN .AND. SWKTP(I).LE.GCROMAX) ) THEN
C
C  If CUFLAG is set, make the interface call only for the specific
C  workstation.
C
              IF (FCODE .EQ. 79) GO TO 10
              IF (CUFLAG.GE.0 .AND. SOPWK(I).NE.CUFLAG) GO TO 10 
C
C  Get the local workstation ID and convert characters to ADE 
C  before invoking the interface.
C
              CALL GZXID(SOPWK(I),XID,RERR)
              IF (RERR .NE. 0) GO TO 55
C
              IF (STRL2 .GT. 0) THEN
                DO 210 J=1,IADIM
                  ADESTR(J) = 0
  210           CONTINUE
                DO 30 J=1,STRL2
                  ADESTR(J) = ICHAR(STR(J:J))
   30           CONTINUE
              ENDIF
C
              CALL GGKWDR(XID,FCODE,CONT,IL1,IL2,ID,IC1,IC2,IC,
     -                    RL1,RL2,RX,RY,STRL1,STRL2,ADESTR,RERR,XERMSG)
            ELSE IF (SWKTP(I) .EQ. GWSS) THEN
              IF (FCODE .EQ. 0) THEN
                IF (CUFLAG.GE.0 .AND. SOPWK(I).NE.CUFLAG) GO TO 10
                CALL GZSRAT(3,ICNTX,RCNTX)
                CALL GWIWDR(ICNTX,RCNTX)
              ELSE IF (FCODE .EQ. 79) THEN
                CALL GWIWDR(ICNTX,RCNTX)
              ENDIF
            ENDIF
   55       CONTINUE
          ENDIF
   10   CONTINUE
      ENDIF
C
C  Put functions out to the active workstations.
C
      IF (IST .GE. GWSAC) THEN
        DO 20 I=1,NACWK
          IF (
     +      FCODE.EQ.-1  .OR. FCODE.EQ.93   .OR. FCODE.EQ.101 .OR.
     +     (FCODE.GE.11 .AND. FCODE.LE.16)) THEN
C
C  Determine the workstation type.
C
            DO 70 J=1,NOPWK
              IF (SACWK(I) .EQ. SOPWK(J)) THEN
                ITYP = SWKTP(J)
                GO TO 90
              ENDIF
   70       CONTINUE
   90       CONTINUE
C
            IF (ITYP .EQ. GCGM) THEN
C
C  If CUFLAG is set, make the interface call only for the specific
C  workstation.
C
              IF (CUFLAG.GE.0 .AND. SACWK(I).NE.CUFLAG) GO TO 60
              CALL G01WDR(SACWK(I),' ')
   60         CONTINUE
            ELSE IF (ITYP.EQ.GXWC  .OR. ITYP.EQ.GXWE .OR.
     +               ITYP.EQ.GDMP  .OR. ITYP.EQ.GPIX .OR.
     +              (ITYP.GE.GPSMIN .AND. ITYP.LE.GPSMAX) .OR.
     +               ITYP.EQ.GPDFP .OR. ITYP.EQ.GPDFL .OR.
     +            (ITYP.GE.GCROMIN .AND. ITYP.LE.GCROMAX) ) THEN
C
C  If CUFLAG is set, make the interface call only for the specific
C  workstation.
C
              IF (CUFLAG.GE.0 .AND. SACWK(I).NE.CUFLAG) GO TO 20
C
C  Get the local workstation ID and convert characters to ADE
C  before invoking the interface.
C
              CALL GZXID(SACWK(I),XID,RERR)
              IF (RERR .NE. 0) GO TO 65
              IF (STRL2 .GT. 0) THEN
                DO 220 J=1,IADIM
                  ADESTR(J) = 0
  220           CONTINUE
                DO 40 J=1,STRL2
                  ADESTR(J) = ICHAR(STR(J:J))
   40           CONTINUE
              ENDIF
              CALL GGKWDR(XID,FCODE,CONT,IL1,IL2,ID,IC1,IC2,IC,
     +                    RL1,RL2,RX,RY,STRL1,STRL2,ADESTR,RERR,XERMSG)
            ENDIF
   65       CONTINUE
          ENDIF
   20   CONTINUE
      ENDIF
C
C  Store segments in WISS.
C
      IF (IST .EQ. GSGOP) THEN
C
C  Open segment name if create segment (GKS was put in state GSGOP
C  by GCRSG before the interface call); flush and close if close segment.      
C
        IF (FCODE.EQ.80 .OR. FCODE.EQ.81) THEN
          CALL GZSRAT(3,ICNTX,RCNTX)
          CALL GWIWDR(ICNTX,RCNTX)
          RETURN
        ENDIF
C
C  Determine if WISS is open, send out appropriate elements if so.
C
        DO 110 I=1,NOPWK
          IF (SWKTP(I) .EQ. GWSS) THEN
            IF (FCODE.EQ. 6 .OR.  FCODE.EQ.61 .OR.
     +         (FCODE.GE.21 .AND. FCODE.LE.43)) THEN          
              CALL GZSRAT(3,ICNTX,RCNTX)
              CALL GWIWDR(ICNTX,RCNTX)
              RETURN
            ENDIF
          ENDIF
  110   CONTINUE
C
C  Determine if WISS is active, send out appropriate elements if so.
C
        DO 130 I=1,NACWK
C
          DO 170 J=1,NOPWK
            IF (SACWK(I) .EQ. SOPWK(J)) THEN
              ITYP = SWKTP(J)
              GO TO 190
            ENDIF
  170     CONTINUE
  190     CONTINUE
C
          IF (ITYP .EQ. GWSS) THEN
            IF (FCODE.GE.11 .AND. FCODE.LE.16) THEN
              CALL GZSRAT(3,ICNTX,RCNTX)
              CALL GWIWDR(ICNTX,RCNTX)
              RETURN
            ENDIF
          ENDIF
  130   CONTINUE
      ENDIF
C
      RETURN
      END
