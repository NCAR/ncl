C
C $Id: stitle.f,v 1.3 1993-01-15 23:30:43 kennison Exp $
C
      SUBROUTINE STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MV)
C
      CHARACTER*(*) CARDS(NCARDS)
      CHARACTER*512 CTMP
      INTEGER SLSTLN
C
      DIMENSION CRECT(4),XE(4),YE(4)
C
C The labeled common block SLCOMN holds all of the internal parameters
C for the STITLE package.
C
      COMMON /SLCOMN/ ICU,ICO,PCHSZ,GAPSZ,T1,T2,NXST,NXFIN,ICRTJP,
     +                LIM(4),IBKG,LND,BGCLR(3),FGCLR(3),IFST,IWK,FIN,
     +                FOU,ISPB,ISPF,IDEN,IWU,IMAP,OORV
      SAVE   /SLCOMN/
C
C Declare the common block in which STITLE communicates to PLOTCHAR the
C coordinates of the viewport outside which scrolled titles are to be
C clipped.
C
      COMMON /PCSTCM/ XVPL,XVPR,YVPB,YVPT
      SAVE   /PCSTCM/
C
C Declare the block data routine SLBLDA external to force it to load,
C so that the internal parameters will be initialized.
C
      EXTERNAL SLBLDA
C
C Preset local variables.
C
      DATA IORNT  / 0 /
      DATA XE(1),XE(2),XE(3),XE(4)/0.0,1.0,1.0,0.0/
      DATA YE(1),YE(2),YE(3),YE(4)/0.0,0.0,1.0,1.0/
C
C Initialize variables if this is the first user call.
C
      IF (IFST .EQ. 0) THEN
        CALL SLINIT
        IFST = 1
      ENDIF
C
C If the card buffer is empty, there is nothing to do.
C
      IF (NCARDS .LE. 0) RETURN
C
C Find out how long each "card image" is.
C
      LCRD = LEN(CARDS(1))
C
C Open WISS for GFLASH.
C
      CALL GOPWK(IDEN,IWU,3)
C
C Tell PLOTCHAR where the clipping rectangle is.
C
      XVPL = REAL(LIM(1))/32767.
      XVPR = REAL(LIM(2))/32767.
      YVPB = REAL(LIM(3))/32767.
      YVPT = REAL(LIM(4))/32767.
C
C Set internal parameters required to make PLOTCHAR do clipping (saving
C the original values for later restoration).
C
      CALL PCGETI('MA',ISMA)
      CALL PCGETR('OR',RSOR)
C
      IF (IMAP.GT.0 ) CALL PCSETI('MA',IMAP)
      IF (OORV.NE.0.) CALL PCSETR('OR',OORV)
C
C Save current GKS clipping indicator, and set clipping off.  All
C necessary clipping is currently done by PLOTCHAR.
C
      CALL GQCLIP(IER,ICLP,CRECT)
      CALL GSCLIP(0)
C
C Save current grid size, and set to 1024.
C
      CALL GETSI (IXSC,IYSC)
      CALL SETI  (  10,  10)
C
C Save current normalization number and set to normalization
C transformation 0.
C
      CALL GQCNTN(IER,NTOLD)
      CALL GSELNT(0)
C
C If in production mode, dot the corners of the first and last frames
C (for splicing purposes).
C
      IF (MV.EQ.0 .AND. LND.EQ.1) THEN
        CALL GQCR(IWK,1,0,IER,RR,GG,BB)
        CALL GSCR(IWK,1,1.,1.,1.)
        CALL GSPMCI(1)
        CALL GPM(4,XE,YE)
        CALL FRAME
        IF (IER .EQ. 0) CALL GSCR(IWK,1,RR,GG,BB)
      ENDIF
C
C ISCRD1 is the X-coordinate of the center of the window.
C
      ISCRD1 = (LIM(1)+LIM(2))/64
C
C ISCRD2 is the Y-coordinate of the center of the window.
C
      ISCRD2 = (LIM(3)+LIM(4))/64
C
C ISCRD3 is half the height of the window.
C
      ISCRD3 = (LIM(4)-LIM(3))/64
C
C Initial picture.
C
      IF (FIN .NE. 0.) THEN
C
        NREP = INT(FIN*24.+.5)
        IF (MV .NE. 0) NREP = 1
C
        DO 104 IREP=1,NREP
C
C Plot background.
C
          IF (ISPB .EQ. 0) THEN
            CALL RGBHSV (BGCLR(1),BGCLR(2),BGCLR(3),HH,SS,VV)
            VV = REAL(IREP)*VV/REAL(NREP)
            CALL HSVRGB (HH,SS,VV,RR,GG,BB)
            CALL GSCR(IWK,IBKG,RR,GG,BB)
          ELSE
            RR = 1.
            CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
          ENDIF
C
          IF (RR.GT.0.01 .OR. GG.GT.0.01 .OR. BB.GT.0.01) CALL SLBKGD
C
C Set foreground color.
C
          IF (ISPF .EQ. 0) THEN
            CALL RGBHSV (FGCLR(1),FGCLR(2),FGCLR(3),HH,SS,VV)
            VV = REAL(IREP)*VV/REAL(NREP)
            CALL HSVRGB (HH,SS,VV,RR,GG,BB)
            CALL GSCR(IWK,1,RR,GG,BB)
          ELSE
            CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
          ENDIF
C
C Plot characters.
C
          CALL SLUBKG (-1)
C
          DO 103 I=1,NCARDS
            READ (CARDS(I)(1:20),'(3I5,F5.1)') IXPOS,IYPOS,ICNTR,SIZE
            IF (IXPOS .EQ. -9999) GO TO 103
            NUMCHR = MIN(512,SLSTLN(CARDS(I)(21:LCRD)))
            CTMP = CARDS(I)(21:20+NUMCHR)
            IF (I.NE.NCARDS) THEN
              DO 101 J=I+1,NCARDS
                IF (CARDS(J)(1:5) .NE. '-9999') GO TO 102
                NOACHR = MIN(512-NUMCHR,SLSTLN(CARDS(J)(21:LCRD)))
                IF (NOACHR.NE.0) THEN
                  CTMP(NUMCHR+1:NUMCHR+NOACHR) = CARDS(J)(21:20+NOACHR)
                  NUMCHR = NUMCHR+NOACHR
                END IF
  101         CONTINUE
            END IF
  102       XPOS = CPUX(IXPOS-NXST+ISCRD1)
            YPOS = CPUY(IYPOS-NYST+ISCRD2)
            CNTR = REAL(ICNTR-1)
            PSZ = (6./7.)*SIZE*PCHSZ/1023.
            CALL PLCHHQ(XPOS,YPOS,CTMP(1:NUMCHR),PSZ,REAL(IORNT),CNTR)
  103     CONTINUE
C
          CALL SLUBKG (+1)
C
          IF (MV .NE. 0) THEN
            CALL SLOWND
            CTMP = ' '
            WRITE(CTMP(1:8),'(F8.2)') FIN
            CTMP(9:24) = ' SECS OF FADE IN'
            CALL SLPWRV (CPUX(1012),CPUY(848),CTMP,24,2,0,0)
          END IF
C
          CALL FRAME
C
  104   CONTINUE
C
      END IF
C
      IF (TST .NE. 0.) THEN
C
        CALL GFLAS1(IDEN)
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
C
        CALL SLUBKG (-2)
C
        DO 107 I=1,NCARDS
          IF (IBKG.NE.0 .AND. I.EQ.1) CALL SLBKGD
          READ (CARDS(I)(1:20),'(3I5,F5.1)') IXPOS,IYPOS,ICNTR,SIZE
          IF (IXPOS .EQ. -9999) GO TO 107
          NUMCHR = MIN(512,SLSTLN(CARDS(I)(21:LCRD)))
          CTMP = CARDS(I)(21:20+NUMCHR)
          IF (I.NE.NCARDS) THEN
            DO 105 J=I+1,NCARDS
              IF (CARDS(J)(1:5) .NE. '-9999') GO TO 106
              NOACHR = MIN(512-NUMCHR,SLSTLN(CARDS(J)(21:LCRD)))
              IF (NOACHR.NE.0) THEN
                CTMP(NUMCHR+1:NUMCHR+NOACHR) = CARDS(J)(21:20+NOACHR)
                NUMCHR = NUMCHR+NOACHR
              END IF
  105       CONTINUE
          END IF
  106     XPOS = CPUX(IXPOS-NXST+ISCRD1)
          YPOS = CPUY(IYPOS-NYST+ISCRD2)
          CNTR = REAL(ICNTR-1)
          PSZ = (6./7.)*SIZE*PCHSZ/1023.
          CALL PLCHHQ(XPOS,YPOS,CTMP(1:NUMCHR),PSZ,REAL(IORNT),CNTR)
  107   CONTINUE
C
        CALL SLUBKG (+2)
C
        CALL GFLAS2
C
        NREP = INT(TST*24.+.5)
        IF (MV .NE. 0) NREP = 1
C
        DO 108 I=1,NREP
          CALL GFLAS3 (IDEN)
          IF (MV .NE. 0) THEN
            CALL SLOWND
            CTMP = ' '
            WRITE(CTMP(1:8),'(F8.2)') TST
            CTMP(9:22) = ' SECS AT START'
            CALL SLPWRV (CPUX(1012),CPUY(848),CTMP,22,2,0,0)
          END IF
          CALL FRAME
  108   CONTINUE
C
      END IF
C
C Moving portion.  Note the inclusion of a test to prevent PLCHHQ from
C being called if it is not possible that any portion of the line is
C visible in the viewport.
C
      IF (TMV .NE. 0.) THEN
C
        TFRAC = 0.
        NDIFFR = INT(TMV*24.+.5)
        IF (MV .NE. 0) NDIFFR = MAX0(IABS(NYFIN-NYST)/ICRTJP,1)
        TPER = TMV/FLOAT(NDIFFR)
        DIFFX = FLOAT(NXFIN-NXST)/FLOAT(NDIFFR)
        DIFFY = FLOAT(NYFIN-NYST)/FLOAT(NDIFFR)
        NDIFFR = NDIFFR+1
C
        DO 112 K=1,NDIFFR
          FKM1 = REAL(K-1)
          CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
          CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
          LX = ISCRD1-NXST-INT(FKM1*DIFFX)
          LY = ISCRD2-NYST-INT(FKM1*DIFFY)
C
          CALL SLUBKG (-3)
C
          DO 111 I=1,NCARDS
            IF (IBKG.NE.0 .AND. I.EQ.1) CALL SLBKGD
            READ (CARDS(I)(1:20),'(3I5,F5.1)') IXPOS,IYPOS,ICNTR,SIZE
            IF (IXPOS .EQ. -9999) GO TO 111
            NUMCHR = MIN(512,SLSTLN(CARDS(I)(21:LCRD)))
            CTMP = CARDS(I)(21:20+NUMCHR)
            IF (I.NE.NCARDS) THEN
              DO 109 J=I+1,NCARDS
                IF (CARDS(J)(1:5) .NE. '-9999') GO TO 110
                NOACHR = MIN(512-NUMCHR,SLSTLN(CARDS(J)(21:LCRD)))
                IF (NOACHR.NE.0) THEN
                  CTMP(NUMCHR+1:NUMCHR+NOACHR) = CARDS(J)(21:20+NOACHR)
                  NUMCHR = NUMCHR+NOACHR
                END IF
  109         CONTINUE
            END IF
  110       MX = IXPOS+LX
            MY = IYPOS+LY
            XPOS = CPUX(MX)
            YPOS = CPUY(MY)
            CNTR = REAL(ICNTR-1)
            PSZ = (6./7.)*SIZE*PCHSZ/1023.
            MYCRIT = INT(SIZE*PCHSZ)
            MYDIS = IABS(MY-ISCRD2)
            IF (IMAP.NE.100.OR.OORV.NE.1.E12.OR.MYDIS.LT.ISCRD3+MYCRIT)
     +      CALL PLCHHQ(XPOS,YPOS,CTMP(1:NUMCHR),PSZ,REAL(IORNT),CNTR)
  111     CONTINUE
C
          CALL SLUBKG (+3)
C
          IF (MV .NE. 0) THEN
            CALL SLOWND
            CTMP = ' '
            WRITE(CTMP(1:8),'(F8.2)') TFRAC
            CTMP(9:28) = ' SECS INTO MOVE TIME'
            CALL SLPWRV (CPUX(1012),CPUY(848),CTMP,28,2,0,0)
            TFRAC = TFRAC+TPER
          END IF
C
          CALL FRAME
C
  112   CONTINUE
C
      END IF
C
C Last picture.
C
      IF (TFIN .NE. 0.) THEN
C
        CALL GFLAS1 (IDEN)
        CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
        CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
C
        CALL SLUBKG (-4)
C
        DO 115 I=1,NCARDS
          IF (IBKG.NE.0 .AND. I.EQ.1) CALL SLBKGD
          READ (CARDS(I)(1:20),'(3I5,F5.1)') IXPOS,IYPOS,ICNTR,SIZE
          IF (IXPOS .EQ. -9999) GO TO 115
          NUMCHR = MIN(512,SLSTLN(CARDS(I)(21:LCRD)))
          CTMP = CARDS(I)(21:20+NUMCHR)
          IF (I.NE.NCARDS) THEN
            DO 113 J=I+1,NCARDS
              IF (CARDS(J)(1:5) .NE. '-9999') GO TO 114
              NOACHR = MIN(512-NUMCHR,SLSTLN(CARDS(J)(21:LCRD)))
              IF (NOACHR.NE.0) THEN
                CTMP(NUMCHR+1:NUMCHR+NOACHR) = CARDS(J)(21:20+NOACHR)
                NUMCHR = NUMCHR+NOACHR
              END IF
  113       CONTINUE
          END IF
  114     XPOS = CPUX(IXPOS-NXFIN+ISCRD1)
          YPOS = CPUY(IYPOS-NYFIN+ISCRD2)
          CNTR = REAL(ICNTR-1)
          PSZ = (6./7.)*SIZE*PCHSZ/1023.
          CALL PLCHHQ(XPOS,YPOS,CTMP(1:NUMCHR),PSZ,REAL(IORNT),CNTR)
  115   CONTINUE
C
        CALL SLUBKG (+4)
C
        CALL GFLAS2
C
        NREP = INT(TFIN*24.+.5)
        IF (MV .NE. 0) NREP = 1
C
        DO 116 I=1,NREP
          CALL GFLAS3 (IDEN)
          IF (MV .NE. 0) THEN
            CALL SLOWND
            CTMP = ' '
            WRITE(CTMP(1:8),'(F8.2)') TFIN
            CTMP(9:20) = ' SECS AT END'
            CALL SLPWRV (CPUX(1012),CPUY(848),CTMP,20,2,0,0)
          END IF
          CALL FRAME
  116   CONTINUE
C
      END IF
C
C Fade out.
C
      IF (FOU .NE. 0.) THEN
C
        NREP = INT(FOU*24.+.5)
        IF (MV .NE. 0) NREP = 1
C
        DO 120 IREP=1,NREP
C
C Plot background.
C
          IF (ISPB .EQ. 0) THEN
            CALL RGBHSV (BGCLR(1),BGCLR(2),BGCLR(3),HH,SS,VV)
            VV = REAL(NREP-IREP+1)*VV/REAL(NREP)
            CALL HSVRGB (HH,SS,VV,RR,GG,BB)
            CALL GSCR(IWK,IBKG,RR,GG,BB)
          ELSE
            RR = 1.
            CALL GSCR(IWK,IBKG,BGCLR(1),BGCLR(2),BGCLR(3))
          ENDIF
C
          IF (RR.GT.0.01 .OR. GG.GT.0.01 .OR. BB.GT.0.01) CALL SLBKGD
C
C Set foreground color.
C
          IF (ISPF .EQ. 0) THEN
            CALL RGBHSV (FGCLR(1),FGCLR(2),FGCLR(3),HH,SS,VV)
            VV = REAL(NREP-IREP+1)*VV/REAL(NREP)
            CALL HSVRGB (HH,SS,VV,RR,GG,BB)
            CALL GSCR(IWK,1,RR,GG,BB)
          ELSE
            CALL GSCR(IWK,1,FGCLR(1),FGCLR(2),FGCLR(3))
          ENDIF
C
C Plot characters.
C
          CALL SLUBKG (-5)
C
          DO 119 I=1,NCARDS
            READ (CARDS(I)(1:20),'(3I5,F5.1)') IXPOS,IYPOS,ICNTR,SIZE
            IF (IXPOS .EQ. -9999) GO TO 119
            NUMCHR = MIN(512,SLSTLN(CARDS(I)(21:LCRD)))
            CTMP = CARDS(I)(21:20+NUMCHR)
            IF (I.NE.NCARDS) THEN
              DO 117 J=I+1,NCARDS
                IF (CARDS(J)(1:5) .NE. '-9999') GO TO 118
                NOACHR = MIN(512-NUMCHR,SLSTLN(CARDS(J)(21:LCRD)))
                IF (NOACHR.NE.0) THEN
                  CTMP(NUMCHR+1:NUMCHR+NOACHR) = CARDS(J)(21:20+NOACHR)
                  NUMCHR = NUMCHR+NOACHR
                END IF
  117         CONTINUE
            END IF
  118       XPOS = CPUX(IXPOS-NXFIN+ISCRD1)
            YPOS = CPUY(IYPOS-NYFIN+ISCRD2)
            CNTR = REAL(ICNTR-1)
            PSZ = (6./7.)*SIZE*PCHSZ/1023.
            CALL PLCHHQ(XPOS,YPOS,CTMP(1:NUMCHR),PSZ,REAL(IORNT),CNTR)
  119     CONTINUE
C
          CALL SLUBKG (+5)
C
          IF (MV .NE. 0) THEN
            CALL SLOWND
            CTMP = ' '
            WRITE(CTMP(1:8),'(F8.2)') FOU
            CTMP(9:25) = ' SECS OF FADE OUT'
            CALL SLPWRV (CPUX(1012),CPUY(848),CTMP,25,2,0,0)
          END IF
C
          CALL FRAME
C
  120   CONTINUE
C
      END IF
C
C Dot corners of frame.
C
      IF (MV.EQ.0 .AND. LND.EQ.1) THEN
        CALL GQCR(IWK,1,0,IER,RR,GG,BB)
        CALL GSCR(IWK,1,1.,1.,1.)
        CALL GSPMCI(1)
        CALL GPM(4,XE,YE)
        CALL FRAME
        IF (IER .EQ. 0) CALL GSCR(IWK,1,RR,GG,BB)
      ENDIF
C
C Close WISS.
C
      CALL GCLWK(IDEN)
C
C Restore the values of internal parameters of PLOTCHAR that have been
C changed.
C
      CALL PCSETI('MA',ISMA)
      CALL PCSETR('OR',RSOR)
C
C Restore original settings of various GKS and SPPS parameters.
C
      CALL GSCLIP(ICLP)
      CALL SETI  (IXSC,IYSC)
      CALL GSELNT(NTOLD)
C
      RETURN
C
      END
