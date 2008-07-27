C
C	$Id: wmrgwt.f,v 1.9 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMRGWT(N,X,Y,IFNT,NASC)
C
C  Fill the region given by coordinates (X(I),Y(I),I=1,N) with
C  character ICHR (an ASCII decimal equivalent) from font IFNT.
C
      include 'wmcomn.h'
C
C  The value of IDRFLG control how the clipped symbols are drawn.
C  The area is filled and then a representative line is drawn so
C  that the symbols do not disappear on low-resolution devices.
C
      COMMON /WMLGCM/IDRFLG
C
C  Parameters for apportioning work space.
C
      PARAMETER (MDGU=7000,MDGUH=MDGU/2,MDGUP1=MDGU+1,
     +           MDGUP=MDGU+MDGUH+1,MDGU2=2*MDGU,MDGU2P=MDGU2+1)
      DIMENSION X(N),Y(N)
      EXTERNAL WMLGFA
C
C  Check error status.
C
      IF (ICFELL('WMRGWT - Uncleared prior error',1) .NE. 0) RETURN
C
C  Find the extent of the specified region.
C
      XRL = X(1)
      XRR = X(1)
      YRB = Y(1)
      YRT = Y(1)
      DO 10 I=2,N
        XRL = MIN(XRL,X(I))
        YRB = MIN(YRB,Y(I))
        XRR = MAX(XRR,X(I))
        YRT = MAX(YRT,Y(I))
   10 CONTINUE
C
C  Get the digitization of the character.
C
      IPSS = 2
      IBNU = 3
      CALL PCCFFF (IPSS,IBNU,IFNT,NASC,WSIZER,WSIZER,RWORK,MDGU,NDGU)
C
C  Set up character width and height, depending on the character
C  being drawn.  IOFFLG flags whether the characters should be
C  offset from row-to-row, and ROFFLG specifies how much to offset.
C
      IF (IFNT.EQ.37 .AND. NASC.EQ.101) THEN
C
C    Showers.
        WIDSCL = 1.20
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 1.60*WSIZER
        IDRFLG = 1
        IOFFLG = 0
        ROFFLG = 0.
        SSCALE = 1.
      ELSE IF (IFNT.EQ.37 .AND. NASC.EQ.102) THEN
C
C    T-storms.
        WIDSCL = 1.90
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 1.60*WSIZER
        IDRFLG = 2
        IOFFLG = 0
        ROFFLG = 0.
        SSCALE = 1.
      ELSE IF (IFNT.EQ.37 .AND. NASC.EQ.103) THEN
C
C    Rain
        WIDSCL = 2.25
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 1.00*WSIZER
        IDRFLG = 3
        IOFFLG = 1
        ROFFLG = 0.5
        SSCALE = 1.
      ELSE IF (IFNT.EQ.37 .AND. NASC.EQ.106) THEN
C
C    Flurries.
        WIDSCL = 2.00
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 1.10*WSIZER
        IDRFLG = 3
        IOFFLG = 1
        ROFFLG = 0.4
        SSCALE = 1.
      ELSE IF (IFNT.EQ.37 .AND. NASC.EQ.104) THEN
C
C    Snow.
        WIDSCL = 2.20
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 1.37*WSIZER
        IDRFLG = 3
        IOFFLG = 1
        ROFFLG = 0.5
        SSCALE = 1.33
      ELSE IF (IFNT.EQ.37 .AND. NASC.EQ.105) THEN
C
C    Ice.
        WIDSCL = 1.40
        CHRWID = WIDSCL*(RWORK(2)-RWORK(1))
        CHRHIT = 0.7*WSIZER
        IDRFLG = 4
        IOFFLG = 1
        ROFFLG = 0.35
        SSCALE = 1.33
C
      ELSE
        WIDSCL = 1.00
        CHRWID = RWORK(2)-RWORK(1)
        CHRHIT = WSIZER
        IDRFLG = 3
        IOFFLG = 0
        ROFFLG = 0.
        SSCALE = 1.0
      ENDIF
C
C  Sweep across the region from top to bottom.  Set the value for 
C  IDRFLG so that outlines are drawn to prevent small filled areas 
C  from dropping out on low-resolution screens.
C
      SEEDX = XRL+0.5*CHRWID/WIDSCL
      SEEDY = YRT-0.5*CHRHIT
      LXEXT = ((XRR-XRL)/CHRWID)+2
      LYEXT = ((YRT-YRB)/CHRHIT)+2
      DO 20 J=1,LYEXT
        YP = SEEDY-(J-1)*CHRHIT
        DO 30 I=1,LXEXT
          XP = SEEDX+(I-1)*CHRWID-
     +           REAL(MOD(J-1,2))*REAL(IOFFLG)*ROFFLG*CHRWID
          L = 1
            K = 0
   60       CONTINUE
            L = L+2
            IF (L .GT. NDGU) GO TO 70
              K = K+1
              IF (RWORK(L).EQ.-2047.) THEN 
                CALL PPINPO(X,Y,N,RWORK(MDGUP1),RWORK(MDGUP),
     +                      K-1,RWORK(MDGU2P),IWORK(MDGU2P),
     +                      NWRK-MDGU2,WMLGFA,IER)
C
C  Ignore degenerate areas.
C
                IF (IER.NE.0 .AND. IER.NE.2) THEN
                  CALL SETER (
     +              'WMRGWT - error calculating area intersection',
     +               1, 1)
                  RETURN
                ENDIF
                K = 0
              ELSE IF (RWORK(L).EQ.-2048.) THEN
C
C  Install a line clipper in here when it becomes available.
C
                IDRFLG = -1
                CALL PPINPO(X,Y,N,RWORK(MDGUP1),RWORK(MDGUP),
     +                      K-1,RWORK(MDGU2P),IWORK(MDGU2P),
     +                      NWRK-MDGU2,WMLGFA,IER)
C
C  Ignore degenerate areas.
C
                IF (IER.NE.0 .AND. IER.NE.2) THEN
                  CALL SETER (
     +              'WMRGWT - error calculating area intersection',
     +               1, 1)
                  RETURN
                ENDIF
                K = 0
              ELSE
                RWORK(K+MDGU)       = XP+SSCALE*RWORK(L)
                RWORK(K+MDGU+MDGUH) = YP+SSCALE*RWORK(L+1)
              ENDIF
            GO TO 60
   70     CONTINUE
   30   CONTINUE
   20 CONTINUE
C
      RETURN
      END
