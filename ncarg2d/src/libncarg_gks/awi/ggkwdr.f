C
C	$Id: ggkwdr.f,v 1.1 1993-01-09 01:58:44 fred Exp $
C
      SUBROUTINE GGKWDR(WKID,FCODE,CONT,IL1,IL2,ID,IC1,IC2,IC,
     -                  RL1,RL2,RX,RY,STRL1,STRL2,STR,IER,IMSG)
C
C  Dummy driver interface routine.
C
      PARAMETER(NFNC=33)
      INTEGER WKID,FCODE,CONT,IL1,IL2,ID(*),IC1,IC2,IC(*)
      INTEGER RL1,RL2,STRL1,STRL2,STR(*),IER,IMSG(*)
      REAL    RX(*),RY(*)
      CHARACTER*1  STRC(500)
      CHARACTER*40 LAB(NFNC)
      INTEGER FMAP(NFNC)
      DATA FMAP/-3,-2,0,1,6,11,12,13,14,15,22,23,24,26,27,
     +          28,30,31,32,33,34,35,36,38,39,40,56,61,3,
     +          72,71,-256,-1/
      DATA    (LAB(LL),LL=1,17)/
     +           'OPEN WORKSTATION',
     +           'ACTIVATE WORKSTATION',
     +           'CLOSE WORKSTATION',
     +           'CLEAR WORKSTATION',
     +           'ESCAPE',
     +           'POLYLINE',
     +           'POLYMARKER',
     +           'TEXT',
     +           'FILL AREA',
     +           'CELL ARRAY',
     +           'SET LINETYPE',
     +           'SET LINEWIDTH SCALE FACTOR',
     +           'SET POLYLINE COLOR INDEX',
     +           'SET MARKER TYPE',
     +           'SET MARKER SIZE SCALE FACTOR',
     +           'SET POLYMARKER COLOR INDEX',
     +           'SET TEXT FONT AND PRECISION'
     +                                 /
      DATA    (LAB(LL),LL=18,NFNC)/
     +           'SET CHARACTER EXPANSION FACTOR',
     +           'SET CHARACTER SPACING',
     +           'SET TEXT COLOR INDEX',
     +           'SET CHARACTER HEIGHT AND UP VECTOR',
     +           'SET TEXT PATH',
     +           'SET TEXT ALIGNMENT',
     +           'SET FILL AREA INTERIOR STYLE',
     +           'SET FILL AREA STYLE INDEX',
     +           'SET FILL AREA COLOR INDEX',
     +           'SET COLOR REPRESENTATION',
     +           'SET CLIP INDICATOR',
     +           'UPDATE WORKSTATION',
     +           'SET WORKSTATION WINDOW',
     +           'SET WORKSTATION VIEWPORT',
     +           'INQUIRE COLOUR REPRESENTATION',
     +           'DEACTIVATE WORKSTATION'
     +                              /
      DATA    KOUNT/-20/
C
      KOUNT = KOUNT-1
      IER = 0
C
C  Determine the function name associated with FCODE.
C
      DO 10 I=1,NFNC
        IF (FCODE .EQ. FMAP(I)) THEN
          JNDX = I
          GO TO 20
        ENDIF
   10 CONTINUE
C     WRITE(6,800) FCODE
C 800 FORMAT(' Invalid function code =',I5)
      IER = -109
      RETURN
   20 CONTINUE
C
C  Write out WKID, FCODE, the function name and CONT.
C
      WRITE(6,500) WKID,FCODE,LAB(JNDX),CONT
  500 FORMAT('------------------------------------------------',
     -       /' GGKWDR called with:  WKID  =',I4,
     -       /'                      FCODE =',I4,' -- ',A40,
     -       /'                      CONT  =',I4)
C
C  Write out the integer array.
C
      IF (IL1 .GT. 0) THEN
        IF (FCODE.EQ.-3 .OR. FCODE.EQ.-2) THEN
          IL2 = KOUNT
          WRITE(6,701) IL1,IL2,(LL,ID(LL),LL=1,3)
  701     FORMAT(/'  IL1 =',I5,', IL2 =',I5,
     -        /('    ID(',I3,') = ',I8) )
        ELSE
          WRITE(6,501) IL1,IL2,(LL,ID(LL),LL=1,IL2)
  501     FORMAT(/'  IL1 =',I5,', IL2 =',I5,
     -          /('    ID(',I3,') = ',I8) )
        ENDIF
      ELSE
        WRITE(6,601) IL1,IL2
  601   FORMAT(/'  IL1 =',I5,', IL2 =',I5)
      ENDIF
C
C  Write out the color index array.
C
      IF (IC1 .GT. 0) THEN
        WRITE(6,502) IC1,IC2,(LL,IC(LL),LL=1,IC2)
  502   FORMAT(/'  IC1 =',I6,', IC2 =',I6,
     -        /('    IC(',I3,') = ',I8) )
      ELSE
        WRITE(6,602) IC1,IC2
  602   FORMAT(/'  IC1 =',I5,', IC2 =',I5)
      ENDIF
C
C  Write out the real arrays.
C
      IF (RL1 .GT. 0) THEN
        WRITE(6,503) RL1,RL2,(LL,RX(LL),LL,RY(LL),LL=1,RL2)
  503   FORMAT(/'  RL1 =',I5,', RL2 =',I5,
     -        /('    RX(',I3,') = ',F10.3,', RY(',I3,') = ',
     -               F10.3) )
      ELSE
        WRITE(6,603) RL1,RL2
  603   FORMAT(/'  RL1 =',I5,', RL2 =',I5)
      ENDIF
C
C  Write out the strings.
C
      IF (STRL2 .GT. 0) THEN
        DO 30 I=1,STRL2
          STRC(I) = CHAR(STR(I))
   30   CONTINUE
        WRITE(6,504) STRL1,STRL2,(LL,STRC(LL),LL=1,STRL2)
  504   FORMAT(/'  STRL1 =',I4,', STRL2 =',I4,
     -        /('    STR(',I3,') = ',A1) )
      ELSE
        WRITE(6,604) STRL1,STRL2
  604   FORMAT(/'  STRL1 =',I4,', STRL2 =',I4)
      ENDIF
C
      RETURN
      END
