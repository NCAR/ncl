C
C       $Id: ngreop.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGREOP(WKID,   CONID,  ITYPE, FNAME, IOPT, IAT, RAT,
     +                  NCOLRS, NSTART, CTAB)
C
C Reopen an existing NCAR Graphics metafile for appending.
C
C ARGUMENTS
C
C ON INPUT  
C    
C  WKID    INTEGER     The workstation identifier that the reopened 
C                      metafile will subsequently be known by.  This 
C                      does not have to be the same as the worksttion 
C                      identifier used to create the original metafile.
C
C  CONID   INTEGER     The connection identifier.
C
C  ITYPE   INTEGER     The workstation type.  Currently the only valid 
C                      value is "1".  This subroutine may be augmented 
C                      to accommodate PostScript files in the future.
C
C  FNAME   CHARACTER   The filename of the metafile being opened for
C                      appending.
C
C  IOPT    INTEGER     A flag indicating an action:
C                         = 0 - reestablish the color table only (using
C                               the color table in argument CTAB 
C                               described below).  The attributes of
C                               the reopened workstation will be set
C                               to default values and may well be out
C                               of sync with the current GKS attributes.
C                         = 1 - reestablish the color table and 
C                               GKS state (the GKS state as supplied
C                               in arguments IAT and RAT described
C                               below).  The state values will not be
C                               flushed to the metafile.
C                         = 2 - reestablish the color table and GKS state
C                               and flush the GKS state values to the 
C                               metafile.
C                         = 3 - reestablish the color table and flush 
C                               the current GKS state values to the 
C                               metafile (not the values in IAT and RAT).
C
C                      If IOPT equals 1 or 2, then IAT and RAT must be
C                      supplied, otherwise not.
C
C  IAT    INTEGER(14)  An array containing GKS integer state variables 
C                      as follows:
C
C                        IAT( 1) = Clip indicator
C                        IAT( 2) = Line type
C                        IAT( 3) = Polyline color index
C                        IAT( 4) = Marker type
C                        IAT( 5) = Polymarker color index
C                        IAT( 6) = Text font
C                        IAT( 7) = Text precision
C                        IAT( 8) = Text color index
C                        IAT( 9) = Text path
C                        IAT(10) = Text horizontal alignment
C                        IAT(11) = Text vertical alignment
C                        IAT(12) = Fill area interior style
C                        IAT(13) = Fill are style index
C                        IAT(14) = Fill area color index
C
C                      At any time the values of the above NCAR GKS 
C                      state variables can be saved and/or restored 
C                      using the NGSRAT function.
C                   
C  RAT    REAL(7)      A REAL array containing GKS floating-point
C                      state variables as follows:
C
C                        RAT( 1) = Linewidth scale factor
C                        RAT( 2) = Marker scale factor
C                        RAT( 3) = Character expansion factor
C                        RAT( 4) = Character spacing
C                        RAT( 5) = Character height in world coordinates
C                        RAT( 6) = Character up vector, X component in
C                                  world coordinates
C                        RAT( 7) = Character up vector, Y component in
C                                  world coordinates
C 
C                      At any time the values of the above NCAR GKS 
C                      state variables can be saved and/or restored 
C                      using the NGSRAT function.
C
C  NCOLRS INTEGER     The number of colors in the color table supplied
C                     in argument CTAB, described below.  NCOLRS can be
C                     0 .
C
C
C  NSTART INTEGER     The color index associated with the first color 
C                     in the color table CTAB (all other color indices 
C                     are filled in in sequence).  For example, if
C                     NCOLRS = 3 and NSTART = 4, then the color values
C                     defined in CTAB would be used to define color
C                     indices 4, 5, and 6.
C
C  CTAB   REAL(3,NCOLRS)  
C                     A color table used to initialize the reopened
C                     workstation.  This color table does not
C                     necessarily have to agree with the color table
C                     in effect when the original metafile was created.
C
C
CRLB: PARAMETER (LIDR=75)
      PARAMETER (LIDR=77)
      INTEGER WKID, CONID, ITYPE, IOPT, IAT(*), NCOLRS, NSTART 
      INTEGER IATE(31)     
      REAL    CTAB(3,*),RAT(*),RATE(19)
      CHARACTER*(*) FNAME
      CHARACTER*80 IDR(LIDR),ODR
C
C  Initialize character array to blanks.
C
      DO 10 I=1,LIDR
        IDR(I) = ' '
   10 CONTINUE
C
      IF (IOPT.EQ.1 .OR. IOPT.EQ.2) THEN
        IAFLG = 1
      ELSE
        IAFLG = 0
      ENDIF
C
C  Basic parameters.
C
      WRITE(IDR(1),502) WKID, CONID, ITYPE, IAFLG, NCOLRS, NSTART, IOPT
  502 FORMAT(7I5)
C
C  File name.
C
      NLEN = LEN(FNAME)
      NLRMD = MOD(NLEN,80)
      NLDIV = NLEN/80
      DO 20 I=1,NLDIV
        IL = 80*(I-1)+1
        IR = IL+79
        IDR(I+1) = FNAME(IL:IR)
   20 CONTINUE
      IF (NLRMD .NE. 0) THEN
        IL = 80*NLDIV+1
        IR = IL+NLRMD-1
        IDR(NLDIV+2)(1:NLRMD) = FNAME(IL:IR)
      ENDIF
C
C  Attributes (expand to full set for GESC call).
C

      IF (IAFLG .EQ. 1) THEN
        IATE( 1) = IAT(1)
        IATE( 2) = 1
        IATE( 3) = IAT(2)
        IATE( 4) = IAT(3)
        IATE( 5) = 1
        IATE( 6) = IAT(4)
        IATE( 7) = IAT(5)
        IATE( 8) = 1
        IATE( 9) = IAT(6)
        IATE(10) = IAT(7)
        IATE(11) = IAT(8)
        IATE(12) = IAT(9)
        IATE(13) = IAT(10)
        IATE(14) = IAT(11)
        IATE(15) = 1
        IATE(16) = IAT(12)
        IATE(17) = IAT(13)
        IATE(18) = IAT(14)
        IATE(19) = 1
        IATE(20) = 1
        IATE(21) = 1
        IATE(22) = 1
        IATE(23) = 1
        IATE(24) = 1
        IATE(25) = 1
        IATE(26) = 1
        IATE(27) = 1
        IATE(28) = 1
        IATE(29) = 1
        IATE(30) = 1
        IATE(31) = 1
C
        DO 155 I=1,7
          RATE(I) = RAT(I)
  155   CONTINUE
        RATE(8) = 1.
        RATE(9) = 1.
        RATE(10) = 0.
        RATE(11) = 0.
        CALL GQCLIP(IER,ICD,RATE(12))
        SCL = 1./SQRT(RAT(6)*RAT(6)+RAT(7)*RAT(7))
        XP = RAT(5)*SCL*RAT(6)
        YP = RAT(5)*SCL*RAT(7)
        XB =  YP
        YB = -XP
        CALL GZW2NX(1,XP,XTMP)
        CALL GZW2NY(1,YP,YTMP)
        CALL GZW2NX(1,0.,ZXTMP)
        CALL GZW2NY(1,0.,ZYTMP)
        RATE(16) = XTMP-ZXTMP
        RATE(17) = YTMP-ZYTMP
        CALL GZW2NX(1,XB,XTMP)
        CALL GZW2NY(1,YB,YTMP)
        RATE(18) = XTMP-ZXTMP
        RATE(19) = YTMP-ZYTMP
C --------------------------------------------------
C  Serializing these values was modified to support the presence of
C  32bit ARGB values, which are always *much* larger in magnitude
C  than the original I4 character fields.  Expanded to I10s; note
C  that 2 extra 80-character records are now required, so that
C  subsequent offsets into IDR have changed.     RLB 1/2012
C
C        WRITE(IDR( 6),503) (IATE(LL),LL= 1,20)
C        WRITE(IDR( 7),504) (IATE(LL),LL=21,31)
C        WRITE(IDR( 8),505) (RATE(LL),LL= 1, 5)
C        WRITE(IDR( 9),505) (RATE(LL),LL= 6,10)
C        WRITE(IDR(10),505) (RATE(LL),LL=11,15)
C        WRITE(IDR(11),506) (RATE(LL),LL=16,19)
C --------------------------------------------------        
        WRITE(IDR( 6),507) (IATE(LL),LL= 1, 8)
        WRITE(IDR( 7),507) (IATE(LL),LL= 9,16)
        WRITE(IDR( 8),507) (IATE(LL),LL=17,24)
        WRITE(IDR( 9),507) (IATE(LL),LL=25,31),0
        WRITE(IDR(10),505) (RATE(LL),LL= 1, 5)
        WRITE(IDR(11),505) (RATE(LL),LL= 6,10)
        WRITE(IDR(12),505) (RATE(LL),LL=11,15)
        WRITE(IDR(13),506) (RATE(LL),LL=16,19)
  503   FORMAT(20I4)
  504   FORMAT(11I4)
  505   FORMAT(5E16.7)
  506   FORMAT(4E16.7)
  507   FORMAT(8I10)
      ENDIF
C
C  Color table.
C
      
      IF (IAFLG .EQ. 1) THEN
C       IANDX = 12
        IANDX = 14
      ELSE
        IANDX = 6
      ENDIF
      ICRM = MOD(NCOLRS,4)
      ICDV = NCOLRS/4
      ICOLR = 0
      DO 50 I=1,ICDV
        DO 60 J=1,4
          ICOLR = ICOLR+1
          ID1 = 18*(J-1)+1
          ID2 = ID1+17
          WRITE(IDR(IANDX)(ID1:ID2),500) CTAB(1,ICOLR),
     +                          CTAB(2,ICOLR),CTAB(3,ICOLR)
  500     FORMAT(3F6.3)
   60   CONTINUE
        IANDX = IANDX+1
   50 CONTINUE
C
      DO 70 J=1,ICRM
        ICOLR = ICOLR+1
        ID1 = 18*(J-1)+1
        ID2 = ID1+17
        WRITE(IDR(IANDX)(ID1:ID2),500) CTAB(1,ICOLR),
     +                          CTAB(2,ICOLR),CTAB(3,ICOLR)
   70 CONTINUE
C
      CALL GESC(-1389,IANDX,IDR,1,1,ODR)
C
      RETURN
      END
