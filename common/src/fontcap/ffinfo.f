C
C	$Id: ffinfo.f,v 1.4 2008-07-27 12:23:41 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE FFINFO(IER)
C
C  Fill in the values for the initial technical information of
C  the fontcap.
C
      include 'fntcom.h'
C
C  Number of bits per integer; number of bytes per word.
C
      NBPERI = I1MACH(5)
      NBYPWD = NBPERI/8
C
C  Type flag.
C
      IS = 1
      TYPFLG = 0
      DO 10 I=1,16
        TYPFLG = IOR(TYPFLG,ISHIFT(IS,I-1))
   10 CONTINUE
C
C  Index table pointer (2*NUMNTR bytes, plus 40 bytes for the font
C  name, plus two bytes for the pointer to the last byte).
C
      TABPNT = NBYPWD*((2*NUMNTR+40+2-1)/NBYPWD)+NBYPWD
C
C  X bit width.
C
      MXF = MAX(IABS(FLLX),IABS(FLLY))
      IXWD = FRIGHT+MXF
      DO 20 I=1,16
        IF (IXWD .LT. 2**I) THEN
          XBITWD = I
          GO TO 30
        ENDIF
   20 CONTINUE
      IER = EXWD
      RETURN
   30 CONTINUE
C
C  Y bit width (must accommodate for the character width being stored`
C  in a Y coordinate position).
C
      IYWD = FRIGHT+MXF
      DO 40 I=1,16
        IF (IYWD .LT. 2**I) THEN
          YBITWD = I
          GO TO 50
        ENDIF
   40 CONTINUE
      IER = EYWD
      RETURN
   50 CONTINUE
C
C  X bias.
C
      IF (FLLEX .LT. 0) THEN
        XBIAS = IABS(FLLEX)
      ELSE
        XBIAS = -FLLEX
      ENDIF
C
C  Y bias.
C
      IF (FLLEY .LT. 0) THEN
        YBIAS = IABS(FLLEY)
      ELSE
        YBIAS = -FLLEY
      ENDIF
C
C  Special value flag field width.
C
      DO 60 I=1,16
        IF (SVNUM .LT. 2**I) THEN
          PKFLWD = I
          GO TO 70
        ENDIF
   60 CONTINUE
      IER = ESWD
      RETURN
   70 CONTINUE
C
C  Packet width.
C
      PKWID = PKFLWD+XBITWD+YBITWD
C
      RETURN
      END
