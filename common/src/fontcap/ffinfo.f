C
C	$Id: ffinfo.f,v 1.3 2000-08-22 03:53:12 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
