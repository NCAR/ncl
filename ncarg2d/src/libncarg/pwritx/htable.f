C
C	$Id: htable.f,v 1.4 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
      SUBROUTINE HTABLE
C
C Creates a machine independent table from which one can obtain
C the Hollerith code for a given character.
C
C The hollerith codes are used as an index into the array containing
C the digitizations of each of the characters.
C
      INTEGER  HOLLER, I, TBSIZE
      LOGICAL  READY
      PARAMETER (TBSIZE=256)
C
C If your machine uses the ASCII character set, TBSIZE can be changed
C to 128.
C
      COMMON / HOLTAB / HOLLER(TBSIZE), READY
C
C Define alphanumerics for use in setting up the table.
C
      CHARACTER*36 ALPHAN
      DATA ALPHAN / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' /
C
C First initialize the array to an error value, 200
C
      DO 5  I=1, TBSIZE
           HOLLER(I) = 200
 5    CONTINUE
C
C Use the intrinsic function ICHAR to map the hollerith codes into
C specific positions in the array HOLLER
C
C
C Put the codes for the alphanumerics in the array.
C
      DO 10 I=1,36
           HOLLER(ICHAR(ALPHAN(I:I))) = I
 10   CONTINUE
C
C Now put in the other accepted characters
C
      HOLLER( ICHAR(':') ) = 0
      HOLLER( ICHAR('+') ) = 37
      HOLLER( ICHAR('-') ) = 38
      HOLLER( ICHAR('*') ) = 39
      HOLLER( ICHAR('/') ) = 40
      HOLLER( ICHAR('(') ) = 41
      HOLLER( ICHAR(')') ) = 42
      HOLLER( ICHAR('$') ) = 43
      HOLLER( ICHAR('=') ) = 44
      HOLLER( ICHAR(' ') ) = 45
      HOLLER( ICHAR(',') ) = 46
      HOLLER( ICHAR('.') ) = 47
      HOLLER( ICHAR('''') ) = 52
C
C Set the flag to indicate the table is initialized
C
      READY = .TRUE.
C
C Done
C
      RETURN
      END
