C
C $Id: argpai.f,v 1.2 2000-07-12 16:21:47 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE ARGPAI (CHS,IBG,IVL)
C
C This routine looks for a subscript of the form "(n)" in positions IBG
C and following of the character string CHS.  It returns the value of
C "n" (which may be negative) in IVL.  If no subscript is found, IVL
C is zeroed.
C
      CHARACTER*(*) CHS
      CHARACTER*1 ICH
C
      IVL=0
      ISN=1
C
      IST=0
C
      DO 101 ICHS=IBG,LEN(CHS)
      ICH=CHS(ICHS:ICHS)
      IF (.NOT.(ICH.NE.' ')) GO TO 10001
        IF (.NOT.(IST.EQ.0)) GO TO 10002
          IF (ICH.NE.'(') GO TO 102
          IST=1
        GO TO 10003
10002   CONTINUE
        IF (.NOT.(IST.EQ.1)) GO TO 10004
          IF (.NOT.(ICHAR(CHS(ICHS:ICHS)).GE.ICHAR('0').AND.ICHAR(CHS(IC
     +HS:ICHS)).LE.ICHAR('9'))) GO TO 10005
            IVL=ICHAR(CHS(ICHS:ICHS))-ICHAR('0')
          GO TO 10006
10005     CONTINUE
          IF (.NOT.(ICH.EQ.'-')) GO TO 10007
            ISN=-1
          GO TO 10006
10007     CONTINUE
          IF (.NOT.(ICH.NE.'+')) GO TO 10008
            GO TO 102
10006     CONTINUE
10008     CONTINUE
          IST=2
        GO TO 10003
10004   CONTINUE
        IF (.NOT.(IST.EQ.2)) GO TO 10009
          IF (.NOT.(ICHAR(CHS(ICHS:ICHS)).GE.ICHAR('0').AND.ICHAR(CHS(IC
     +HS:ICHS)).LE.ICHAR('9'))) GO TO 10010
            IVL=IVL*10+ICHAR(CHS(ICHS:ICHS))-ICHAR('0')
          GO TO 10011
10010     CONTINUE
          IF (.NOT.(ICH.NE.')')) GO TO 10012
            GO TO 102
10012     CONTINUE
            GO TO 103
10011     CONTINUE
10003   CONTINUE
10009   CONTINUE
10001 CONTINUE
  101 CONTINUE
C
  102 IVL=0
      RETURN
C
  103 IVL=ISN*IVL
      RETURN
C
      END
