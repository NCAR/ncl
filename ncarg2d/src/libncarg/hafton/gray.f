C
C	$Id: gray.f,v 1.4 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GRAY
C
C SUBROUTINE GRAY COLORS HALF-TONE CELL (I,J) WITH INTENSITY INTEN.
C THE ROUTINE ASSUMES 8X8 CELL SIZE ON A VIRTUAL SCREEN 1024X1024.
C
      DIMENSION       IFOT(16)   ,JFOT(16)
      DIMENSION       WIND2(4)   ,VIEW2(4)
CCC      DIMENSION       MX(16)  ,MY(16)
      COMMON /HAFTO1/ I          ,J          ,INTEN
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
      SAVE
C
      DATA
     1 IFOT(1),IFOT(2),IFOT(3),IFOT(4),IFOT(5),IFOT(6),IFOT(7),IFOT(8)/
     2   1,      5,      1,      5,      3,      7,      3,      7   /
      DATA
     1 IFOT(9),IFOT(10),IFOT(11),IFOT(12),IFOT(13),IFOT(14),IFOT(15)/
     2   3,      7,       3,       7,       1,       5,       1/,
     3 IFOT(16)/
     4   5    /
C
      DATA
     1 JFOT(1),JFOT(2),JFOT(3),JFOT(4),JFOT(5),JFOT(6),JFOT(7),JFOT(8)/
     2   1,      5,      5,      1,      3,     7,      7,      3   /
      DATA
     1 JFOT(9),JFOT(10),JFOT(11),JFOT(12),JFOT(13),JFOT(14),JFOT(15)/
     2   1,      5,       5,       1,       3,       7,       7/,
     3 JFOT(16)/
     4   3    /
      IF (INTEN) 103,103,101
  101 I1 = I*8
      J1 = J*8
      IF ((NPOINT+INTEN) .LE.NPTMAX)  GO TO 1015
      CALL GETSET (VIEW2(1),VIEW2(2),VIEW2(3),VIEW2(4),
     +             WIND2(1),WIND2(2),WIND2(3),WIND2(4),IOLLS2)
      CALL SET (0.,1.,0.,1.,0.,1023.,0.,1023.,1)
      CALL POINTS (XPNT,YPNT,NPOINT,0,0)
      CALL SET (VIEW2(1),VIEW2(2),VIEW2(3),VIEW2(4),
     +          WIND2(1),WIND2(2),WIND2(3),WIND2(4),IOLLS2)
      NPOINT = 0
 1015 DO 102 I2=1,INTEN
      NPOINT = NPOINT + 1
      XPNT(NPOINT) = I1+IFOT(I2)
      YPNT(NPOINT) = J1+JFOT(I2)
  102 CONTINUE
  103 RETURN
      END
