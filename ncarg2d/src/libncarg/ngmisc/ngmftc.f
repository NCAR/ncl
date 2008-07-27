C
C       $Id: ngmftc.f,v 1.4 2008-07-27 00:17:18 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGMFTC(WKID)
C
C Effects a temporaty close in metafile output allowing for subsequent 
C reopening for appending using NGREOP.  The metafile output may be 
C suspended at any time, and not necessarily at a picture break.
C
C ARGUMENTS
C
C ON INPUT  
C    
C   WKID  An integer argument indicating the workstation ID of the
C         workstation to temporarily close.
C
      INTEGER WKID
      CHARACTER*80 IDR(1),ODR(1)
C
      WRITE(IDR(1)(1:5),500) WKID
  500 FORMAT(I5)
      CALL SFLUSH
      CALL GESC(-1387,1,IDR,1,1,ODR)
C
      RETURN
      END
