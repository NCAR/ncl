C
C $Id: slpwrv.f,v 1.5 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLPWRV (XPOS,YPOS,CHRS,SIZE)
C
C This routine is used by STITLE to write a message vertically.  The
C message is centered at the position (XPOS,YPOS) in the fractional
C coordinate system.  The characters are as specified by CHRS and they
C are of the size specified by SIZE (a character width, between 0 and
C 1, in the fractional coordinate system).
C
C Note: The current "user" coordinate system must be the fractional
C coordinate system for this routine to work properly.
C
        CHARACTER*(*) CHRS
C
C Find the length of the character string.
C
        NCHR=LEN(CHRS)
C
C Compute the appropriate size to tell WTSTR to use.
C
        ISIZ=INT(1023.*SIZE)
C
C Write the characters one at a time, using WTSTR.
C
        DO 101 I=1,NCHR
          CALL WTSTR (XPOS,YPOS+SIZE*(NCHR+1-2*I),CHRS(I:I),ISIZ,0,0)
          IF (ICFELL('SLPWRV',1).NE.0) RETURN
  101   CONTINUE
C
        RETURN
C
      END
