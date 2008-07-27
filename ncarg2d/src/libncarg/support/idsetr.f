C
C $Id: idsetr.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDSETR (PNAM,RVAL)
C
C Set the real value of the BIVAR parameter named PNAM from RVAL.
C
        CHARACTER*(*) PNAM
C
C The common block IDCOMN holds all of the internal parameters of
C the package BIVAR.
C
        COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
        SAVE   /IDCOMN/
C
C Define a temporary variable in which to put the first three characters
C of PNAM.
C
        CHARACTER*3 CTMP
C
C Define a character variable in which messages may be formed.
C
        CHARACTER*37 CMSG
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL IDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDSETR (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Extract the first three characters of the parameter name.
C
        CTMP=PNAM
C
C If the parameter name has less than three characters, log an error.
C
        IF (LEN(PNAM).LT.3) GO TO 901
C
C See what the parameter name is ...
C
C ... the flag specifying the interpolation type, ...
C
        IF      (CTMP.EQ.'ITY'.OR.CTMP.EQ.'ity') THEN
C
          INTY=MAX(0,MIN(1,INT(RVAL)))
C
C ... or the flag specifying the triangulation type.
C
        ELSE IF (CTMP.EQ.'TTY'.OR.CTMP.EQ.'tty') THEN
C
          ITTY=MAX(0,MIN(1,INT(RVAL)))
C
        ELSE
C
C Otherwise, the parameter name is not recognized.
C
          GO TO 901
C
        END IF
C
        RETURN
C
C Error exit.
C
  901   CMSG(1:37)='IDSETR (BIVAR) - INVALID KEYWORD: '//CTMP
        CALL SETER (CMSG(1:37),8,1)
        RETURN
C
      END
