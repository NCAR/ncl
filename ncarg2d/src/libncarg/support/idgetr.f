C
C $Id: idgetr.f,v 1.1 1995-11-03 23:45:21 kennison Exp $
C
      SUBROUTINE IDGETR (PNAM,RVAL)
C
C Get in RVAL the real value of the BIVAR parameter named PNAM.
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
C Declare the block data routine IDBLDA external to force it to load,
C so that the internal parameters will be initialized.
C
        EXTERNAL IDBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDGETR (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
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
          RVAL=REAL(INTY)
C
C ... or the flag specifying the triangulation type.
C
        ELSE IF (CTMP.EQ.'TTY'.OR.CTMP.EQ.'tty') THEN
C
          RVAL=REAL(ITTY)
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
  901   CMSG(1:37)='IDGETR (BIVAR) - INVALID KEYWORD: '//CTMP
        CALL SETER (CMSG(1:37),2,1)
        RETURN
C
      END
