C
C $Id: dpgetc.f,v 1.3 1994-09-22 22:06:17 kennison Exp $
C
      SUBROUTINE DPGETC (PNAM,CVAL)
C
C This routine is used to get the value of an internal parameter of
C type CHARACTER.
C
        CHARACTER*(*) PNAM,CVAL
C
C Declare the character common block.
C
        COMMON /DPCMCH/ CHDP,CHRB,CHRG,CHRS
          CHARACTER*256 CHDP
          CHARACTER*1 CHRB,CHRG,CHRS
        SAVE   /DPCMCH/
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Declare the block data external to force it to load.
C
        EXTERNAL DPBLDA
C
C Declare a temporary variable in which to form error messages.
C
        CHARACTER*39 CTMP
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPGETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the specified parameter or log an error.
C
        IF (LEN(PNAM).LT.3) THEN
          CTMP='DPGETC - PARAMETER NAME TOO SHORT - '//PNAM
          CALL SETER (CTMP(1:38),2,1)
          RETURN
        ELSE IF (PNAM(1:3).EQ.'CRB'.OR.PNAM(1:3).EQ.'crb') THEN
          CVAL=CHRB
        ELSE IF (PNAM(1:3).EQ.'CRG'.OR.PNAM(1:3).EQ.'crg') THEN
          CVAL=CHRG
        ELSE IF (PNAM(1:3).EQ.'CRS'.OR.PNAM(1:3).EQ.'crs') THEN
          CVAL=CHRS
        ELSE IF (PNAM(1:3).EQ.'DPT'.OR.PNAM(1:3).EQ.'dpt') THEN
          CVAL=CHDP(1:LCDP)
        ELSE
          CTMP='DPGETC - PARAMETER NAME NOT KNOWN - '//PNAM
          CALL SETER (CTMP(1:39),3,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
