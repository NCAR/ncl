      SUBROUTINE DPSETC (PNAM,CVAL)
C
C This routine is used to set the value of an internal parameter of
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
        IF (ICFELL('DPSETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Because most changes in internal parameters of DASHPACK result in
C changing the way in which the current dash pattern is interpreted,
C force a buffer dump before changing anything and ensure that, the
C next time DPDRAW is called, the dash pattern element descriptors
C will be regenerated.
C
        CALL DPSMTH (0.,0.,2)
        IF (ICFELL('DPSETC',2).NE.0) RETURN
C
        IDPI=0
C
C Change the specified parameter or log an error.
C
        IF (LEN(PNAM).LT.3) THEN
          CTMP='DPSETC - PARAMETER NAME TOO SHORT - '//PNAM
          CALL SETER (CTMP(1:38),3,1)
          RETURN
        ELSE IF (PNAM(1:3).EQ.'CRB'.OR.PNAM(1:3).EQ.'crb') THEN
          CHRB=CVAL
        ELSE IF (PNAM(1:3).EQ.'CRG'.OR.PNAM(1:3).EQ.'crg') THEN
          CHRG=CVAL
        ELSE IF (PNAM(1:3).EQ.'CRS'.OR.PNAM(1:3).EQ.'crs') THEN
          CHRS=CVAL
        ELSE IF (PNAM(1:3).EQ.'DPT'.OR.PNAM(1:3).EQ.'dpt') THEN
          LCDP=MAX(1,MIN(256,LEN(CVAL)))
          CHDP=CVAL
        ELSE
          CTMP='DPSETC - PARAMETER NAME NOT KNOWN - '//PNAM
          CALL SETER (CTMP(1:39),4,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
