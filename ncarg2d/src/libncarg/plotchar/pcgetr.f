C
C	$Id: pcgetr.f,v 1.1.1.1 1992-04-17 22:32:19 ncargd Exp $
C
C
C ---------------------------------------------------------------------
C
      SUBROUTINE PCGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCGETR may be used to get PLCHHQ parameters which have
C values of type REAL.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),ICEN,IQUF,
     +                ISCR,ITEF,JCOD,NFCC,SSIC,SSPR,SUBS,VPIC(3),
     +                WPIC(3),XBEG,XCEN,XEND,XMUL(3),YBEG,YCEN,YEND,
     +                YMUL(3)
      SAVE   /PCPRMS/
C
      COMMON /PCPFMQ/ RHTW
      SAVE   /PCPFMQ/
C
      COMMON /PCPFFC/ NFNT
      SAVE   /PCPFFC/
C
C Get the selected parameter.
C
      IF (WHCH(1:2).EQ.'AS') THEN
        RVAL=ADDS
      ELSE IF (WHCH(1:2).EQ.'CD') THEN
        RVAL=REAL(JCOD)
      ELSE IF (WHCH(1:2).EQ.'CE') THEN
        RVAL=REAL(ICEN)
      ELSE IF (WHCH(1:2).EQ.'CH') THEN
        RVAL=HPIC(3)
      ELSE IF (WHCH(1:2).EQ.'CS') THEN
        RVAL=2.*CONS
      ELSE IF (WHCH(1:2).EQ.'CV') THEN
        RVAL=VPIC(3)
      ELSE IF (WHCH(1:2).EQ.'CW') THEN
        RVAL=WPIC(3)
      ELSE IF (WHCH(1:2).EQ.'DB') THEN
        RVAL=DSTB
      ELSE IF (WHCH(1:2).EQ.'DL') THEN
        RVAL=DSTL
      ELSE IF (WHCH(1:2).EQ.'DR') THEN
        RVAL=DSTR
      ELSE IF (WHCH(1:2).EQ.'DT') THEN
        RVAL=DSTT
      ELSE IF (WHCH(1:2).EQ.'FN') THEN
        RVAL=REAL(NFNT)
      ELSE IF (WHCH(1:2).EQ.'HW') THEN
        RVAL=RHTW
      ELSE IF (WHCH(1:2).EQ.'IH') THEN
        RVAL=HPIC(2)
      ELSE IF (WHCH(1:2).EQ.'IS') THEN
        RVAL=SSIC
      ELSE IF (WHCH(1:2).EQ.'IV') THEN
        RVAL=VPIC(2)
      ELSE IF (WHCH(1:2).EQ.'IW') THEN
        RVAL=WPIC(2)
      ELSE IF (WHCH(1:2).EQ.'PH') THEN
        RVAL=HPIC(1)
      ELSE IF (WHCH(1:2).EQ.'PS') THEN
        RVAL=SSPR
      ELSE IF (WHCH(1:2).EQ.'PV') THEN
        RVAL=VPIC(1)
      ELSE IF (WHCH(1:2).EQ.'PW') THEN
        RVAL=WPIC(1)
      ELSE IF (WHCH(1:2).EQ.'QU') THEN
        RVAL=REAL(IQUF)
      ELSE IF (WHCH(1:2).EQ.'SC') THEN
        RVAL=REAL(ISCR)
      ELSE IF (WHCH(1:2).EQ.'SS') THEN
        RVAL=SUBS
      ELSE IF (WHCH(1:2).EQ.'TE') THEN
        RVAL=REAL(ITEF)
      ELSE IF (WHCH(1:2).EQ.'XB') THEN
        RVAL=XBEG
      ELSE IF (WHCH(1:2).EQ.'XC') THEN
        RVAL=XCEN
      ELSE IF (WHCH(1:2).EQ.'XE') THEN
        RVAL=XEND
      ELSE IF (WHCH(1:2).EQ.'YB') THEN
        RVAL=YBEG
      ELSE IF (WHCH(1:2).EQ.'YC') THEN
        RVAL=YCEN
      ELSE IF (WHCH(1:2).EQ.'YE') THEN
        RVAL=YEND
      ELSE
        CALL SETER ('PCGETR - UNRECOGNIZED PARAMETER NAME',1,2)
      END IF
C
C Done.
C
      RETURN
C
      END
