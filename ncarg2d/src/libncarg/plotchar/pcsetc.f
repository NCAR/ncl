C
C	$Id: pcsetc.f,v 1.1.1.1 1992-04-17 22:32:19 ncargd Exp $
C
C
C ---------------------------------------------------------------------
C
      SUBROUTINE PCSETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C The subroutine PCSETC may be used to set PLCHHQ parameters which have
C values of type CHARACTER.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),ICEN,IQUF,
     +                ISCR,ITEF,JCOD,NFCC,SSIC,SSPR,SUBS,VPIC(3),
     +                WPIC(3),XBEG,XCEN,XEND,XMUL(3),YBEG,YCEN,YEND,
     +                YMUL(3)
      SAVE   /PCPRMS/
C
C The only possibility is the function-code character.  What is really
C stored is the index of the character in the local collating sequence.
C
      IF (WHCH(1:2).EQ.'FC') THEN
        NFCC=ICHAR(CVAL(1:1))
      ELSE
        CALL SETER ('PCSETC - UNRECOGNIZED PARAMETER NAME',1,2)
      END IF
C
C Done.
C
      RETURN
C
      END
