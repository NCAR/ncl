C
C $Id: pcsetc.f,v 1.4 1992-11-17 18:46:53 kennison Exp $
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
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),ICEN,IOUC,
     +                IOUF,
     +                IQUF,ISHC,ISHF,ITEF,JCOD,NFCC,NFNT,SHDX,SHDY,
     +                SIZA,SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,
     +                XEND,XMUL(3),YBEG,YCEN,YEND,YMUL(3)
      SAVE   /PCPRMS/
C
C Declare the BLOCK DATA routine external to force it to load.
C
      EXTERNAL PCBLDA
C
C The only possibility is the function-code character.  What is really
C stored is the index of the character in the local collating sequence.
C
      IF (WHCH(1:2).EQ.'FC'.OR.WHCH(1:2).EQ.'fc') THEN
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
