C
C***********************************************************************
C C O D E   -   P A R A M E T E R - A C C E S S   R O U T I N E S
C***********************************************************************
C
      SUBROUTINE PCGETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C The subroutine PCGETC may be used to get PLCHHQ parameters which have
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
C Provide for the case that the parameter had not been initialized yet.
C
      IF (WHCH(1:2).EQ.'FC'.OR.WHCH(1:2).EQ.'fc') THEN
        IF (NFCC.EQ.0) NFCC=ICHAR(':')
        CVAL=CHAR(NFCC)
      ELSE
        CALL SETER ('PCGETC - UNRECOGNIZED PARAMETER NAME',1,2)
      END IF
C
C Done.
C
      RETURN
C
      END
