C
C $Id: pcblda.f,v 1.2 1992-11-17 18:46:00 kennison Exp $
C
C
C***********************************************************************
C B L O C K   D A T A   R O U T I N E S   -   D E F A U L T S
C***********************************************************************
C
      BLOCK DATA PCBLDA
C
C Specify default values of internal parameters of PLCHHQ, PLCHMQ, and
C the routine PCCFFC, which is called to retrieve a character from a
C fontcap.
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
      COMMON /PCSVEM/ IBNU,ICOD,IDDA(8625),IDDL,RDGU(8800),IDPC(256),
     +                IERU,INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFMQ/ IMAP,RHTW
      SAVE   /PCPFMQ/
C
C Define the add-space flag, which allows the user to specify
C additional spacing between characters along the line.
C
      DATA ADDS / 0. /
C
C Define the constant-spacing flag, which allows the user to position
C characters a constant distance apart along the line.
C
      DATA CONS / 0. /
C
C Define the number of the unit from which the binary file of data may
C be read.
C
      DATA IBNU / 3 /
C
C ICEN is the internal parameter 'CE'; it determines the type of
C centering to be done by PLCHHQ.  When ICEN is zero, centering is
C controlled by the final argument of PLCHHQ, CNTR.  When ICEN is
C non-zero, the value of CNTR is ignored and the string written is
C centered exactly on the point (XPOS,YPOS).  This will mostly be
C useful when when writing a single character which is intended to
C mark a particular point.
C
      DATA ICEN / 0 /
C
C IMAP is the internal parameter 'MA'; it says how X/Y coordinates
C are to be mapped.
C
      DATA IMAP / 0 /
C
C INIT is an initialization flag for PLCHHQ.
C
      DATA INIT / 0 /
C
C IOUC is the outline color index, to be used only when IOUF is non-
C zero.
C
      DATA IOUC / 1 /
C
C IOUF is the "outline" flag.  When it is set non-zero, characters
C from the filled fonts are outlined in a contrasting color.
C
      DATA IOUF / 0 /
C
C IQUF is the "quality" flag.  When it is zero, high-quality characters
C are used.  Otherwise, lower-quality characters are produced by calling
C PCMQLQ, which in turn calls either PLCHMQ (if IQUF = 1) or PLCHLQ (if
C IQUF = 2).
C
      DATA IQUF / 0 /
C
C ISHC is the shadow color index.  When ISHF is set greater than or
C equal to zero, characters drawn by PLCHHQ and PLCHMQ have shadows
C drawn in the color specified by ISHC.
C
      DATA ISHC / 0 /
C
C ISHF is the "shadow" flag.  When it is set non-zero, characters drawn
C by PLCHHQ and PLCHMQ are "shadowed" by first being drawn, with an
C an offset of SHDX and SHDY, in the color specified by ISHC.
C
      DATA ISHF / 0 /
C
C ITEF is the "compute-text-extent-vectors" flag.  When it is set, calls
C to PLCHHQ with ANGD = 360. plot nothing; they cause the parameters
C DSTL, DSTT, DSTB, and DSTT (the magnitudes of the text extent vectors)
C to be computed for recovery by the calling program.
C
      DATA ITEF / 0 /
C
C The first time a given dataset is read, it is checked for correctness.
C The flags IVCO and IVDU are used to prevent this from being done
C thereafter.
C
      DATA IVCO,IVDU / 0,0 /
C
C JCOD may be set to 0 to request use of the complex dataset or to 1 to
C request use of the duplex dataset.  Its value is compared with the
C value of ICOD every time PLCHHQ is called, and, if the two differ,
C the dataset is reloaded.
C
      DATA JCOD / 0 /
C
C NFCC is the position of the function code character in the collating
C sequence - the default, an apostrophe, is set during initialization.
C
      DATA NFCC / 0 /
C
C NFNT, if non-zero, selects one of the fonts defined by fontcaps,
C characters from which are used in place of the "built-in" high
C quality characters.  One may change font in the middle of a string,
C using the "function code" F, followed by the number of the desired
C font.
C
      DATA NFNT / 0 /
C
C Define the height and width of characters of the various sizes and
C the vertical spacing between lines (on a 1024x1024 grid).
C
      DATA WPIC / 16.,12., 8. /
      DATA HPIC / 21.,13., 9. /
      DATA VPIC / 32.,20.,14. /
C
C Define the default value of the ratio of character height to width.
C
      DATA RHTW / 1.75 /
C
C SIZA is the internal parameter 'SA', which serves as a multiplier
C for the sizes of the characters written by PLCHHQ.  Its default
C value is (16/21)*(7/6)=(112/126)=.888888888888888, which serves
C to make the characters written by a call to PLCHHQ more nearly
C the same size as those written by a call to PLCHMQ.  (They are
C exactly the same height, but not necessarily of the same width.)
C This parameter is being introduced in October, 1992, for inclusion
C in release 3.2 of NCAR Graphics.  Those users who want PLCHHQ to
C produce the same size characters it used to should set 'SA' to 1.
C
      DATA SIZA / .888888888888888 /
C
C Define the subtract-space flag, which allows the user to specify
C reduced spacing between characters along the line.
C
      DATA SUBS / 0. /
C
C Define the extent of the shifts for super- or subscripting (in terms
C of plotter address units on a 1024x1024 grid).
C
      DATA SSPR / 10. /
      DATA SSIC /  7. /
C
C SHDX and SHDY specify the X and Y offsets to be used in drawing
C character shadows.  These are stated as fractions of the font height.
C
      DATA SHDX,SHDY / .05,.05 /
C
C Define default values for x/y positioning information retrievable by
C the user, just in case.
C
      DATA XBEG,XCEN,XEND / 0.,0.,0. /
      DATA YBEG,YCEN,YEND / 0.,0.,0. /
C
C Define multipliers for the x and y components of the digitized
C characters to make them come out the right size.
C
      DATA XMUL / 1.,1.,1. /
      DATA YMUL / 1.,1.,1. /
C
      END
