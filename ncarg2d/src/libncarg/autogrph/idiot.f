C
C	$Id: idiot.f,v 1.1.1.1 1992-04-17 22:31:05 ncargd Exp $
C
C
C ---------------------------------------------------------------------
C
      SUBROUTINE IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA)
C
      REAL XDRA(*),YDRA(*)
C
      INTEGER LDSH(*)
C
      CHARACTER*(*) LABX,LABY,LABG
C
      CHARACTER*16 AGBNCH
C
C This is an implementation of the routine from which AUTOGRAPH grew.
C It should work pretty much as the original did (if you can figure out
C what that was).
C
C Do statistics-gathering call.
C
      LOGICAL Q8Q4
      SAVE Q8Q4
      DATA Q8Q4 /.TRUE./
      IF (Q8Q4) THEN
        CALL Q8QST4('GRAPHX','AUTOGRAPH','IDIOT','VERSION 07')
        Q8Q4 = .FALSE.
      ENDIF
C
      CALL ANOTAT (LABX,LABY,1,2-ISIGN(1,NPTS),1,AGBNCH(LDSH))
C
      CALL DISPLA (2-MAX0(-1,MIN0(1,LFRA)),1,LTYP)
C
      CALL AGEZSU (5,XDRA,YDRA,IABS(NPTS),1,IABS(NPTS),LABG,IIVX,IIEX,
     +                                                        IIVY,IIEY)
      CALL AGBACK
C
      CALL AGCURV (XDRA,1,YDRA,1,IABS(NPTS),1)
C
      IF (LFRA.GT.0) CALL FRAME
C
      RETURN
C
C Revision history:
C
C February, 1979   Added a revision history and enhanced machine
C                  independency.
C
C September, 1979  Fixed a couple of problems which caused the code to
C                  bomb when core was pre-set to indefinites and the
C                  1st graph drawn was peculiar in some way and another
C                  which caused it to set the default dashed-line-speci-
C                  fier length wrong.  Added new documentation.
C
C October, 1979    Changed the way IDIOT behaves when NPTS is negative.
C
C March, 1980      Fixed a couple of small errors, one which prevented
C                  an error exit in AGSETP from ever being reached and
C                  another which caused AUTOGRAPH to blow up when given
C                  a zero or negative on a logarithmic axis.  Changed
C                  the way in which NBPF is computed by AGSTR1.
C
C August, 1981     Removed all calls setting the plotter intensity and
C                  made the computation of the variable SMRL portable.
C
C April, 1984      Made the code strictly FORTRAN-77 compatible, taking
C                  out all dependency on support routines (such as LOC).
C                  This required some changes in the user interface.
C
C February, 1985   Put code in AGSETP to reclaim character-store space
C                  used by character-string dash patterns when they are
C                  redefined using binary patterns.  Also changed AGGTCH
C                  to return a single blank for a non-existent string.
C
C August, 1985     Put code in AGGETP so that the label-name identifier
C                  is now returned properly.  Among other things, this
C                  cures a problem which caused the character-storage
C                  space to be eaten up.
C
C December, 1985   Fixed AGSETP to zero the current-line pointer when
C                  the current-label pointer is changed.
C
C January, 1986    Fixed AGAXIS to respond properly to the zeroing of
C                  NCIM by AGCHNL.
C
C August, 1986     Put patches in AGSETC and AGSETP to correct the
C                  handling of label names.
C
C September,1986   Put in SAVE statements for all the COMMON blocks.
C
C February, 1987   Changed the value of SMJP, in AGAXIS, from 4 to 1.
C
C May, 1987        Changed the copyright notice to include the phrase
C                  "by UCAR".  Sigh.
C
C October, 1989    Changed the computation and use of "SMRL" to avoid
C                  various problems that have occurred over the years.
C                  Fixed a bug which sometimes caused the first label
C                  on an axis to be omitted.  Added the routines AGGETR
C                  and AGSETR for consistency with other utilities.
C
C May, 1990        Changed AGNUMB to more accurately compute the value
C                  of a quantity from which the digits of a label are
C                  to be generated.  Changed AGEXAX to more accurately
C                  compute the value of a quantity to be used as the
C                  distance between tick marks on an axis.  Tweaked the
C                  values of the constants SMLL and SMRL.  Changed all
C                  occurrences of the function name SNGL to REAL.
C
      END
