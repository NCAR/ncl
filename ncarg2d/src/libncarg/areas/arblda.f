C ---------------------------------------------------------------------
C I N T R O D U C T I O N
C ---------------------------------------------------------------------
C
C This file contains implementation instructions and the code for a
C graphics package called AREAS.  Given a set of "edges" dividing the
C plane into separate areas, the package allows one to recover the
C polygons defining all of those areas.  The polygons may then be
C color-filled, shaded, or used as "shields" in the drawing of other
C objects.
C
C ---------------------------------------------------------------------
C I M P L E M E N T A T I O N   I N S T R U C T I O N S
C ---------------------------------------------------------------------
C
C How you go about implementing AREAS depends on which version of this
C file you have.  Look at the first two lines of the file.  If they are
C
C    .OP LS=10001 LI=1 CB RT ES=< ET=> OC UC=0 IF=2
C    .EL I
C
C you have an IFTRAN version of the file; otherwise, you have a FORTRAN
C version.
C
C The IFTRAN version of AREAS is more readable and, because of extensive
C parameterization, easier to change; unfortunately, one must have an
C IFTRAN preprocessor available in order to generate usable FORTRAN code
C from the IFTRAN code.
C
C To implement the IFTRAN version.
C -- --------- --- ------ -------
C
C To implement the IFTRAN version, review the IFTRAN commands in the
C rest of this section, making such changes as seem necessary for the
C target machine, and then run the resulting file through IFTRAN to
C obtain a FORTRAN file.
C
C To implement the FORTRAN version.
C -- --------- --- ------- -------
C
C The FORTRAN version is portable FORTRAN-77 code and should require no
C modifications to run.
C
C In the FORTRAN version, the section "IMPLEMENTATION INSTRUCTIONS"
C ends here; in the IFTRAN version, various preprocessor-time variables
C are set following this point.
C
C
C ---------------------------------------------------------------------
C C O D E
C ---------------------------------------------------------------------
C
C The BLOCK DATA routine ARBLDA
C --- ----- ---- ------- ------
C
      BLOCK DATA ARBLDA
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI
      SAVE   /ARCOMN/
C
C Below are descriptions of all the common variables and default values
C for those which require defaults.
C
C IAD is the type of arithmetic desired by the user, as follows:
C
C IAD=0 allows AREAS to decide what type of arithmetic to use.
C IAD=1 forces the use of real arithmetic.
C IAD=2 forces the use of double-precision arithmetic.
C IAD=3 forces the use of multiple-precision arithmetic.
C
      DATA IAD / 0 /
C
C IAU is the type of arithmetic actually chosen for use, either by the
C user or by AREAS itself, as follows:
C
C IAU=0 says that no choice has been made yet.
C IAU=1 specifies the use of real arithmetic.
C IAU=2 specifies the use of double-precision arithmetic.
C IAU=3 specifies the use of multiple-precision arithmetic.
C
      DATA IAU / 0 /
C
C ILC is the largest coordinate value to be used.  ARINIT sets RLC
C equal to REAL(ILC).
C
      DATA ILC / 1000000 /
C
C ILM is equal to ILC-1, RLM is equal to ILM, ILP is equal to ILC+1,
C and RLP is equal to ILP.  All of these values are set by ARINIT.
C
C IBS is the base for the multiple-precision arithmetic, when that type
C of arithmetic is selected.  Its value is set by ARINIT.  RBS is made
C equal to REAL(IBS) and DBS is made equal to DBLE(IBS).
C
      DATA IBS / 0 /
C
C IDB is the internal parameter 'IDB', which may be set non-zero by a
C user program to turn on the production of debug plots.
C
      DATA IDB / 0 /
C
C IDC is the internal parameter 'DC', which may be set by a user
C program to change the range of color indices used by ARDBPA.
C
      DATA IDC / 100 /
C
C IDI is the internal parameter 'DI', which may be retrieved in a user
C version of the routine "APR".  Its value will be 1 if the polygon to
C be processed is traced counter-clockwise (interior to the left), or a
C 2 if the polygon is traced clockwise (interior to the right).
C
      DATA IDI / 0 /
C
      END
