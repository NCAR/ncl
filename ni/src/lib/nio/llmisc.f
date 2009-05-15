C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EPRIN
C
C This routine just prints the current error message, if any.
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Do it.
C
        IF (IERRF.NE.0) WRITE (IERRU,'('' ERROR '',I4,'' IN '',A)')
     +                                              IERRF,ERMSG(1:LOMSG)
C
C Done.
C
        RETURN
C
      END
C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SETER (MESSG,NERRF,IROPT)
C
        CHARACTER*(*) MESSG
C
C This routine is called when an error occurs.  The arguments specify
C an error message, an error number, and an option indicating whether
C the error is recoverable or fatal.  Exactly how the error is handled
C depends not only on the values of these arguments, but also on the
C values of internal variables specifying whether recovery mode is in
C effect and whether a previous error has occurred and not yet been
C recovered from and cleared.
C
C If no uncleared recoverable error has occurred and there are no
C errors in the arguments, then the following apply:
C
C   IROPT = 1, recovery mode active      -  just remember the error.
C   IROPT = 1, recovery mode not active  -  print and stop.
C   IROPT = 2                            -  print, dump, and stop.
C
C Input:
C
C   MESSG  - the error message (113 characters maximum).  The error
C            message should contain the name of the routine in which
C            the error occurred, followed by a blank, then a hyphen
C            (minus sign), then another blank, and then a short
C            description of the error.
C   NERRF  - the error number - must be non-zero.
C   IROPT  - the option - must have IROPT = 1 or 2.
C
C Error states:
C
C   1 - message length not positive.
C   2 - NERRF equal to 0.
C   3 - an unrecovered error followed by another error.
C   4 - bad value for IROPT (less than 1 or greater than 2).
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL SEBLDA
C
C The unit number for error messages is I1MACH(4).  Save that value,
C if it has not already been done.
C
        IF (IERRU.EQ.0) IERRU=I1MACH(4)
C
C Check for various error conditions.  The low-order bits of IERRC
C are used to keep track of which such errors have occurred.
C
        IERRC=0
C
C Check for a message of length zero or less.
C
        IF (LEN(MESSG).LE.0) THEN
          IERRC=IERRC+1
          WRITE (IERRU,1001)
        END IF
C
C Check for NERRF = 0.
C
        IF (NERRF.EQ.0) THEN
          IERRC=IERRC+2
          WRITE (IERRU,1002)
        END IF
C
C Check for a previous unrecovered error.
C
        IF (IERRF.NE.0) THEN
          IERRC=IERRC+4
          WRITE (IERRU,1003)
        END IF
C
C Check for an illegal value of the recovery flag.
C
        IF (IROPT.NE.1.AND.IROPT.NE.2) THEN
          IERRC=IERRC+8
          WRITE (IERRU,1004)
        END IF
C
C If one of the error conditions applies, print the appropriate
C information and quit.
C
        IF (IERRC.NE.0) THEN
          IF (MOD(IERRC/4,2).NE.0) THEN
            WRITE (IERRU,1005) IERRF,ERMSG(1:LOMSG)
          END IF
          IF (MOD(IERRC,2).EQ.0) THEN
            WRITE (IERRU,1006) NERRF,MESSG(1:ICLOEM(MESSG))
          END IF
          CALL FDUM
          STOP
        END IF
C
C Save the error message and error number.
C
        IERRF=NERRF
        ERMSG=MESSG
        LOMSG=ICLOEM(ERMSG)
C
C If recovery mode is activated and the error is recoverable, return
C to the caller for recovery action.
C
        IF (IRECF.EQ.1.AND.IROPT.EQ.1) RETURN
C
C Otherwise, print the error message.
C
        WRITE (IERRU,1007) IERRF,ERMSG(1:LOMSG)
C
C If the error is fatal, call the dump routine.
C
        IF (IROPT.EQ.2) CALL FDUM
C
C Quit.
C
        STOP
C
C Formats used above.
C
 1001 FORMAT (' ERROR    1 IN SETER - MESSAGE LENGTH IS NOT POSITIVE')
 1002 FORMAT (' ERROR    2 IN SETER - ILLEGAL VALUE FOR ERROR NUMBER')
 1003 FORMAT (' ERROR    3 IN SETER - AN UNCLEARED PRIOR ERROR EXISTS')
 1004 FORMAT (' ERROR    4 IN SETER - ILLEGAL VALUE FOR RECOVERY FLAG')
 1005 FORMAT (' ... MESSAGE FOR UNCLEARED PRIOR ERROR IS AS FOLLOWS:'/
     +        ' ... ERROR ',I4,' IN ',A)
 1006 FORMAT (' ... MESSAGE FOR CURRENT CALL TO SETER IS AS FOLLOWS:'/
     +        ' ... ERROR ',I4,' IN ',A)
 1007 FORMAT (' ERROR ',I4,' IN ',A)
C
      END
C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION ICFELL (MESSG,NERRF)
C
        CHARACTER*(*) MESSG
C
C ICFELL (which stands for "I Check For Errors on Lower Level") is used
C to check for the occurrence of a recoverable error in a lower-level
C routine and (perhaps) to update the current error flag and message.
C
C The old value of the current error flag is returned as the value of
C the function ICFELL.  If that value is zero, nothing else has been
C done.  If the value of ICFELL is non-zero, the following actions have
C been taken:
C
C   If MESSG is blank, the current error message has not been changed.
C
C   If MESSG is non-blank and its length is 6 or less, it should be the
C   name of the routine referencing ICFELL; the current error message
C   has been altered by prepending first a slash and then the value of
C   MESSG.  (Note that, if an error occurs several levels deep, the
C   effect of using ICFELL at each level is, effectively, to generate
C   traceback information in the error message.)
C
C   If MESSG is non-blank and its length is 7 or greater, its value
C   has become the new value of the current error message and the
C   previous error message has been printed.  This is used at the
C   beginning of an NCAR Graphics routine to check for an outstanding
C   error that the user has not recovered from and to ensure that the
C   message for the outstanding error gets printed.
C
C   If the expression NERRF has the value zero, the current error
C   flag has not been changed.
C
C   If the expression NERRF has a non-zero value, the value of the
C   current error flag has been made equal to that value.
C
C An example:  Assume that the routine "A" calls the routine "B" and
C that "B" detects an error and calls SETER with error number "32" and
C error message "B - ERROR HAS OCCURRED".  If recovery mode is not in
C effect, SETER prints the error message and STOPs, but, if recovery
C mode is in effect, control returns from "SETER" to "B" and thence to
C "A".  At that point, the statement
C
C   IF (ICFELL('A',13).NE.0) RETURN
C
C detects the fact that an error has occurred in "B" and results in a
C return from "A" to whatever routine called it.  It also changes the
C current error message to read "A/B - ERROR HAS OCCURRED" and changes
C the error number from "32" to "13".
C
C Another example:  Assume that the NCAR Graphics routine "A" is called
C when recovery mode is set and that it detects an error, calls SETER,
C and RETURNs to the user.  If the user neglects to check the error
C state and calls the routine "B" next, the statement
C
C   IF (ICFELL('B - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C ensures that the error message from routine "A" will be printed, that
C it will be replaced by "B - UNCLEARED PRIOR ERROR", and that "B" will
C not attempt to execute.
C
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C Define a character temporary to use.
C
        CHARACTER*256 CTEMP
C
C Set the value of the function to the current value of the error flag.
C
        ICFELL=IERRF
C
C If the current error flag is non-zero, update its value and the value
C of the error message as directed by the values of the arguments of
C ICFELL.
C
        IF (IERRF.NE.0) THEN
C
          IF (MESSG.NE.' ') THEN
C
            LENMSG=ICLOEM(MESSG)
C
            IF (LENMSG.LE.6) THEN
              CTEMP=ERMSG
              ERMSG=MESSG(1:LENMSG)//'/'//CTEMP
            ELSE
              CALL EPRIN
              ERMSG=MESSG
            END IF
C
            LOMSG=ICLOEM(ERMSG)
C
          END IF
C
          IF (NERRF.NE.0) IERRF=NERRF
C
        END IF
C
C Done.
C
        RETURN
C
      END
C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      FUNCTION ICLOEM (MESSG)
C
        CHARACTER*(*) MESSG
C
C ICLOEM(MESSG) is the index of the last non-blank character in MESSG.
C
        DO 101 I=LEN(MESSG),1,-1
         IF (MESSG(I:I).NE.' ') THEN
           ICLOEM=I
           RETURN
         END IF
  101   CONTINUE
C
        ICLOEM=1
C
        RETURN
C
      END
C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FDUM
C
C This routine is called when a fatal error occurs.  The default
C version does nothing.  A version may be supplied by the user to
C do something more useful.
C
        RETURN
C
      END
      INTEGER FUNCTION I1MACH(I)
      INTEGER I
C
C    I1MACH( 1) = THE STANDARD INPUT UNIT.
C    I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C    I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    I1MACH( 6) = THE NUMBER OF CHARACTERS PER CHARACTER STORAGE UNIT.
C    INTEGERS HAVE FORM SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C    I1MACH( 7) = A, THE BASE.
C    I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C    FLOATS HAVE FORM  SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C               WHERE  EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, THE BASE.
C  SINGLE-PRECISION
C    I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C  DOUBLE-PRECISION
C    I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
      INTEGER IMACH(16), OUTPUT, SC, SMALL(2)
      SAVE IMACH, SC
      REAL RMACH
      EQUIVALENCE (IMACH(4),OUTPUT), (RMACH,SMALL(1))
      INTEGER I3, J, K, T3E(3)
      DATA T3E(1) / 9777664 /
      DATA T3E(2) / 5323660 /
      DATA T3E(3) / 46980 /
C  THIS VERSION ADAPTS AUTOMATICALLY TO MOST CURRENT MACHINES,
C  INCLUDING AUTO-DOUBLE COMPILERS.
C  TO COMPILE ON OLDER MACHINES, ADD A C IN COLUMN 1
C  ON THE NEXT LINE
      DATA SC/0/
C  AND REMOVE THE C FROM COLUMN 1 IN ONE OF THE SECTIONS BELOW.
C  CONSTANTS FOR EVEN OLDER MACHINES CAN BE OBTAINED BY
C          mail netlib@research.bell-labs.com
C          send old1mach from blas
C  PLEASE SEND CORRECTIONS TO dmg OR ehg@bell-labs.com.
C
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SC/987/
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /, SC/987/
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
C
C     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 7
C     WHICH IS APPROPRIATE FOR THE UNIVAC-FOR SYSTEM.
C     IF YOU HAVE THE UNIVAC-FTN SYSTEM, SET IT TO 1.
C
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    6 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /, SC/987/
C
      IF (SC .NE. 987) THEN
*        *** CHECK FOR AUTODOUBLE ***
         SMALL(2) = 0
         RMACH = 1E13
         IF (SMALL(2) .NE. 0) THEN
*           *** AUTODOUBLED ***
            IF (      (SMALL(1) .EQ. 1117925532
     *           .AND. SMALL(2) .EQ. -448790528)
     *       .OR.     (SMALL(2) .EQ. 1117925532
     *           .AND. SMALL(1) .EQ. -448790528)) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
            ELSE IF ( SMALL(1) .EQ. -2065213935
     *          .AND. SMALL(2) .EQ. 10752) THEN
*               *** VAX WITH D_FLOATING ***
               IMACH(10) = 2
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
            ELSE IF ( SMALL(1) .EQ. 1267827943
     *          .AND. SMALL(2) .EQ. 704643072) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
            ELSE
               WRITE(*,9010)
               STOP 777
               END IF
            IMACH(11) = IMACH(14)
            IMACH(12) = IMACH(15)
            IMACH(13) = IMACH(16)
         ELSE
            RMACH = 1234567.
            IF (SMALL(1) .EQ. 1234613304) THEN
*               *** IEEE ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -125
               IMACH(13) = 128
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
               SC = 987
            ELSE IF (SMALL(1) .EQ. -1271379306) THEN
*               *** VAX ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -127
               IMACH(13) = 127
               IMACH(14) = 56
               IMACH(15) = -127
               IMACH(16) = 127
               SC = 987
            ELSE IF (SMALL(1) .EQ. 1175639687) THEN
*               *** IBM MAINFRAME ***
               IMACH(10) = 16
               IMACH(11) = 6
               IMACH(12) = -64
               IMACH(13) = 63
               IMACH(14) = 14
               IMACH(15) = -64
               IMACH(16) = 63
               SC = 987
            ELSE IF (SMALL(1) .EQ. 1251390520) THEN
*              *** CONVEX C-1 ***
               IMACH(10) = 2
               IMACH(11) = 24
               IMACH(12) = -128
               IMACH(13) = 127
               IMACH(14) = 53
               IMACH(15) = -1024
               IMACH(16) = 1023
            ELSE
               DO 10 I3 = 1, 3
                  J = SMALL(1) / 10000000
                  K = SMALL(1) - 10000000*J
                  IF (K .NE. T3E(I3)) GO TO 20
                  SMALL(1) = J
 10               CONTINUE
*              *** CRAY T3E ***
               IMACH( 1) = 5
               IMACH( 2) = 6
               IMACH( 3) = 0
C
C Note: the original version returns '6' for IMACH(4). But, we've
C been using '0' for years, so we hard-coded it to '0' here.
C
               IMACH( 4) = 0
               IMACH( 5) = 64
               IMACH( 6) = 8
               IMACH( 7) = 2
               IMACH( 8) = 63
               CALL I1MCR1(IMACH(9), K, 32767, 16777215, 16777215)
               IMACH(10) = 2
               IMACH(11) = 53
               IMACH(12) = -1021
               IMACH(13) = 1024
               IMACH(14) = 53
               IMACH(15) = -1021
               IMACH(16) = 1024
               GO TO 35
 20            CALL I1MCR1(J, K, 16405, 9876536, 0)
               IF (SMALL(1) .NE. J) THEN
                  WRITE(*,9020)
                  STOP 777
                  END IF
*              *** CRAY 1, XMP, 2, AND 3 ***
               IMACH(1) = 5
               IMACH(2) = 6
               IMACH(3) = 102
               IMACH(4) = 6
               IMACH(5) = 46
               IMACH(6) = 8
               IMACH(7) = 2
               IMACH(8) = 45
               CALL I1MCR1(IMACH(9), K, 0, 4194303, 16777215)
               IMACH(10) = 2
               IMACH(11) = 47
               IMACH(12) = -8188
               IMACH(13) = 8189
               IMACH(14) = 94
               IMACH(15) = -8141
               IMACH(16) = 8189
               GO TO 35
               END IF
            END IF
         IMACH( 1) = 5
         IMACH( 2) = 6
         IMACH( 3) = 7
         IMACH( 4) = 0
         IMACH( 5) = 32
         IMACH( 6) = 4
         IMACH( 7) = 2
         IMACH( 8) = 31
         IMACH( 9) = 2147483647
 35      SC = 987
         END IF
 9010 FORMAT(/' Adjust autodoubled I1MACH by uncommenting data'/
     * ' statements appropriate for your machine and setting'/
     * ' IMACH(I) = IMACH(I+3) for I = 11, 12, and 13.')
 9020 FORMAT(/' Adjust I1MACH by uncommenting data statements'/
     * ' appropriate for your machine.')
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 40
      I1MACH = IMACH(I)
      RETURN
 40   WRITE(*,*) 'I1MACH(I): I =',I,' is out of bounds.'
      STOP
* /* C source for I1MACH -- remove the * in column 1 */
* /* Note that some values may need changing. */
*#include <stdio.h>
*#include <float.h>
*#include <limits.h>
*#include <math.h>
*
*long i1mach_(long *i)
*{
*	switch(*i){
*	  case 1:  return 5;	/* standard input */
*	  case 2:  return 6;	/* standard output */
*	  case 3:  return 7;	/* standard punch */
*	  case 4:  return 0;	/* standard error */
*	  case 5:  return 32;	/* bits per integer */
*	  case 6:  return sizeof(int);
*	  case 7:  return 2;	/* base for integers */
*	  case 8:  return 31;	/* digits of integer base */
*	  case 9:  return LONG_MAX;
*	  case 10: return FLT_RADIX;
*	  case 11: return FLT_MANT_DIG;
*	  case 12: return FLT_MIN_EXP;
*	  case 13: return FLT_MAX_EXP;
*	  case 14: return DBL_MANT_DIG;
*	  case 15: return DBL_MIN_EXP;
*	  case 16: return DBL_MAX_EXP;
*	  }
*	fprintf(stderr, "invalid argument: i1mach(%ld)\n", *i);
*	exit(1);return 0; /* some compilers demand return values */
*}
      END
      SUBROUTINE I1MCR1(A, A1, B, C, D)
**** SPECIAL COMPUTATION FOR OLD CRAY MACHINES ****
      INTEGER A, A1, B, C, D
      A1 = 16777216*B + C
      A = 16777216*A1 + D
      END
C
C $Id: llmisc.f,v 1.1 2009-05-15 00:49:27 dbrown Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SEBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA SEBLDAX
C
C The common blocks SECOMI and SECOMC are used to hold shared variables
C of types INTEGER and CHARACTER, respectively, for the routine SETER
C and associated routines.  For descriptions of these variables and for
C default values of them, see the block data routine SEBLDAX.
C
        COMMON /SECOMI/ IERRU,IERRF,IRECF,LOMSG
        SAVE   /SECOMI/
C
        COMMON /SECOMC/ ERMSG
          CHARACTER*256 ERMSG
        SAVE   /SECOMC/
C
C IERRU is the logical unit for error messages.  Its default value is
C zero, which serves as a signal to SETER that the proper value should
C be gotten from I1MACH.
C
        DATA IERRU / 0 /
C
C IERRF is the error flag.  Initially, it is zero.  A non-zero value
C means that there is an uncleared prior error.
C
        DATA IERRF / 0 /
C
C IRECF is the recovery flag, which can have values of 1, which turns
C recovery mode on, or 2, which turns recovery mode off and causes all
C errors to be treated as fatal errors.  The latter is the default.
C
        DATA IRECF / 2 /
C
C LOMSG is the actual length of the error message in ERMSG.
C
        DATA LOMSG / 1 /
C
C ERMSG is the text of the current error message, only meaningful when
C IERRF is non-zero.
C
        DATA ERMSG / ' ' /
C
      END
