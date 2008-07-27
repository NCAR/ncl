C
C $Id: seter.f,v 1.8 2008-07-27 00:17:31 haley Exp $
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
C default values of them, see the block data routine SEBLDA.
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
