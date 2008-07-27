C
C $Id: icfell.f,v 1.7 2008-07-27 00:17:29 haley Exp $
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
