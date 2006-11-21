CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0
C   Copyright (C) 1995-2004, Scientific Computing Division,
C   University Corporation for Atmospheric Research
C   Licensed under the GNU General Public License (GPL)
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id: xerfft.f,v 1.2 2006-11-21 01:10:20 haley Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE DXERFFT(SRNAME,INFO)
C
C     .. Scalar Arguments ..
      CHARACTER*6 SRNAME
      INTEGER INFO
C
C     ..
C
C  Purpose
C  =======
C
C  XERFFT  is an error handler for library FFTPACK version 5.0 routines.
C  It is called by an FFTPACK 5.0 routine if an input parameter has an
C  invalid value.  A message is printed and execution stops.
C
C  Installers may consider modifying the STOP statement in order to
C  call system-specific exception-handling facilities.
C
C  Arguments
C  =========
C
C  SRNAME  (input) CHARACTER*6
C          The name of the routine which called XERFFT.
C
C  INFO    (input) INTEGER
C          When a single  invalid parameter in the parameter list of
C          the calling routine has been detected, INFO is the position
C          of that parameter.  In the case when an illegal combination
C          of LOT, JUMP, N, and INC has been detected, the calling
C          subprogram calls XERFFT with INFO = -1.
C
C =====================================================================
C
C     .. Executable Statements ..
C
      IF (INFO.GE.1) THEN
          WRITE (*,FMT='(A,A,A,I3,A)') ' ** On entry to ',SRNAME,
     +      ' parameter number ',INFO,' had an illegal value'
      ELSE IF (INFO.EQ.-1) THEN
          WRITE (*,FMT='(A,A,A,A)') ' ** On entry to ',SRNAME,
     +      ' parameters LOT, JUMP, N and INC are inconsistent'
      ELSE IF (INFO.EQ.-2) THEN
          WRITE (*,FMT='(A,A,A,A)') ' ** On entry to ',SRNAME,
     +      ' parameter L is greater than LDIM'
      ELSE IF (INFO.EQ.-3) THEN
          WRITE (*,FMT='(A,A,A,A)') ' ** On entry to ',SRNAME,
     +      ' parameter M is greater than MDIM'
      ELSE IF (INFO.EQ.-5) THEN
          WRITE (*,FMT='(A,A,A,A)') ' ** Within ',SRNAME,
     +      ' input error returned by lower level routine'
      ELSE IF (INFO.EQ.-6) THEN
          WRITE (*,FMT='(A,A,A,A)') ' ** On entry to ',SRNAME,
     +      ' parameter LDIM is less than 2*(L/2+1)'
      END IF
*
      STOP
*
*     End of XERFFT
*
      END
