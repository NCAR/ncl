C
C	$Id: gcsgwk.f,v 1.9 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GCSGWK(WKID,SGNA)
C
C  COPY SEGMENT TO WORKSTATION.
C
      INTEGER ECSGWK
      PARAMETER (ECSGWK=62)
C
      include 'gkscom.h'
      include 'trstat.h'
C
      INTEGER WKID,SGNA,WKTP,WKCT,FCODEO,CONTO
C
C  This subroutine is here solely as support for the SPPS GFLASn
C  entries.  Full segmentation is not a part of the NCAR GKS
C  package at this time.  The NCAR package is non-standard to the
C  extent that certain segmentation functions are supported, but
C  not all level 1 functions are supported.  This subroutine should
C  be considered a user entry point only by way of the GFLASn
C  calls--it should never be called directly be the user.
C
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(6,ECSGWK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the workstation identifier is valid.
C
      CALL GZCKWK(20,ECSGWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,ECSGWK,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if WISS is open.
C
      DO 200 I=1,NOPWK
        CALL GQWKCA(SWKTP(I),IER,IWCAT)
        IF (IWCAT .EQ. GWISS) GO TO 10
  200 CONTINUE
      ERS = 1
      CALL GERHND(27,ECSGWK,ERF)
      ERS = 0
      RETURN
C
   10 CONTINUE
C
C  Determine the workstation type.
C
      DO 201 I=1,NOPWK
        IF (SOPWK(I) .EQ. WKID) THEN
        WKTP = SWKTP(I)
        GO TO 20
      ENDIF
  201 CONTINUE
   20 CONTINUE
C
C  Get the workstation category.
C
      CALL GQWKCA(WKTP,IER,WKCT)
C
C  Check if an MI workstation.
C
      IF (WKCT .EQ. GMI) THEN
        ERS = 1
        CALL GERHND(33,ECSGWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if input workstation.
C
      IF (WKCT .EQ. GINPUT) THEN
        ERS = 1
        CALL GERHND(35,ECSGWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if WISS workstation.
C
      IF (WKCT .EQ. GWISS) THEN
        ERS = 1
        CALL GERHND(36,ECSGWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the segment name is valid.
C
      IF (SGNA.LT.0 .OR. SGNA.GT.99) THEN
        ERS = 1
        CALL GERHND(120,ECSGWK,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the segment name is in WISS.
C
      DO 202 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) THEN
          STR = ' '
          STR = SEGNAM(I)
          GO TO 30
        ENDIF
  202 CONTINUE
      ERS = 1
      CALL GERHND(124,ECSGWK,ERF)
      ERS = 0
      RETURN
   30 CONTINUE
C
C  If the copy is to a non-CGM, or if the segment transformation
C  is not the identity, or if the clipping rectangle transform 
C  flag is set, do the copy by parsing the segments.
C
      DO 60 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) THEN
          MXSREC = SEGLEN(I)
C
C  Retrieve the current segment transformation.
C
          CURTM(1,1) = SEGT(I,1,1) 
          CURTM(1,2) = SEGT(I,1,2) 
          CURTM(1,3) = SEGT(I,1,3) 
          CURTM(2,1) = SEGT(I,2,1) 
          CURTM(2,2) = SEGT(I,2,2) 
          CURTM(2,3) = SEGT(I,2,3) 
          GO TO 70
        ENDIF
   60 CONTINUE
   70 CONTINUE
      IF (WKTP.NE.GCGM .OR. IGSGCP.NE.0 
     +                 .OR. CURTM(1,1).NE.1. .OR.CURTM(1,2).NE.0.
     +                 .OR. CURTM(1,3).NE.0. .OR.CURTM(2,1).NE.0.
     +                 .OR. CURTM(2,2).NE.1. .OR.CURTM(2,3).NE.0.
     +                 ) THEN
        CALL GZCPWK(WKID)
        RETURN
      ELSE
C
C  Copy to the metafile.
C
C
C  Invoke the workstation interface.
C
C  Put out a new picture initialization if the picture is empty.
C
        IF (NOPICT .LE. 0) THEN
          FCODEO = FCODE
          CONTO  = CONT
          FCODE  = 91
          CONT   =  0
          CALL G01WDR(WKID,' ')
          FCODE  = FCODEO
          CONT   = CONTO
          NOPICT = 1
        ENDIF
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
        CUFLAG = WKID
        FCODE = 82
        CONT  = 0
        CALL GZROI(0)
        IL1 = 3
        IL2 = 3
        ID(1) = WCONID
        ID(2) = SGNA
        DO 40 I=1,NUMSEG
          IF (SEGS(I) .EQ. SGNA) THEN
            ID(3) = SEGLEN(I)
            GO TO 50
          ENDIF
   40   CONTINUE
   50   CONTINUE
        STRL1 = 80
        STRL2 = 80
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,ECSGWK,ERF)
          ERS = 0
        ENDIF
      ENDIF
      CUFLAG = -1
C
      RETURN
      END
