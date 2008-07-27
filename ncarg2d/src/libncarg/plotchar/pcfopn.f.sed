C
C $Id: pcfopn.f.sed,v 1.6 2008-07-27 01:15:51 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCFOPN (IBNU,NFNT)
C
C Declare space in which to acquire the database directory path.
C
        CHARACTER*1024 FLNM
C
C Declare a character temporary of 113 characters (the length of the
C largest error message SETER can handle).
C
        CHARACTER*113 CTMP
C
C See what the call to PCFOPN was meant to do.
C
        IF (NFNT.EQ.0) THEN
C
C The call to PCFOPN is to open the default PLOTCHAR dataset.
C
C Get the database directory path.
C
          FLNM=' '
          CALL GNGPAT (FLNM,'SED_DBDIR',ISTA)
C
C Test for possible error by GNGPAT.
C
          IF (ISTA.NE.-1) THEN
C
C No error by GNGPAT.  Try to append the database name and jump to
C open the database.
C
            DO 101 I=1,LEN(FLNM)-13
              IF (FLNM(I:I).EQ.CHAR(0)) THEN
                FLNM(I:I+13)='/PlotcharData'//CHAR(0)
                LOFN=I+12
                GO TO 104
              END IF
  101       CONTINUE
C
C Database name too long.  Jump to issue an error message.
C
            LOFN=LEN(FLNM)-12
            GO TO 105
C
          ELSE
C
C Error by GNGPAT.  What is returned in FLNM is its error message.
C Form a complete error message, call SETER with it, and return.
C
            DO 102 I=2,LEN(FLNM)
              LOEM=I
              IF (FLNM(I:I).EQ.CHAR(0)) GO TO 103
  102       CONTINUE
C
  103       CTMP='PCFOPN/GNGPAT - '//FLNM(1:LOEM-1)
            CALL SETER (CTMP(1:MIN(LOEM+15,113)),1,1)
            RETURN
C
          END IF
C
C Attempt to open the default PLOTCHAR database.  If an error occurs,
C jump to issue an error message.
C
  104     CALL NGOFRO (FLNM,IBNU,ISTA)
          IF (ISTA.NE.0) GO TO 105
C
        ELSE
C
C The call to PCFOPN is to open one of the fontcaps.  Try to open it.
C
          CALL BOFRED (IBNU,NFNT,IOST,ISTA)
C
C If an error occurs, call SETER with an appropriate error message.
C
          IF (ISTA.NE.0) THEN
            WRITE (CTMP,'(''PCFOPN - ERROR OPENING FONT'',I5)') NFNT
            CALL SETER (CTMP(1:32),2,1)
            RETURN
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
C An error has occurred in attempting to open the default PLOTCHAR
C database.  FLNM contains a path name, which may or may not be
C complete.  In any case, it may be too C long to be included in
C the error message issued.  Deal with it.
C
  105   CTMP='PCFOPN - ERROR OPENING PWRITX DATA FILE '//FLNM
        LOEM=40+LOFN
        IF (LOEM.GT.113) THEN
          CTMP(98:113)='.../PlotcharData'
          LOEM=113
        END IF
        CALL SETER (CTMP(1:LOEM),3,1)
        RETURN
C
      END
