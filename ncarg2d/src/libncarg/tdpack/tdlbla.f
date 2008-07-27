C
C $Id: tdlbla.f,v 1.5 2008-07-27 00:17:32 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDLBLA (IAXS,ILBL,NLBL,XAT0,XAT1,YAT0,YAT1,ANGD)
C
        CHARACTER*(*) ILBL,NLBL
C
C This routine is called to put labels on a particular edge of a box.
C It is assumed that TDPARA has been called to define a rectangle in
C 3-space lying in one corner of one face of the box being labelled.
C The sides of this rectangle are assumed to be vectors of length 1,
C (that is to say, the rectangle defines a unit square within that
C face of the box).  The arguments are as follows:
C
C   IAXS is an input integer saying which edge of the face is being
C   labelled (1 => left, 2 => right, 3 => bottom, and 4 => top).  The
C   meaning of "left", "right", "bottom", and "top" are defined by the
C   orientation of the rectangle defined by the last call to TDPARA.
C
C   ILBL is an input character string to be used as an informational
C   label.  If the string is blank, no informational label is written.
C
C   NLBL is an input character string containing numeric labels.  The
C   labels need not be in any particular order, but they have to be
C   separated by blanks and each has to be readable using a FORTRAN
C   format of the form "En.0", where "n" is the length of the label.
C   If the string is blank, no informational label is written.
C
C   XAT0 and XAT1 are the real values of "X" associated with the left
C   and right edges of the face being labelled.
C
C   YAT0 and YAT1 are the real values of "Y" associated with the bottom
C   and top edges of the face being labelled.
C
C   ANGD is an input real specifying the angle, in degrees, at which
C   the labels are to be written.  This angle is defined with reference
C   to the parallelogram defined by the last call to TDPARA.
C
C Declare a required TDPACK common block.
C
        COMMON /TDCOM4/ CSM1,CSM2
        SAVE   /TDCOM4/
C
C Declare a variable into which to encode a number-reading format.
C
        CHARACTER*7 FORM
C
C Define various positioning parameters for labels on the four sides
C of the box face.
C
        DIMENSION XPIL(4),YPIL(4),XOIL(4),YOIL(4),PONL(4),OONL(4),
     +            PTKS(4),OTKE(4)
C
        DATA XPIL / 0.000 , 1.000 , 0.500 , 0.500 /
        DATA YPIL / 0.500 , 0.500 , 0.000 , 1.000 /
        DATA XOIL / -.110 , +.110 , 0.000 , 0.000 /
        DATA YOIL / 0.000 , 0.000 , -.110 , +.110 /
        DATA PONL / 0.000 , 1.000 , 0.000 , 1.000 /
        DATA OONL / -.045 , +.045 , -.045 , +.045 /
        DATA PTKS / 0.000 , 1.000 , 0.000 , 1.000 /
        DATA OTKE / -.015 , +.015 , -.015 , +.015 /
C
        XRNG=ABS(XAT1-XAT0)
        YRNG=ABS(YAT1-YAT0)
C
C Output the informational label, if any.
C
        IF (ILBL.NE.' ')
     +    CALL TDPLCH (XPIL(IAXS)*XRNG+XOIL(IAXS)*CSM2,
     +                 YPIL(IAXS)*YRNG+YOIL(IAXS)*CSM2,
     +                 ILBL(1:LNBPCS(ILBL)),.04*CSM2,ANGD,0.)
C
C Output the numeric labels, if any.
C
        IF (NLBL.NE.' ') THEN
          NCHS=LEN(NLBL)
          IF (NLBL(1:1).EQ.' '.OR.NLBL(1:1).EQ.'+'.OR.
     +        NLBL(1:1).EQ.'-'.OR.NLBL(1:1).EQ.'.'.OR.
     +        NLBL(1:1).EQ.'0'.OR.NLBL(1:1).EQ.'1'.OR.
     +        NLBL(1:1).EQ.'2'.OR.NLBL(1:1).EQ.'3'.OR.
     +        NLBL(1:1).EQ.'4'.OR.NLBL(1:1).EQ.'5'.OR.
     +        NLBL(1:1).EQ.'6'.OR.NLBL(1:1).EQ.'7'.OR.
     +        NLBL(1:1).EQ.'8'.OR.NLBL(1:1).EQ.'9'.OR.
     +        NLBL(1:1).EQ.'*') THEN
            IOFC=1
          ELSE
            IOFC=2
          END IF
          IBEG=0
          DO 101 I=IOFC,NCHS
            IF (NLBL(I:I).NE.' '.AND.NLBL(I:I).NE.'*') THEN
              IF (IBEG.EQ.0) IBEG=I
              IEND=I
            END IF
            IF (IBEG.NE.0) THEN
              IF (NLBL(I:I).EQ.' '.OR.
     +            NLBL(I:I).EQ.'*'.OR.I.EQ.NCHS) THEN
                WRITE (FORM,'(''(E'',I2,''.0)'')') IEND-IBEG+1
                READ  (NLBL(IBEG:IEND),FORM) RVAL
                IF (IOFC.NE.1) CALL TDLBLP (NLBL(1:1),RVAL,RVAL)
                IF (IAXS.EQ.1.OR.IAXS.EQ.2) THEN
                  RVAL=(RVAL-YAT0)*SIGN(1.,YAT1-YAT0)
                  CALL TDPLCH (PONL(IAXS)*XRNG+OONL(IAXS)*CSM2,RVAL,
     +                         NLBL(IBEG:IEND),.03*CSM2,ANGD,0.)
                  CALL TDLNPA (PTKS(IAXS)*XRNG                ,RVAL,
     +                         PTKS(IAXS)*XRNG+OTKE(IAXS)*CSM2,RVAL)
                ELSE
                  RVAL=(RVAL-XAT0)*SIGN(1.,XAT1-XAT0)
                  CALL TDPLCH (RVAL,PONL(IAXS)*YRNG+OONL(IAXS)*CSM2,
     +                         NLBL(IBEG:IEND),.03*CSM2,ANGD,0.)
                  CALL TDLNPA (RVAL,PTKS(IAXS)*YRNG                ,
     +                         RVAL,PTKS(IAXS)*YRNG+OTKE(IAXS)*CSM2)
                END IF
                IBEG=0
              END IF
            END IF
  101     CONTINUE
        END IF
C
C Done.
C
        RETURN
C
      END
