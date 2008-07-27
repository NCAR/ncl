C
C	$Id: gtxtat.f,v 1.6 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
        SUBROUTINE GTXTAT(IOS,STATUS)
C
C  Set the TEXT attributes.
C
C  Valid attributes:
C    TEXT INDEX BUNDLE
C    TEXT FONT INDEX
C    TEXT PRECISION
C    CHARACTER EXPANSION FACTOR
C    CHARACTER SPACING
C    TEXT COLOR
C    CHARACTER HEIGHT
C    CHARACTER ORIENTATION
C    CHARACTER PATH
C    TEXT ALIGNMENT
C
      include 'trstat.h'
      include 'trpars.h'
      include 'trinst.h'
      include 'trcode.h'
      include 'gkscom.h'
C
      INTEGER IOS, STATUS
      INTEGER TEMP
C
      IF (OPID .EQ. ATELTI) THEN
C
C  TEXT BUNDLE INDEX
C
        CALL GOPDEC(TXTIDX,MIXCPR,1,IOS,STATUS)
        CALL GSTXI(TXTIDX)
      ELSE IF (OPID .EQ. ATELTF) THEN
C
C  FONT INDEX
C
        CALL GOPDEC(TEMP,MIXCPR,1,IOS,STATUS)
        CALL GENNEG(TEMP,FINDEX)
        IF (FINDEX .LE. 0) FINDEX = 1
C
C  Set the font index to point to the appropriate internal font table.
C
        CALL GQTXFP(IER,IFNT,IPREC)
        CALL GSTXFP(FINDEX,IPREC)
C
      ELSE IF (OPID .EQ. ATELTP) THEN
C
C  TEXT PRECISION
C
        CALL GOPDEC(TXTPRE,MENCPR,1,IOS,STATUS)
        CALL GQTXFP(IER,IFNT,IPREC)
        CALL GSTXFP(IFNT,TXTPRE)
      ELSE IF (OPID .EQ. ATELCE) THEN
C
C  CHARACTER EXPANSION FACTOR
C
        CALL GTFLT(CEXPN,MFLCPR,IOS,STATUS)
        CALL GSCHXP(CEXPN)
      ELSE IF (OPID .EQ. ATELCS) THEN
C
C  CHARACTER SPACING
C
        CALL GTFLT(CSPACE,MFLCPR,IOS,STATUS)
        CALL GSCHSP(CSPACE)
      ELSE IF (OPID .EQ. ATELTC) THEN
C
C  Set the text color.
C
        CALL GOPDEC(TXTCOL,MCICPR,1,IOS,STATUS)
        CALL GSTXCI(TXTCOL)
      ELSE IF (OPID .EQ. ATELCH) THEN
C
C  CHARACTER HEIGHT
C
        CALL GOPDEC(CHIGHT,MOPLEN,1,IOS,STATUS)
        XHIGHT = REAL(CHIGHT)/32767.
C
C  Scale the character height by the original scale factor in the
C  Y direction.  Take a vactor (0,XHIGHT), transform it with the
C  current transformation matrix, then find the length of the
C  transformed vector.
C
        YSCALE = SQRT(CURTM(1,2)*CURTM(1,2)+CURTM(2,2)*CURTM(2,2))
        HN = XHIGHT*YSCALE
        IF (HN .LT. 0.00004) HN = 0.00004
        CALL GSCHH(HN)
      ELSE IF (OPID .EQ. ATELCO) THEN
C
C  CHARACTER ORIENTATION (given as up and base vectors).
C
        CALL GOPDEC(TEMP,MOPLEN,1,IOS,STATUS)
        CALL GENNEG(TEMP,XU)
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(TEMP,MOPLEN,1,IOS,STATUS)
        CALL GENNEG(TEMP,YU)
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(TEMP,MOPLEN,1,IOS,STATUS)
        CALL GENNEG(TEMP,XB)
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(TEMP,MOPLEN,1,IOS,STATUS)
        CALL GENNEG(TEMP,YB)
C
C  Modify the up vector by the current segment transformation.
C
        RXUT = CURTM(1,1)*REAL(XU)+CURTM(1,2)*REAL(YU)
        RYUT = CURTM(2,1)*REAL(XU)+CURTM(2,2)*REAL(YU)
C
C  If the modified values are both zero, then the transformation 
C  scaling is such as to make the character orientation of no 
C  observable consequence, so set it to horizontal.
C
        IF (RXUT.EQ.0. .AND. RYUT.EQ.0.) THEN
          RXUT = 0.
          RYUT = 1.
        ENDIF
        CALL GSCHUP(RXUT,RYUT)
      ELSE IF (OPID.EQ.ATELTH) THEN
C
C  TEXT PATH
C
        CALL GOPDEC(PATH,MENCPR,1,IOS,STATUS)
        CALL GSTXP(PATH)
      ELSE IF (OPID .EQ. ATELTA) THEN
C
C  TEXT ALIGNMENT
C
        CALL GOPDEC(HORIZ,MENCPR,1,IOS,STATUS)
        IF (STATUS .NE. 0) RETURN
        CALL GOPDEC(VERT,MENCPR,1,IOS,STATUS)
        CALL GSTXAL(HORIZ,VERT)
C
C  Skip over the rest of the alignment information.
C
        CALL GSKPOP(8,LEN-(MENCPR*2/8),IOS,STATUS)
      END IF
C
      RETURN
      END
