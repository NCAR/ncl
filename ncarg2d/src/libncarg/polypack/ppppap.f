C
C $Id: ppppap.f,v 1.1 1994-06-20 19:49:03 kennison Exp $
C
      SUBROUTINE PPPPAP (XCOP,YCOP,NCOP,NBTS)
C
C This routine may be called to pre-process a polygon that is to be
C used as input to one of the polygon-manipulation routines.  The
C polygon is defined by the points (XCOP(I),YCOP(I)), for I = 1 to
C NCOP.  NBTS is the number of significant bits to be left in the
C fractional parts of the point coordinates; you should probably not
C use a value less than about 10 (?) nor one greater than 24 on a
C machine with 32-bit reals or greater than 48 on a machine with
C 64-bit reals.  For most purposes, NBTS = 18 is probably okay.
C
      DIMENSION XCOP(NCOP),YCOP(NCOP)
C
C Reduce the number of significant bits in each point coordinate to
C NBTS by zeroing the remaining bits.  This is useful in avoiding a
C problem that occurs in the trapezoid-producing routines when there
C are edge segments that are very nearly, but not quite, horizontal.
C
      DO 10001 I=1,NCOP
        XCOP(I)=PPZBTS(XCOP(I),NBTS)
        YCOP(I)=PPZBTS(YCOP(I),NBTS)
10001 CONTINUE
C
C Cull adjacent points points that are identical.  This step is
C probably unnecessary now, as I no longer know of any problem
C caused by adjacent identical points, but it does no harm.
C
      NOUT=1
C
      DO 10002 I=2,NCOP
        IF (.NOT.(XCOP(I).NE.XCOP(I-1).OR.YCOP(I).NE.YCOP(I-1)))
     +  GO TO 10003
          NOUT=NOUT+1
          IF (.NOT.(NOUT.NE.I)) GO TO 10004
            XCOP(NOUT)=XCOP(I)
            YCOP(NOUT)=YCOP(I)
10004     CONTINUE
10003   CONTINUE
10002 CONTINUE
C
      NCOP=NOUT
C
C Done.
C
      RETURN
C
      END
