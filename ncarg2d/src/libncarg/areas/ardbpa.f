C
C $Id: ardbpa.f,v 1.11 1995-04-19 17:20:04 kennison Exp $
C
      SUBROUTINE ARDBPA (IAMA,IGIP,LABL)
C
      DIMENSION IAMA(*)
C
      CHARACTER*(*) LABL
C
C The routine ARDBPA produces a picture of that part of the contents of
C the area map IAMA which belongs to the group IGIP; if IGIP is zero or
C negative, all groups of edges are shown.  The character string LABL
C will be used as a label for the picture.
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,IRC,RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C The common block ARCOM1 is used to communicate with the arrow-drawing
C routine ARDBDA.
C
      COMMON /ARCOM1/ DT
C
C Check for an uncleared prior error.
C
      IF (ICFELL('ARDBPA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pull out the length of the area map and check for initialization.
C
      LAMA=IAMA(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAMA(LAMA).NE.LAMA)) GO TO 10001
        CALL SETER ('ARDBPA - INITIALIZATION DONE IMPROPERLY',2,1)
        RETURN
10001 CONTINUE
C
C Save the current polyline color index and text color index.
C
      CALL GQPLCI (IERR,IPCI)
      IF (.NOT.(IERR.NE.0)) GO TO 10002
        CALL SETER ('ARDBPA - ERROR EXIT FROM GQPLCI',3,1)
        RETURN
10002 CONTINUE
      CALL GQTXCI (IERR,ITCI)
      IF (.NOT.(IERR.NE.0)) GO TO 10003
        CALL SETER ('ARDBPA - ERROR EXIT FROM GQTXCI',4,1)
        RETURN
10003 CONTINUE
C
C Save the current state of the SET call and switch to the fractional
C coordinate system.
C
      CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('ARDBPA',5).NE.0) RETURN
      CALL    SET (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)
      IF (ICFELL('ARDBPA',6).NE.0) RETURN
C
C Define colors to use for different kinds of edges, as follows:
C
C   COLOR      LEFT AREA IDENTIFIER          RIGHT AREA IDENTIFIER
C
C   Magenta    less than or equal to zero    less than or equal to zero
C   Yellow     less than or equal to zero    greater than zero
C   Cyan       greater than zero             less than or equal to zero
C   White      great than zero               greater than zero
C
C Gray is used for edges for which the group identifier is negated,
C which has a special meaning.
C
      CALL GSCR (1,IDC+1,1.,0.,1.)
      CALL GSCR (1,IDC+2,1.,1.,0.)
      CALL GSCR (1,IDC+3,0.,1.,1.)
      CALL GSCR (1,IDC+4,1.,1.,1.)
      CALL GSCR (1,IDC+5,.8,.8,.8)
C
C Switch to white initially.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('ARDBPA',7).NE.0) RETURN
      CALL GSPLCI (IDC+4)
      CALL GSTXCI (IDC+4)
      ICLO=IDC+4
C
C Put a label at the top of the plot.
C
      CALL PLCHLQ (.5,.98,LABL,.015,0.,0.)
      IF (ICFELL('ARDBPA',8).NE.0) RETURN
C
C Trace the edges in the area map, drawing arrows as we go.
C
      DT=0.
      INDX=8
      RXCN=.5
      RYCN=.5
C
  101 RXCO=RXCN
      RYCO=RYCN
C
      IF (.NOT.(INDX.LT.8.OR.INDX.GT.IAMA(5).OR.MOD(INDX-8,10).NE.0))
     +GO TO 10004
        CALL SETER ('ARDBPA - BAD POINTERS IN AREA MAP',9,1)
        RETURN
10004 CONTINUE
C
      RXCN=REAL(IAMA(INDX+1))/RLC
      RYCN=REAL(IAMA(INDX+2))/RLC
C
      IF (.NOT.(IAMA(INDX+7).NE.0)) GO TO 10005
        IGID=ABS(IAMA(INDX+7))
        IF (.NOT.(IGID.LT.IAMA(6))) GO TO 10006
          IGID=IAMA(IAMA(1)-IGID)/2
        GO TO 10007
10006   CONTINUE
          IGID=IAMA(IGID)/2
10007   CONTINUE
        IF (.NOT.(IGIP.LE.0.OR.IGID.EQ.IGIP)) GO TO 10008
          IAIL=IAMA(INDX+8)
          IF (IAIL.GT.0) IAIL=IAMA(IAIL)/2
          IAIR=IAMA(INDX+9)
          IF (IAIR.GT.0) IAIR=IAMA(IAIR)/2
          IF (.NOT.(IAMA(INDX+7).GT.0)) GO TO 10009
            IF (IAIL.LE.0.AND.IAIR.LE.0) ICLN=IDC+1
            IF (IAIL.LE.0.AND.IAIR.GT.0) ICLN=IDC+2
            IF (IAIL.GT.0.AND.IAIR.LE.0) ICLN=IDC+3
            IF (IAIL.GT.0.AND.IAIR.GT.0) ICLN=IDC+4
          GO TO 10010
10009     CONTINUE
            ICLN=IDC+5
10010     CONTINUE
          IF (.NOT.(ICLN.NE.ICLO)) GO TO 10011
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('ARDBPA',10).NE.0) RETURN
            CALL GSPLCI (ICLN)
            CALL GSTXCI (ICLN)
            ICLO=ICLN
10011     CONTINUE
          CALL ARDBDA (RXCO,RYCO,RXCN,RYCN,IAIL,IAIR,IGIP,IGID)
          IF (ICFELL('ARDBPA',11).NE.0) RETURN
10008   CONTINUE
      GO TO 10012
10005 CONTINUE
        DT=0.
10012 CONTINUE
C
      IF (.NOT.(IAMA(INDX+3).NE.0)) GO TO 10013
        INDX=IAMA(INDX+3)
        GO TO 101
10013 CONTINUE
C
C Advance the frame.
C
      CALL FRAME
      IF (ICFELL('ARDBPA',12).NE.0) RETURN
C
C Restore the polyline color index and the text color index.
C
      CALL GSPLCI (IPCI)
      CALL GSTXCI (ITCI)
C
C Restore the original SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('ARDBPA',13).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
