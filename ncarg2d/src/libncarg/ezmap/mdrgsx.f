C
C $Id: mdrgsx.f,v 1.14 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGSX (ICAT,ICEL,IRIM,ILAT,ILON,IFLL,IRGL,
     +                                         IAMA,LAMA,XCRA,YCRA,MCRA)
C
        INTEGER ICAT,ICEL,IRIM,ILAT,ILON,IFLL,IRGL,IAMA(LAMA),LAMA,MCRA
        REAL XCRA(MCRA),YCRA(MCRA)
C
C This routine, given the file identifiers ICAT, ICEL, and IRIM for a
C particular level of the RANGS/GSHHS data, and the integer latitude and
C longitude ILAT and ILON (-90.LE.ILAT.LE.89 and 0.LE.ILON.LE.359) of
C the lower left-hand corner of a particular 1-degree square, retrieves
C the polygons for that square, checks them against a list of polygons
C that self-intersect, edits them if they do, and then does with them
C what is implied by the value of IFLL.  If IFLL is 0, portions of the
C polygon that do not lie on the edges of the 1-degree square are drawn.
C If IFLL is 1, the polygons are filled directly, without going through
C an area map.  If IFLL is 2, the polygons are drawn using an area map,
C which allows for transparency situations that IFLL=1 does not.  IRGL
C is the level at which the RANGS/GSHHS data is being used, required
C here for the call to MDRGED.  The array IAMA is an area map array.
C The arrays XCRA and YCRA are used to hold the coordinates of points
C defining polygons retrieved from the data arrays.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Declare the routine that will fill the areas.
C
        EXTERNAL         MDRGFA
C
C Declare local variables.
C
        INTEGER          I,IAAI(1),IAGI(1),ICWP,IFLG,ILEV,IPGB,IPID,
     +                   IPOS,ISGB,ISTA,ITML,ITMP,ITYP,LNLG,NCRA,NPTS,
     +                   ISHIFT
C
        REAL             XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT
C
        DOUBLE PRECISION PLAT,PLON,QLAT,QLON,RLAT,RLON
C
C CTM4 and CTM1 are character temporaries into which we can read a group
C of four bytes or a single byte, respectively.  (Because the data are
C written in a byte-swapped fashion, we cannot read a four-byte quantity
C as if it were an integer; instead, we must read the four bytes and put
C the integer together.)
C
        CHARACTER*4 CTM4
        CHARACTER*1 CTM1
C
C Define an amended version of the FORTRAN function ICHAR that will work
C on machines like the IBM, which sign-extend eight-bit characters.
C
        CHARACTER*1 CARG
C
        ICHARF(CARG)=MOD(ICHAR(CARG)+256,256)
C
C Copy the specified values of ILAT and ILON to real variables in a
C common block to be passed to the area-fill routine MDRGFA.
C
        OLAT=REAL(ILAT)
        OLON=REAL(ILON)
C
C Copy the value of MCRA to an integer variable in a common block to
C be passed to the routine MDRGIP.
C
        LCRA=MCRA
C
C Initialize the area map.
C
        IF (IFLL.GE.2) CALL ARINAM (IAMA,LAMA)
C
C Save the current SET state and re-do it in a manner allowing for the
C mapping of lat/lon points from the chosen 1-degree square to the whole
C plotter frame.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C Zero the polygon-level counter and the count of polygon points saved.
C
        ILEV=0
        NCRA=0
C
C Look in the catalog for a pointer to the definition of the desired
C 1-degree square in the cell file.
C
        CALL NGSEEK (ICAT,4*((89-ILAT)*360+ILON),0,ISTA)
C
        IF (ISTA.LT.0) THEN
          CALL SETER ('MDRGSX - SEEK FAILURE IN CAT FILE',1,1)
          RETURN
        END IF
C
        CALL NGRDCH (ICAT,CTM4,4,ISTA)
C
        IF (ISTA.NE.4) THEN
          CALL SETER ('MDRGSX - READ FAILURE IN CAT FILE',2,1)
          RETURN
        END IF
C
        IPOS=IOR(ISHIFT(IOR(ISHIFT(IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                        ICHARF(CTM4(3:3))),8),
     +                                        ICHARF(CTM4(2:2))),8),
     +                                        ICHARF(CTM4(1:1)))-1
C
C Position the cell file to the start of the definition.
C
        CALL NGSEEK (ICEL,IPOS,0,ISTA)
C
        IF (ISTA.LT.0) THEN
          CALL SETER ('MDRGSX - SEEK FAILURE IN CEL FILE',3,1)
          RETURN
        END IF
C
C Read a new polygon byte.
C
  101   CALL NGRDCH (ICEL,CTM1,1,ISTA)
C
        IF (ISTA.NE.1) THEN
          CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',4,1)
          RETURN
        END IF
C
        IPGB = ICHARF(CTM1)
C
C If the polygon byte is non-zero, we are at the start of a new polygon.
C
        IF (IPGB.NE.0) THEN
C
C Bump the value of ILEV by one.  This is done because the definition
C of a polygon is recursive; it can itself include one or more interior
C polygons that form holes in it.  Each polygon begins with a non-zero
C polygon byte and ends with a zero polygon byte; if, when we read for
C a polygon byte, we get a non-zero value, we know that a new level of
C recursion is beginning.
C
          ILEV=ILEV+1
C
C Read the polygon ID.
C
          CALL NGRDCH (ICEL,CTM4,4,ISTA)
C
          IF (ISTA.NE.4) THEN
            CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',5,1)
            RETURN
          END IF
C
          IPID=IOR(ISHIFT(IOR(ISHIFT(IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                          ICHARF(CTM4(3:3))),8),
     +                                          ICHARF(CTM4(2:2))),8),
     +                                          ICHARF(CTM4(1:1)))
C
C Zero IFLG.  It gets set non-zero when we have found the first point
C of the polygon, positive if that point is on the edge of the 1-degree
C cell, negative if otherwise.
C
          IFLG=0
C
C Zero ITMP.  This is done because we save the previous value of ITMP
C in ITML; it prevents ITML from getting a garbage value.
C
          ITMP=0
C
C Read a new segment byte.
C
  102     CALL NGRDCH (ICEL,CTM1,1,ISTA)
C
          IF (ISTA.NE.1) THEN
            CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',6,1)
            RETURN
          END IF
C
          ISGB = ICHARF(CTM1)
C
C The low-order three bits of the segment byte tell us what follows.
C Pull them off into ITMP, saving the previous value as ITML.
C
          ITML=ITMP
          ITMP=IAND(ISGB,7)
C
C If ITMP is non-zero, extract a three-bit integer that tells us the
C type of the polygon we're working on (0 => ocean, 1 => land, 2 =>
C lake, 3 => island in lake, 4 => pond on island) and a one-bit integer
C that says whether it is defined in a clockwise or counterclockwise
C direction.
C
          IF (ITMP.NE.0) THEN
            ITYP=IAND(ISHIFT(ISGB,-4),7)
            ICWP=IAND(ISHIFT(ISGB,-3),1)
          END IF
C
C If ITMP is between 1 and 6, inclusive, it specifies a number of
C points defined by values read from the cell file; each point is
C defined by two 4-byte integers, the first a longitude and the
C second a latitude, in units of millionths of a degree.  All of these
C points are on the edge of the 1-degree cell.
C
          IF (ITMP.GE.1.AND.ITMP.LE.6) THEN
C
            NPTS = ITMP
C
C Loop to read the points.
C
            DO 105 I=1,NPTS
C
C Read a longitude.
C
              CALL NGRDCH (ICEL,CTM4,4,ISTA)
C
              IF (ISTA.NE.4) THEN
                CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',7,1)
                RETURN
              END IF
C
              QLON=RLON
C
              RLON=.000001D0*DBLE(IOR(ISHIFT(IOR(ISHIFT(
     +                       IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                  ICHARF(CTM4(3:3))),8),
     +                                  ICHARF(CTM4(2:2))),8),
     +                                  ICHARF(CTM4(1:1)))-1000000*ILON)
C
C Read a latitude.
C
              CALL NGRDCH (ICEL,CTM4,4,ISTA)
C
              IF (ISTA.NE.4) THEN
                CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',8,1)
                RETURN
              END IF
C
              QLAT=RLAT
C
              RLAT=.000001D0*DBLE(IOR(ISHIFT(IOR(ISHIFT(
     +                       IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                  ICHARF(CTM4(3:3))),8),
     +                                  ICHARF(CTM4(2:2))),8),
     +                                  ICHARF(CTM4(1:1)))-1000000*ILAT)
C
C If we had not yet seen the first point of the polygon (that is to
C say, if IFLG is zero), this is it; save its longitude and latitude
C and set IFLG to say that the first point was on the cell boundary.
C
              IF (IFLG.EQ.0) THEN
                PLON=RLON
                PLAT=RLAT
                IFLG=+1
              END IF
C
C Insert the point in the list.
C
              IF (NCRA+1.LT.MCRA) THEN
                NCRA=NCRA+1
                XCRA(NCRA)=REAL(RLON)
                YCRA(NCRA)=REAL(RLAT)
                IF (NCRA.GT.1) THEN
                  IF (XCRA(NCRA).EQ.XCRA(NCRA-1).AND.
     +                YCRA(NCRA).EQ.YCRA(NCRA-1)) NCRA=NCRA-1
                END IF
              ELSE
                CALL SETER ('MDRGSX - COORDINATE BUFFER OVERFLOW',9,1)
                RETURN
              END IF
C
  105       CONTINUE
C
C Go back to read another segment byte.
C
            GO TO 102
C
C If ITMP is a 7, then the next two values in the cell file are 4-byte
C integers, the first of which is a pointer into the rim file and the
C second of which is the number of points to be read from the rim file.
C Each point in the rim file is defined like a point in the cell file,
C by a pair of 4-byte integers, specifying longitude and latitude, in
C millionths of a degree.  All of the points in the rim file are in
C the interior of the 1-degree cell.
C
          ELSE IF (ITMP.EQ.7) THEN
C
C Get the pointer into the rim file.
C
            CALL NGRDCH (ICEL,CTM4,4,ISTA)
C
            IF (ISTA.NE.4) THEN
              CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',10,1)
              RETURN
            END IF
C
            IPOS=IOR(ISHIFT(IOR(ISHIFT(IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                            ICHARF(CTM4(3:3))),8),
     +                                            ICHARF(CTM4(2:2))),8),
     +                                            ICHARF(CTM4(1:1)))-1
C
C Get the number of points to be read from the rim file.
C
            CALL NGRDCH (ICEL,CTM4,4,ISTA)
C
            IF (ISTA.NE.4) THEN
              CALL SETER ('MDRGSX - READ FAILURE IN CEL FILE',11,1)
              RETURN
            END IF
C
            NPTS=IOR(ISHIFT(IOR(ISHIFT(IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                            ICHARF(CTM4(3:3))),8),
     +                                            ICHARF(CTM4(2:2))),8),
     +                                            ICHARF(CTM4(1:1)))
C
C If the number of points to be read is non-zero (for some reason, it
C *can* happen that the number is zero), read the points.
C
            IF (NPTS.NE.0) THEN
C
C Position to the start of the list of points in the rim file.
C
              CALL NGSEEK (IRIM,IPOS,0,ISTA)
C
              IF (ISTA.LT.0) THEN
                CALL SETER ('MDRGSX - SEEK FAILURE IN RIM FILE',12,1)
                RETURN
              END IF
C
C Loop to read the points.
C
              DO 106 I=1,NPTS
C
C Read a longitude.
C
                CALL NGRDCH (IRIM,CTM4,4,ISTA)
C
                IF (ISTA.NE.4) THEN
                  CALL SETER ('MDRGSX - READ FAILURE IN RIM FILE',13,1)
                  RETURN
                END IF
C
                QLON=RLON
C
                RLON=.000001D0*DBLE(IOR(ISHIFT(IOR(ISHIFT(
     +                       IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                  ICHARF(CTM4(3:3))),8),
     +                                  ICHARF(CTM4(2:2))),8),
     +                                  ICHARF(CTM4(1:1)))-1000000*ILON)
C
C Read a latitude.
C
                CALL NGRDCH (IRIM,CTM4,4,ISTA)
C
                IF (ISTA.NE.4) THEN
                  CALL SETER ('MDRGSX - READ FAILURE IN RIM FILE',14,1)
                  RETURN
                END IF
C
                QLAT=RLAT
C
                RLAT=.000001D0*DBLE(IOR(ISHIFT(IOR(ISHIFT(
     +                       IOR(ISHIFT(ICHARF(CTM4(4:4)),8),
     +                                  ICHARF(CTM4(3:3))),8),
     +                                  ICHARF(CTM4(2:2))),8),
     +                                  ICHARF(CTM4(1:1)))-1000000*ILAT)
C
C If we had not yet seen the first point of the polygon (that is to
C say, if IFLG is zero), this is it; save its longitude and latitude
C and set IFLG to say that the first point was not on the cell boundary.
C
                IF (IFLG.EQ.0) THEN
                  PLON=RLON
                  PLAT=RLAT
                  IFLG=-1
                END IF
C
C Since all of these points are interior points, we just add their
C coordinates to the point-coordinate buffers.
C
                IF (NCRA+1.LT.MCRA) THEN
                  NCRA=NCRA+1
                  XCRA(NCRA)=REAL(RLON)
                  YCRA(NCRA)=REAL(RLAT)
                  IF (NCRA.GT.1) THEN
                    IF (XCRA(NCRA).EQ.XCRA(NCRA-1).AND.
     +                  YCRA(NCRA).EQ.YCRA(NCRA-1)) NCRA=NCRA-1
                  END IF
                ELSE
                  CALL SETER
     +                      ('MDRGSX - COORDINATE BUFFER OVERFLOW',15,1)
                  RETURN
                END IF
C
  106         CONTINUE
C
            END IF
C
C Go back to read another segment byte.
C
            GO TO 102
C
          END IF
C
C If ITMP is zero, we've reached the end of the polygon.  We must supply
C a closure point (the first point of the polygon) and then fill the
C polygon.  Output the fill area.  First, see if at least one point is
C defined in the buffers.
C
          IF (NCRA.GT.1) THEN
C
C Add the coordinates of the closure point to the buffers, edit the
C polygon, and take the desired action (draw portions of it that lie
C in the interior of the unit square, fill it directly, or send it to
C an area map).  The statement beginning "IF (ILAT.GE.55 ... " is
C necessary to fix an error in the GSHHS/RANGS database.
C
            IF (XCRA(NCRA).NE.REAL(PLON).OR.
     +          YCRA(NCRA).NE.REAL(PLAT)) THEN
              NCRA=NCRA+1
              XCRA(NCRA)=REAL(PLON)
              YCRA(NCRA)=REAL(PLAT)
              IF (NCRA.GT.1) THEN
                IF (XCRA(NCRA).EQ.XCRA(NCRA-1).AND.
     +              YCRA(NCRA).EQ.YCRA(NCRA-1)) NCRA=NCRA-1
              END IF
            END IF
C
            IF (NCRA.GT.1) THEN
              IF (ILAT.GE.55.AND.ILAT.LE.57.AND.ILON.EQ.82.AND.
     +                                                 ITYP.EQ.2) ITYP=1
              CALL MDRGED (IRGL,IPID,ILAT,ILON,ICWP,XCRA,YCRA,NCRA,MCRA)
              IF (IFLL.EQ.0) THEN
                CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
                CALL MDRGDR (XCRA,YCRA,NCRA,ITYP+1)
                CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
              ELSE IF (IFLL.EQ.1) THEN
                CALL MDRGIP (XCRA,YCRA,NCRA)
                CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
                CALL MDRGFP (XCRA,YCRA,NCRA,ITYP+1)
                CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
              ELSE
                IF (ICWP.NE.0) THEN
                  CALL MDRGAM (IAMA,XCRA,YCRA,NCRA,1,ITYP+1,ITYP)
                ELSE
                  CALL MDRGAM (IAMA,XCRA,YCRA,NCRA,1,ITYP,ITYP+1)
                END IF
              END IF
            END IF
C
          END IF
C
C Zero the count of coordinates in the point-coordinate buffer.
C
          NCRA=0
C
C Go back to read another polygon byte.
C
          GO TO 101
C
C If the polygon byte is zero, it says that we've hit the end of a
C polygon.
C
        ELSE
C
C Reduce the recursion counter by one and, if it's still non-zero, go
C back to read another polygon byte.
C
          ILEV=ILEV-1
          IF (ILEV.NE.0) GO TO 101
C
        END IF
C
C Scan the area map, retrieving polygons from it, mapping them as
C directed by the current state of EZMAP, and filling them.
C
        IF (IFLL.GE.2) THEN
          CALL ARGETI ('RC(1)',IRC1)
          CALL ARSETI ('RC(1)',0)
          CALL ARSCAM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,1,MDRGFA)
          CALL ARSETI ('RC(1)',IRC1)
        END IF
C
C Restore the original SET state.
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Normal return.
C
        RETURN
C
      END
