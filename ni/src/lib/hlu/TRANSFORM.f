C
C $Id: TRANSFORM.f,v 1.2 1995-02-17 10:23:27 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            TRANSFORM.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Apr 15 16:55:17 MDT 1994
C
C      Description:     
C
C
C	TRANSFORM
C
      subroutine nhlfndctodata(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %iymiss,istat,routrange,ierr)
	integer ipid,n,ixmiss,iymiss,istat,ierr
	real x(n),y(n),xout(n),yout(n),xmiss,ymiss,routrange

	call nhl_fndctodata(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %		iymiss,istat,routrange,ierr)
      end
      subroutine nhlfdatatondc(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %iymiss,istat,routrange,ierr)
	integer ipid,n,ixmiss,iymiss,istat,ierr
	real x(n),y(n),xout(n),yout(n),xmiss,ymiss,routrange

	call nhl_fdatatondc(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %		iymiss,istat,routrange,ierr)
      end
      subroutine nhlfdatapolyline(ipid,x,y,n,ierr)
	integer ipid,n,ierr
	real	x(n),y(n)

	call nhl_fdatapolyline(ipid,x,y,n,ierr)
      end
      subroutine nhlfndcpolyline(ipid,x,y,n,ierr)
	integer ipid,n,ierr
	real	x(n),y(n)

	call nhl_fndcpolyline(ipid,x,y,n,ierr)
      end
      subroutine nhlfistransform(id,istat)

	integer id,istat
	call nhl_fistransform(id,istat)
      end
