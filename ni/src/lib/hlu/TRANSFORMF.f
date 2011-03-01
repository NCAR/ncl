C
C $Id: TRANSFORMF.f,v 1.1 2001-10-09 00:18:36 haley Exp $
C
C****************************************************************
C                                                               *
C                       Copyright (C)  1994                     *
C       University Corporation for Atmospheric Research         *
C                       All Rights Reserved                     *
C                                                               *
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
C       TRANSFORM
C
      subroutine nhlfndctodata(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %iymiss,istat,routrange,ierr)
        integer ipid,n,ixmiss,iymiss,istat,ierr
        real x(n),y(n),xout(n),yout(n),xmiss,ymiss,routrange

        call nhlpfndctodata(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %          iymiss,istat,routrange,ierr)
      end
      subroutine nhlfdatatondc(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %iymiss,istat,routrange,ierr)
        integer ipid,n,ixmiss,iymiss,istat,ierr
        real x(n),y(n),xout(n),yout(n),xmiss,ymiss,routrange

        call nhlpfdatatondc(ipid,x,y,n,xout,yout,xmiss,ymiss,ixmiss,
     %          iymiss,istat,routrange,ierr)
      end
      subroutine nhlfdatapolyline(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfdatapolyline(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfndcpolyline(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfndcpolyline(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfdatapolygon(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfdatapolygon(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfndcpolygon(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfndcpolygon(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfdatapolymarker(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfdatapolymarker(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfndcpolymarker(ipid,igid,x,y,n,ierr)
        integer ipid,igid,n,ierr
        real    x(n),y(n)

        call nhlpfndcpolymarker(ipid,igid,x,y,n,ierr)
      end
      subroutine nhlfistransform(id,istat)

        integer id,istat
        call nhlpfistransform(id,istat)
      end
      subroutine nhlfaddprimitive(itid,ipid,ibid,ierr)
        integer itid,ipid,ibid,ierr

        call nhlpfaddprimitive(itid,ipid,ibid,ierr)
      end
      subroutine nhlfremoveprimitive(itid,ipid,ierr)
        integer itid,ipid,ierr

        call nhlpfremoveprimitive(itid,ipid,ierr)
      end
