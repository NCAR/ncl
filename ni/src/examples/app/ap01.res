!
!      $Id: ap01.res,v 1.2 1995-03-24 11:29:10 boote Exp $
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!									!
!			   Copyright (C)  1995				!
!	     University Corporation for Atmospheric Research		!
!			   All Rights Reserved				!
!									!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!	File:		ap01.res
!
!	Author:		Jeff W. Boote
!			National Center for Atmospheric Research
!			PO 3000, Boulder, Colorado
!
!	Date:		Thu Mar 23 17:11:48 MST 1995
!
!	Description:	
!
ap01.?.wkForegroundColor:	(/1.0,1.0,0.282353/)
ap01.?.wkBackgroundColor:	(/0.0,0.0,0.0/)

*tx1.txPosXF:			.5
*tx1.txPosYF:			.5

*tx1.txString:	NCAR Graphics

!
! The following 5 lines are used to demonstrate how the Resource Database
! determines which specification actually gets used for "tx1"'s Font.
! See $(NCARG_DOCROOT)/ref/hlu/ResDB.html for a disscussion.

ap01*xWorkstationLayerClass*txFont:	default
*tx1.Font:				helvetica
ap01.x*textItemLayerClass*txFont:	times-bold
ap01.x*?.Font:				courier
ap01.x*textItemLayerClass.txFont:	courier-bold
