/*
 *	$Id: X11_class2.c,v 1.7 1992-04-03 20:39:59 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*	X11_class2.c
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *
 *	This file contain the functions that implement class 2 
 *	CGM elements. The supported elements are COLOUR SELECTION MODE,
 *	VDC EXTENT and BACKROUND. These elements are primarily concerened
 *	with providing information necessary to interpret the data in the
 *	CGM. These elements have no direct effect but merely set defaults
 *	and are thus handeled by the generic default table in "default.c"
 */
/*LINTLIBRARY*/

