/*
 *	$Id: text.h,v 1.3 2000-07-12 18:00:52 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/

#define STR_ALC		256	/* intitial amount of mem alocated for strings*/

/* magnitude of a vector*/
#define	MAG(X1,Y1)	((float) (sqrt((double) (((X1) * (X1)) + ((Y1) * (Y1))))))

/* macro for computing scaling factors in x and y directions	*/
#define Y_SCALE(HEIGHT)	(((float) CHAR_HEIGHT / (float) HEIGHT))
#define X_SCALE(HEIGHT) ((float) (((float) CHAR_HEIGHT / (float) HEIGHT)) * CHAR_EXPAN * (float) (MAG(CHAR_X_BASE,CHAR_Y_BASE) / MAG(CHAR_X_UP,CHAR_Y_UP)))


