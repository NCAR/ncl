/*
 *	$Id: sunraster.h,v 1.2 2000-07-12 18:00:51 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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

#include	<pixrect/pixrect_hs.h>

#define	TILE_SIZE		16	/* hatch pattern replication tile size*/
#define	MAX_POLYGON_POINTS	1024	/* max polygon handled by hardware */

#define	POINT_BUF_ALLOCED	20
static	struct	{
	struct	pr_pos	*p;
	int	size;
	} pointBuf = {NULL, 0};

#define	INITIAL_COLOR_ALLOCATION	2
#define	MAX_COLOR	256
#define	MAX_COLOR_INDEX	256
#define	UNALLOCATED	-1

typedef	struct	{
	unsigned char	red,
			green,
			blue;
	} Rgb;

static	struct	{
	int	default_i[MAX_COLOR_INDEX];
	int	current_i[MAX_COLOR_INDEX];
	Rgb	default_rgb[MAX_COLOR_INDEX];
	Rgb	current_rgb[MAX_COLOR_INDEX];
	int	default_index;
	int	current_index;
	int	*next_new_index;
	Rgb	*rgb;
	int	*index;
	} color_tab;

