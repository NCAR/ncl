/*
 *	$Id: meta_edit.h,v 1.3 2000-07-12 18:01:19 haley Exp $
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
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/

#ifndef	_meta_edit_
#define	_meta_edit_

typedef struct {
	int	start_rec,	/* starting record of this frame	*/
		num_rec;	/* number of records in this frame	*/
	} Frame_entry;		/* reference to a frame in a temp file	*/

/*
 *	A struct that will "point" to a frame in a particular metafile
 */
#define	ACTUAL 0
#define REFERENCE 1
typedef	struct {
	int	utype;		/* union flag, one of (ACTUAL, REFERENCE) */
	union {
		int		frame;	/* frame number in working file	*/
		Frame_entry	frame_entry;/* frame location in tmp file */
		} uval;
	} List;

typedef	struct {
	int	number;		/* num elements				*/
	int	size;		/* size memory allocated to list	*/
	List	*list;		/* the ordered list of frame references	*/
	Cgm_fd	tmp_fd;		/* tmp file for reading in new frames	*/
	int	offset;		/* current write file ptr pos. of tmp file*/
	} Working_list;

#endif	/* _meta_edit_	*/
