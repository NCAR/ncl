/*
 *	$Id: meta_edit.h,v 1.5 2008-07-27 03:22:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
