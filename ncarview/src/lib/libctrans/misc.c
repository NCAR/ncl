/*
 *      $Id: misc.c,v 1.2 2000-07-12 18:00:48 haley Exp $
 */
/************************************************************************
*                                                                       *
*                          Copyright (C)  1992                          *
*            University Corporation for Atmospheric Research            *
*                          All Rights Reserved                          *
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

/*
 *	File:		misc.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Feb 28 14:56:18 MST 1992
 *
 *	Description:	miscellaneous ctrans funcions
 */

CoordStringToInt(s, llx, lly, urx, ury)
	char	*s;
	int	*llx, *lly, *urx, *ury;
{
	float	llx_, lly_, urx_, ury_;

	if (sscanf(s, "%f:%f:%f:%f", &llx_, &lly_, &urx_, &ury_) != 4) {
#ifdef	USE_ESPRINTF
		ESprintf(EINVAL, s);
#endif
		return(-1);
	}


	if (llx_ < 0 || llx_ > 1.0 || lly_ < 0 || lly_ > 1.0 ||
		urx_ < 0 || urx_ > 1.0 || ury_ < 0 || ury_ > 1.0) {

#ifdef	USE_ESPRINTF
		ESprintf(EINVAL, s);
#endif
		return(-1);

	}

	if (llx_ > urx_ || lly_ > ury_) {
#ifdef	USE_ESPRINTF
		ESprintf(EINVAL, s);
#endif
		return(-1);
	}

	*llx = 32767 * llx_;
	*lly = 32767 * lly_;
	*urx = 32767 * urx_;
	*ury = 32767 * ury_;
	return(1);
}
