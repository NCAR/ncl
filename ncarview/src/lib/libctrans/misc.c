
/*
 *      $Id: misc.c,v 1.1 1992-02-29 00:13:53 clyne Exp $
 */
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
