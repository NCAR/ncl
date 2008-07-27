/*
 * $Id: nntpvrs.h,v 1.4 2008-07-27 04:02:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

struct datum    *rootdat, *curdat, *holddat;
struct simp     *rootsimp, *cursimp, *holdsimp, *lastsimp, *prevsimp;
struct temp     *roottemp, *curtemp, *lasttemp, *prevtemp;
struct neig     *rootneig, *curneig, *lastneig;
struct asinfo   curas;
