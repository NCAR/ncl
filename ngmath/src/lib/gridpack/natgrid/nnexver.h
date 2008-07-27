/*
 * $Id: nnexver.h,v 1.4 2008-07-27 04:02:37 haley Exp $
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

extern struct datum    *rootdat, *curdat, *holddat;
extern struct simp     *rootsimp, *cursimp, *holdsimp, *lastsimp, *prevsimp;
extern struct temp     *roottemp, *curtemp, *lasttemp, *prevtemp;
extern struct neig     *rootneig, *curneig, *lastneig;
extern struct asinfo   curas;
