/*
 *      $Id: c_tgesc.c,v 1.1 1994-09-20 16:16:41 haley Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gsttst.c
 *
 *	Author:		
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 20 09:42:04 MDT 1994
 *
 *	Description:	This is just a very simple example code that
 *			demonstrats how to call the gescape function
 *			for escape elements that return data.
 */
#include <ncarg/gks.h>

main()
{
	Gescape_out_data	*out;
	Gint			err;
	Gstore			stor;
	char			*data;

	gopen_gks("stdout",0);

	/*
	 * esc -1390 returns a null-terminated charactor string from the
	 * gks(C) interface, so we must create a Gstore to pass in to
	 * gescape.  Then the Gescape_out_data union pointer will be
	 * allocated by the gescape function, and the "data" field
	 * of the escape_u1390 can be dereferenced as a single null-terminated
	 * charactor string.  The size part of the escape_u1390 will contain
	 * the length of the charactor string (including the '\0').
	 */

	gcreate_store(&err,&stor);
	if(err){
		fprintf(stderr,"Unable to allocate Gstore!\n");
		exit(1);
	}
	gescape(-1390,NULL,&stor,&out);
	data = out->escape_u1390.data;

	fprintf(stdout,"%s\n",data);

	/*
	 * This actually free's all the memory allocated for the gescape call.
	 * If the user tries to dereference the "out" pointer after this call
	 * it will probably cause a seg-fault.
	 */
	gdel_store(stor);

	exit(0);
}


