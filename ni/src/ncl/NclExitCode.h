/*
From: 	Dave Allured <dave.allured@noaa.gov>
	Subject: 	[ncl-talk] Exit code for NCL errors
	Date: 	August 5, 2012 2:35:50 PM MDT
	To: 	NCL USERS <ncl-talk@ucar.edu>
 
NCL support team,

Please implement an exit code to the shell indicating failure, when
NCL terminates abnormally.  Even a partial implementation for some
common errors, such as syntax error, subscript out of range, and file
not found, would be helpful.  Thank you.

--Dave
*/

#define	NclNoError		0
#define	NclFileNotFound		-1
#define	NclSyntaxError		-2
#define	NclSubscriptOutOfRange	-3
#define	NclUnknownStatus	-999

extern int NclReturnStatus;

