.\"
.\"	$Id: ncarg_option.m,v 1.3 1992-09-01 23:47:21 clyne Exp $
.\"
.TH OPTION 1NCAR  "March 1992" 
.SH NAME
CloseOptionTbl,
GetOptions,
LoadOptionTable,
OptionOptionTbl,
ParseOptionTable,
PrintOptionHelp \- Option parsing utilities
.SH SYNOPSIS
.nf
.ft B
#include <ncarv.h>
.ft
.fi
.LP
.nf
.ft B
int OpenOptionTbl()
.ft
.fi
.LP
.nf
.ft B
int GetOptions(od, options)
int od
const Option *options;
.ft
.fi
.LP
.nf
.ft B
int LoadOptionTable(od, optd)
int od
OptDescRec *optd;
.ft
.fi
.LP
.nf
.ft B
int ParseOptionTable(od, argc, argv, optds)
int od
int *argc;
char **argv;
OptDescRec *optds;
.ft
.fi
.LP
.nf
.ft B
int ParseEnvOptions(od, envv, optds)
int od
const EnvOpt *envv;
OptDescRec *optds;
.ft
.fi
.LP
.nf
.ft B
void PrintOptionHelp(od, fp)
int od
FILE *fp;
.ft
.fi
.LP
.nf
.ft B
int CloseOptionTbl(od)
int od;
.ft
.fi
.SH DESCRIPTION
.LP
.BR OpenOptionTbl(\|) 
opens an option table for subsequent option parsing. There is a per-process
limit on the number of open option tables. On success
.BR OpenOptionTbl(\|) returns a non-negative option descriptor 
for that option table. On failure, a
.B -1
is returned and 
.BR ESprintf(\|)
is invoked.
.LP
.BR CloseOptionTbl(\|) 
deletes the option table referenced by 
.BR od .
.LP
.BR GetOption(\|) 
retrieves the values of command line options from the 
option table referenced by 
.BR od .
Option values may have been previously stored with calls
to either 
.BR LoadOptionTable(\|) 
or 
.BR ParseOptioTable(\|)  .
.I options 
is a NULL terminated array specifying the names of the options
of interest, a function for converting the string representation of 
the option value into a particular type, and the address where the 
converted string-value should be stored. 
The members of an Option structure are:
.LP
.nf
	char	*option_name	/* the option's full name without the 
				 * preceding '-'
				 */
	int	(*type_conv)()	/* option type converter - converts the
				 * string representation of the option
				 * value into a specified format and stores
				 * the result at address given by 'offset'
				 */
	Voidptr	offset		/* destination address of 
				 * converted option	
				 */
	int	size		/* size of individual option as given by
				 * the sizeof() function - if there are
				 * multiple arguments for a single option
				 * their destination address is computed
				 * by adding 'size' to 'offset' as
				 * necessary. i.e The first argument will
				 * be stored at 'offset', the second at
				 * 'offset + size', etc... 
				 */

.fi
.LP
.I type_conv
may be one of the predefined type converters:
.BR NCARGCvtToInt ,
.BR NCARGCvtToFloat ,
.BR NCARGCvtToChar ,
.BR NCARGCvtToBoolean ,
.BR NCARGCvtToString 
or it may be a user-defined function with the following form:
.LP
.nf
	int	type_conv(from, to)
	char	*from;
	Voidptr	to;
.fi
.LP
.I from
is a character string, representing a single value with 
all white space removed.
.I to
contains the address where the converted string should be stored. 
If 
.I from
is the NULL pointer
.I type_conv
should store a reasonable default value at the addressed pointed to
by 
.IR to .
The predefined type converters
.BR NCARGCvtToInt ,
.BR NCARGCvtToFloat ,
.BR NCARGCvtToChar ,
.BR NCARGCvtToBoolean ,
.BR NCARGCvtToString 
convert a NULL
.I from
to 
.BR 0 , 0.0 , 
.BR '\0' , FALSE , 
and 
.B NULL
respectively.
.I type_conv
should return a -1 on failure and invoke
.B ESprintf(\|)
with an appropriate error message. Otherwise
.I type_conv 
returns a positive value.
.LP
.BR LoadOptionTable(\|)
informs the option table referenced by
.B od
of the valid application options. 
.I optd
is a NULL terminated array of named options with the following members:
.LP
.nf
	char	*option		/* the option's name without the 
				 * the preceding '-'
				 */
	int	arg_count	/* The number of arguments expected
				 * by this option. 
				 */
	char	*value		/* The default value for the option,
				 * possibly NULL. If the option expects
				 * multiple arguments than 'value' 
				 * contains multiple space-separated 
				 * values.
				 */
	char	*help		/* A string containing a brief help
				 * message for the option
				 */
.fi
.LP
.BR ParseOptionTable(\|)
destructively merges the options specified by
.I argv
with the options already in the option table referenced by
.BR od .
.I argv 
is an NULL-terminated array of character pointers to options
and their arguments. 
.I argc 
contains the address of the argument count for 
.IR argv ,
which is at least one. The first
member of 
.I argv
points to a string containing the program name and is ignored by
.BR ParseOptionTable(\|)
.IR argv and argc
are updated by 
.BR ParseOptionTable(\|)
as options and their arguments are recognized.
Unrecognized options are ignored.
Option names may be abbreviated up to the point that they are unique.
.LP
If
.I optds
is non-NULL 
.BR LoadOptionTable(\|)
is called with 
.I optds 
as an argument before 
.I argv
is processed.
.LP
.BR ParseEnvOptions(\|)
is identical to 
.BR ParseOptionTable(\|)
except that it takes a list of environent variable names instead of an
argument vector. If the given environment variable is set its value
is used as the argument value for the coresponding option name (also
supplied in the environment vector,
.I envv.)
.I envv
is a NULL terminated array of environment variables and their corresponding
option names. The members of an EnvOpt struct are:
.LP
.nf
	char	*option		/* the option's name without the 
	char	*env_var	/* the name of the environment variable.
.fi
.LP
.BR PrintOptionHelp(\|)
writes the help string associated with each option in the option
table referenced by 
.R od 
to the file pointer referenced by 
.IR fp .
For each option in the option table the option name, followed by
the string "arg0 arg1 ... argN", where N+1 is the number of 
arguments expected by the option, followed by the options help
string, followed by a newline are printed.
.SH SEE ALSO
.BR ncarg_error (local)
.SH EXAMPLE
.LP
The following example program excepts a number of different options
types:
.sp
.RS
.ft B
.nf
static	struct	{
	int	x,y;
	} Position;

static	struct	{
	boolean		verbose;	/* operate in verbose mode	*/
	char		*file;		/* supply a file name		*/
	Position	coords;		/* x,y coordinates		*/
	} opt;

static	OptDescRec	set_options[] = {
	{"verbose", 0, NULL, "Do operate in verbose mode"},
	{"infile", 1, "gmeta", "Specify input metafile"},
	{"position", 2, "0 0", "Specify window location"},
	{NULL}
};

static	Option	get_options[] = {
        {"verbose", NCARGCvtToBoolean, (Voidptr) &opt.verbose, 
						sizeof(opt.verbose)
	},
        {"infile", NCARGCvtToString, (Voidptr) &opt.file, 
						sizeof(opt.file)
	},
        {"position", NCARGCvtToInt, (Voidptr) &opt.coords.x, 
						sizeof(opt.coords.x)
	}
};

main(argc, argv)
	int	argc;
	char	**argv;
{
	int	od;

	od = OpenOptionTbl();
	if (ParseOptionTable(od, &argc, argv, set_options) < 0) {
		fprintf(
			stderr,"%s : Error parsing command line options : %s\n",
			argv[0], ErrGetMsg()
		);
		exit(1);
	}
        if (GetOptions(od, get_options) < 0) {
                fprintf(
                        stderr,"%s : GetOptions() : %s\n",
                        argv[0], ErrGetMsg()
                );
		PrintOptionHelp(od, stderr);
                exit(1);
        }
	(void) CloseOptionTbl(od);
}
.fi
.ft $
.RE
.LP
The following code shows the definition of the integer type converter
.BR NCARGCvtToInt(\|) :
.sp
.RS
.ft B
.nf
int     NCARGCvtToInt(from, to)
	char    *from;  /* the string   */
	Voidptr to;
{
	int     *iptr   = (int *) to;

	if (! from) {
		*iptr = 0;	/* NULL default value	*/
	}
	else if (sscanf(from, "%d", iptr) != 1) {
		ESprintf(EINVAL, "Convert(%s) to int failed", from);
		return(-1);
	}
	return(1);
}
.fi
.ft $
.RE
