.\"
.\"	$Id: ncarg_error.m,v 1.3 1992-09-01 23:47:19 clyne Exp $
.\"
.TH ERROR 1NCAR  "February 1992" 
.SH NAME
ESprintf,
ErrGetMsg,
ErrGetNum,
ErrorList \- Formatted error reporting
.SH SYNOPSIS
.nf
.ft B
#include <ncarv.h>
.ft
.fi
.LP
.nf
.ft B
const char *ESprintf(err_code, format [ , arg] ... )
unsigned err_code;
const char *format;
.ft
.fi
.LP
.nf
.ft B
const char *ErrGetMsg()
.ft
.fi
.LP
.nf
.ft B
int ErrGetNum()
.ft
.fi
.LP
.nf
.ft B
int	ErrorList(start, num, err_list)
unsigned start;
unsigned num;
const char **err_list;
.ft
.fi
.LP
.nf
.ft B
char *ESPRINTF(err_code, (format [ , arg] ...) )
unsigned err_code;
char *format;
.ft
.fi
.SH DESCRIPTION
.LP
.BR ESprintf(\|)
records a short message to an internal static message buffer. 
.BR ESprintf(\|) 
behaves much like the UNIX 
.BR printf(\|)
family of formatters.
It converts, formats, and stores its
.IR arg s
under the control of 
.IR format .
.BR ESprintf(\|)
accepts an identical set of conversion specifications as its 
UNIX cousins.
.LP
.I err_code
is an index into an internal error table. Valid values of 
.I err_code
are 
.BR errno ,
where 
.B errno
is the UNIX global error code variable, or
a value made available by a call to 
.BR ErrorList(\|) ,
or the manifest constant,
.BR E_UNKNOWN .
A space, followed by a colon, followed by a space, followed by
the message referenced by
.I err_code
are appended to 
.IR format.
.BR ESprintf(\|)
returns the address of the message buffer containing the formatted string.
.LP
.BR ESPRINTF(\|)
is a macro font-end to 
.BR ESprintf(\|)
that prepends the string "FILE: file, LINE: line" to the error
message, where file and line
are the values defined by the special cpp names, 
.B __FILE__ 
and 
.BR __LINE__ ,
respectively. The format string AND the variable argument list 
MUST be enclosed in their own
set of parentheses or the C preprocessor will complain.
.LP
.BR ErrGetMsg(\|)
returns the address of the current message buffer.
.LP
.BR ErrGetNum(\|)
returns the current error code set by the last call to
.BR ESprintf(\|) .
.LP
.BR ErrorList(\|)
Adds an error list to the internal error table for future access by
.BR ESprintf(\|) .
.I start
should be the first valid error code in 
.IR err_list .
The values 0 - 1000 are reserved.
The index into 
.I err_list
is calculated by subtracting 
.I start
from the error number. Thus if you add a list with
.I start
equal to
.B 1001
and later invoke
.B ESprintf(\|)
with 
.I err_code
equal to 
.B 1001
the error message referenced will be
.IB err_list[ 0 ] .
.BR ErrorList(\|) 
returns a non-negative number on success. On failure,
it returns a
.B -1
indicating the the error table is full.
.SH "SEE ALSO"
.BR errno(3),
.BR printf(3V)
.SH BUGS

