
#
#      $Id: math_funcs.sh,v 1.2 1996-12-17 18:41:14 ethan Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1995				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		
#
#	Author:		Ethan Alpert
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Tue Jan 31 15:15:35 MST 1995
#
#	Description:	
#
#	Usage:
#
#	Environment:
#
#	Files:
#
#
#	Options:
#

cp MathFuncs.h.sed MathFuncs.h

sed \
-e 's/FUNCNAME/sinh/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/sinh/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed > .tmp.$$

sed \
-e 's/FUNCNAME/cosh/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/cosh/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/tanh/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/tanh/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

#sed \
#-e 's/FUNCNAME/asinh/g' \
#MathTemplate.h.sed >> MathFuncs.h

#sed \
#-e 's/FUNCNAME/asinh/g' \
#-e 's/OUTDATATYPE/float/g' \
#-e 's/ARG0TYPE/float/g' \
#-e 's/CAST/double/g' \
#MathTemplate.c.sed >> .tmp.$$

#sed \
#-e 's/FUNCNAME/acosh/g' \
#MathTemplate.h.sed >> MathFuncs.h

#sed \
#-e 's/FUNCNAME/acosh/g' \
#-e 's/OUTDATATYPE/float/g' \
#-e 's/ARG0TYPE/float/g' \
#-e 's/CAST/double/g' \
#MathTemplate.c.sed >> .tmp.$$

#sed \
#-e 's/FUNCNAME/atanh/g' \
#MathTemplate.h.sed >> MathFuncs.h

#sed \
#-e 's/FUNCNAME/atanh/g' \
#-e 's/OUTDATATYPE/float/g' \
#-e 's/ARG0TYPE/float/g' \
#-e 's/CAST/double/g' \
#MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/sin/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/sin/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/cos/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/cos/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/tan/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/tan/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/asin/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/asin/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/acos/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/acos/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/atan/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/atan/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$


sed \
-e 's/FUNCNAME/ceil/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/ceil/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/floor/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/floor/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/fabs/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/fabs/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

#sed \
#-e 's/FUNCNAME/trunc/g' \
#MathTemplate.h.sed >> MathFuncs.h

#sed \
#-e 's/FUNCNAME/trunc/g' \
#-e 's/OUTDATATYPE/float/g' \
#-e 's/ARG0TYPE/float/g' \
#-e 's/CAST/double/g' \
#MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/log/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/log/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/log10/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/log10/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/sqrt/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/sqrt/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate.c.sed >> .tmp.$$

#sed \
#-e 's/FUNCNAME/rint/g' \
#MathTemplate.h.sed >> MathFuncs.h

#sed \
#-e 's/FUNCNAME/rint/g' \
#-e 's/OUTDATATYPE/float/g' \
#-e 's/ARG0TYPE/float/g' \
#-e 's/CAST/double/g' \
#MathTemplate.c.sed >> .tmp.$$

sed \
-e 's/FUNCNAME/atan2/g' \
MathTemplate.h.sed >> MathFuncs.h

sed \
-e 's/FUNCNAME/atan2/g' \
-e 's/OUTDATATYPE/float/g' \
-e 's/ARG0TYPE/float/g' \
-e 's/ARG1TYPE/float/g' \
-e 's/CAST/double/g' \
MathTemplate2.c.sed >> .tmp.$$

echo "#endif /* MATHFUNC_h */ " >> MathFuncs.h

sed \
-e "/REPLACE/r .tmp.$$" \
-e '/REPLACE/d' \
MathFuncs.c.sed > MathFuncs.c

echo "created MathFuncs.c"

rm .tmp.$$
