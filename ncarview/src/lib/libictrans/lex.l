%{
#include <ctype.h>
#include "lex.h"
static	short	have_command = 0;
%}


D		[0-9]
UINT		{D}+
ABS_PART	("."|"$"|{UINT})
REL_PART	("+"|"-")[ ]*{UINT}

FRAME		({ABS_PART}|(({ABS_PART}|[ ]*)[ ]*{REL_PART}))

FRAME_LIST	({FRAME}[ ]*","[ ]*{FRAME})

ALPHA		[a-zA-Z]
ALPHAS		{ALPHA}+
COMMAND	("!"|{ALPHAS})
DATA	[\-\\^a-zA-Z0-9!|#%&'()*/:;<>=_?@~.$, ]


%%
[ ]*	;

{FRAME}			{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_TYPE);
			} 
			else 
				REJECT;
			}

{FRAME_LIST}		{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_LIST_TYPE);
			} 
			else 
				REJECT;
			}

{COMMAND}		{
			if (!have_command) {
				have_command = 1;
				return(COMMAND_TYPE);
			}
			else
				REJECT;
			}


{DATA}+			{
			if (!have_command) {
				REJECT;
			}
			else {
				yyleng = nuke_lead_whitespace(yytext);
				return(DATA_TYPE);
			}
			}

[\n]			{
			have_command = 0;
			return(END_LINE);
			}

.			{
			have_command = 0;
			(void) skip_line();
			return(ERROR_TYPE);
			}

%%



nuke_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (*s1) {
		if (!isspace(*s1)) {
			*s2++ = *s1++;
			len++;
		}
		else
			s1++;
	}
	*s2 = '\0';
	return (len);
}

nuke_lead_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (isspace(*s1))
		s1++;

	while (*s1) {
		*s2++ = *s1++;
		len++;
	}
	*s2 = '\0';
	return (len);
}

#ifdef	DEAD
nuke_quotes(s1, len)
	char	*s1;
	int	len;
{

	len -= 2;
	memmove(s1, s1 + 1, len);
	s1[len] = '\0';

	return (len);
}
#endif

skip_line()
{
	int	c;

	while (((c = input()) != EOF) && c != '\n')
	;

	if (c == EOF) unput(c);

	return(1);
}

