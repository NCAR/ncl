%{
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include "WSymbol.h"
#include "y.tab.h"
extern WWrapRec *current;
%}
let [_a-zA-Z]
alnum [_a-zA-Z0-9]
white [\t\r ]
digit [0-9]
%x wrapsec fortsec fortsec2
%%
<fortsec,wrapsec,fortsec2>[\n] {
#ifdef LEXDEBUG
	fprintf(stdout,"Real EOLN\n");
#endif
	return(EOLN);
}
<fortsec,wrapsec,fortsec2>[\n]+"     "({alnum}|"+"|"*"|"&"|"$"|"#"|"-"|"@"|"^"|"!")[ \t]* {
#ifdef LEXDEBUG
	fprintf(stdout,"Line Continuation\n");
#endif
}
[cC!][ \t]*("NCLFORTSTART"|"nclfortstart")[ \t^\n]* {
	WNewWrap();
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN fortsec\n");
#endif
	BEGIN fortsec;
}
<fortsec2>[!][ \t]*("NCLEND"|"nclend")[ \t^\n]* {
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN 0\n");
#endif
	BEGIN 0;
	return(ENDOFIF);
}
<fortsec2>[\n][cC][ \t]*("NCLEND"|"nclend")[ \t^\n]* {
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN 0\n");
#endif
	BEGIN 0;
	return(ENDOFIF);
}
<fortsec2>[!][ \t]*("NCLWRAPSTART"|"nclwrapstart")[ \t^\n]* {
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN wrapsec\n");
#endif
        BEGIN wrapsec;
}
<fortsec2>[\n][cC][ \t]*("NCLWRAPSTART"|"nclwrapstart")[ \t^\n]* {
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN wrapsec\n");
#endif
	BEGIN wrapsec;
}
<wrapsec>[cC!][ \t]*("NCLEND"|"nclend")[ \t^\n]* {
#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN 0\n");
#endif
	BEGIN 0;
	return(ENDOFIF);
}
<fortsec,wrapsec,fortsec2>{digit}+ {
	sscanf(yytext,"%d",&(yylval.integer));
	return(INT);
}
<fortsec>[!][^\n]* {
#ifdef LEXDEBUG
	fprintf(stdout,"comment1.1\n");
#endif
}
<fortsec2>[!][^\n]* {
#ifdef LEXDEBUG
	fprintf(stdout,"comment1.1\n");
#endif
/*
	return(EOLN);
*/
}
<fortsec>[\n][cC][^\n]* {
#ifdef LEXDEBUG
	fprintf(stdout,"comment1\n");
#endif
}
<fortsec2>[\n][cC][^\n]* {
#ifdef LEXDEBUG
	fprintf(stdout,"comment0\n");
#endif
	return (EOLN);
}
<fortsec>{let}{alnum}* {
	NclSymbol *s;
	
	if((s = _NclLookUpInScope(current->frec,(char*)yytext)) == NULL)  {
#ifdef LEXDEBUG
		fprintf(stdout,"InS: %s\n",(char*)yytext);
#endif
		s = _NclAddInScope(current->frec,(char*)yytext,UNDEF);
	} 
#ifdef LEXDEBUG
		else {
		fprintf(stdout,"FoundS: %s\n",(char*)yytext);
	}
#endif

#ifdef LEXDEBUG
        fprintf(stdout,"BEGIN fortsec2\n");
#endif
	BEGIN fortsec2;
	yylval.sym = s;	
	return(s->type);
}
<fortsec2>{let}{alnum}* {
	NclSymbol *s;
	
	if((s = _NclLookUpInScope(current->frec,(char*)yytext)) == NULL)  {
#ifdef LEXDEBUG
		fprintf(stdout,"InS: %s\n",(char*)yytext);
#endif
		s = _NclAddInScope(current->frec,(char*)yytext,UNDEF);
	} 
#ifdef LEXDEBUG
		else {
		fprintf(stdout,"FoundS: %s\n",(char*)yytext);
	}
#endif

	yylval.sym = s;	
	return(s->type);
}
<wrapsec>{let}{alnum}* {
	NclSymbol *s;
	
	if((s = _NclLookUpInScope(current->wrec,(char*)yytext)) == NULL) {
#ifdef LEXDEBUG
		fprintf(stdout,"InS: %s\n",(char*)yytext);
#endif
		s = _NclAddInScope(current->wrec,(char*)yytext,UNDEF);
	} 
#ifdef LEXDEBUG
	else {
		fprintf(stdout,"FoundS: %s\n",(char*)yytext);
	}
#endif
	
	
	yylval.sym = s;	
	return(s->type);
}
<wrapsec>[\n][cC!][ \t]* {
#ifdef LEXDEBUG
	fprintf(stdout,"scanning cC....\n");
#endif
}
<fortsec,wrapsec,fortsec2>\@ {
	return('@');
}
<fortsec,wrapsec,fortsec2>\& {
	return('&');
}
<fortsec,wrapsec,fortsec2>\( {
	return('(');
}
<fortsec,wrapsec,fortsec2>\) {
	return(')');
} 
<fortsec,wrapsec,fortsec2>\* {
	return('*');
}
<fortsec,wrapsec,fortsec2>\, {
	return(',');
}
<fortsec,wrapsec,fortsec2>\# {
	return('#');
}
<fortsec,wrapsec,fortsec2>\/ {
	return('/');
}
<fortsec,wrapsec,fortsec2>\] {
	return(']');
}
<fortsec,wrapsec,fortsec2>\[ {
	return('[');
}
<fortsec,wrapsec,fortsec2>\= {
	return('=');
}
<fortsec,wrapsec,fortsec2>\^ {
	return('^');
}
<fortsec,wrapsec,fortsec2>\: {
	return(':');
}
<fortsec,wrapsec,fortsec2>. {
#ifdef LEXDEBUG
	fprintf(stdout,"comment6\n");
#endif
}
[^\n]*[\n] {
#ifdef LEXDEBUG
	fprintf(stdout,"comment4\n");
#endif
}
. {
#ifdef LEXDEBUG
	fprintf(stdout,"comment3\n");
#endif
}
