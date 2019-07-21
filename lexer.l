%option noyywrap
%{

#include "c_lexer.hpp"

#include <sstream>
#include <stdlib.h>

int lineCount = 1;
int spaceCount = 1;
int sourceLineCount = 1;

int charLength = 0;

std::string fileName;

%}

OPERATOR [+][-][*][/][%][++][--][-=][+=][*=][/=][==][GT][LT][>=][<=][!=][%=][AND][OR][NOT]
	  [sizeof][,][=]

IDENTIFIER [_a-zA-Z][_a-zA-Z0-9]*


KEYWORD		auto|double|int|float|num|char|String|bool|long|unsigned|signed|void|jabtak|kliye|agar|magar|
		extern|static|input|output|switch|case|default|jaaree|tod
		
NUMBERS		 [0-9]+  
INTEGERS 	 [-+]?[0-9]*  
FLOATING_POINT	 [-+]?[0-9]*[.][0-9]+ 
%%



%%
int main()
{
	return 0;
}
