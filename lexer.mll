{
	open Parser
	open Printf
	exception END_OF_FILE
}

let all_digit = ['0'-'9'] (*All digits*)
let nz_digit = ['1'-'9'] (*Non-zero digits*)

rule lambda_spreadsheet = parse
	| nz_digit all_digit* '.' all_digit* nz_digit 
	| '0' '.' all_digit* nz_digit 
	| nz_digit all_digit* 
	| '0' as fstring	{printf "%s " fstring; FLOAT(fstring)}	(*Deceting float constants with no redundant 0's*)
	
	| "ADD"		{printf "%s " "ADD"; ADD}
	| "SUBT"	{printf "%s " "SUBT"; SUBT}
	| "MULT"	{printf "%s " "MULT"; MULT}
	| "DIV"		{printf "%s " "DIV"; DIV} 		(*Detecting Binary Operators*)
	
	| "COUNT"		{printf "%s " "COUNT"; COUNT}
	| "ROWCOUNT"	{printf "%s " "ROWCOUNT"; ROWCOUNT}	
	| "COLCOUNT"	{printf "%s " "COLCOUNT"; COLCOUNT}
	| "SUM"			{printf "%s " "SUM"; SUM}
	| "ROWSUM"		{printf "%s " "ROWSUM"; ROWSUM}
	| "COLSUM"		{printf "%s " "COLSUM"; COLSUM}
	| "AVG"			{printf "%s " "AVG"; AVG}
	| "ROWAVG"		{printf "%s " "ROWAVG"; ROWAVG}
	| "COLAVG"		{printf "%s " "COLAVG"; COLAVG}
	| "MIN"			{printf "%s " "MIN"; MIN}
	| "ROWMIN"		{printf "%s " "ROWMIN"; ROWMIN}
	| "COLMIN"		{printf "%s " "COLMIN"; COLMIN}
	| "MAX"			{printf "%s " "MAX"; MAX}
	| "ROWMAX"		{printf "%s " "ROWMAX"; ROWMAX}
	| "COLMAX"		{printf "%s " "COLMAX"; COLMAX}	(*Detecting Unary Operators*)		
	
	| '('	{printf "%s " "LPAREN"; LPAREN}
	| ')'	{printf "%s " "RPAREN"; RPAREN}
	| '['	{printf "%s " "LBRACKET"; LBRACKET}
	| ']'	{printf "%s " "RBRACKET"; RBRACKET}
	| ','	{printf "%s " "COMMA"; COMMA}
	| ':'	{printf "%s " "COLON"; COLON}
	| ":="	{printf "%s " "ASSOP"; ASSOP}
	| ';' | ";\n"	{printf "%s " "TERMINATION"; TERMINATION}
	| [' ' '\t' '\n'] {lambda_spreadsheet lexbuf} (* eat up whitespace *)
	| eof {printf "%s \n" "END_OF_FILE"; exit 0 (*raise END_OF_FILE*)}
	| '+' {printf "%s " "PLUS"; PLUS}
	| '-' {printf "%s " "MINUS"; MINUS}
	| '*' {printf "%s " "TIMES"; TIMES}
	| '/' {printf "%s " "DIVIDE"; DIVIDE}
