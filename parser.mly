%{
	open Backend
	open Printf
%}


/*Tokens*/
%token <string> FLOAT

%token ADD
%token SUBT
%token MULT
%token DIV

%token COUNT
%token ROWCOUNT
%token COLCOUNT
%token SUM
%token ROWSUM
%token COLSUM
%token AVG
%token ROWAVG
%token COLAVG
%token MIN
%token ROWMIN
%token COLMIN
%token MAX
%token ROWMAX
%token COLMAX

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token COLON
%token TERMINATION

%token ASSOP
%token PLUS MINUS
%token TIMES DIVIDE

%start main

%type <unit> main

%%

main:
	formula		{ }
;

/*Formulas*/
formula:
	  	{ printf "%s\n" "Creating Initial Sheet"; print_sheet (initialize "data.dat"); initialize "data.dat"(*print_sheet (create_empty 5 5); create_empty 5 5*) } | /*Initializing sheet from file data.dat instead of Creating Initial spreadsheet with assumed dimentions 5*5 as in the previous assignment*/
	formula i ASSOP error TERMINATION			{ printf "\n%s\n" "Invalid Formula"; print_sheet (assign ($1) (UNDEFINED) ($2)); assign ($1) (UNDEFINED) ($2) } |
	formula i ASSOP error TERMINATION			{ printf "\n%s\n" "Invalid Formula"; print_sheet (assign ($1) (UNDEFINED) ($2)); assign ($1) (UNDEFINED) ($2) } |
	formula error TERMINATION					{ printf "\n%s\n" "Parse Error"; $1 } |
	formula error '\n'							{ printf "\n%s\n" "Parse Error"; $1 } |
	formula i ASSOP c TERMINATION				{ printf "\n%s\n" "Assignment on previous sheet"; print_sheet (assign ($1) (DATA($4)) ($2)); assign ($1) (DATA($4)) ($2) } | /*Added cell assignment function to create required sheets*/
	
	formula i ASSOP COUNT r TERMINATION			{ printf "\n%s\n" "full_count on previous sheet"; print_sheet (full_count ($1) ($5) ($2)); full_count ($1) ($5) ($2) } | /*Calling appropriate functions*/
	formula i ASSOP ROWCOUNT r TERMINATION		{ printf "\n%s\n" "row_count on previous sheet"; print_sheet (row_count ($1) ($5) ($2)); row_count ($1) ($5) ($2) } |
	formula i ASSOP COLCOUNT r TERMINATION		{ printf "\n%s\n" "col_count on previous sheet"; print_sheet (col_count ($1) ($5) ($2)); col_count ($1) ($5) ($2) } |

	formula i ASSOP SUM r TERMINATION			{ printf "\n%s\n" "full_sum on previous sheet"; print_sheet (full_sum ($1) ($5) ($2)); full_sum ($1) ($5) ($2) } |
	formula i ASSOP ROWSUM r TERMINATION		{ printf "\n%s\n" "row_sum on previous sheet"; print_sheet (row_sum ($1) ($5) ($2)); row_sum ($1) ($5) ($2) } |
	formula i ASSOP COLSUM r TERMINATION		{ printf "\n%s\n" "col_sum on previous sheet"; print_sheet (col_sum ($1) ($5) ($2)); col_sum ($1) ($5) ($2) } |

	formula i ASSOP AVG r TERMINATION			{ printf "\n%s\n" "full_avg on previous sheet"; print_sheet (full_avg ($1) ($5) ($2)); full_avg ($1) ($5) ($2) } |
	formula i ASSOP ROWAVG r TERMINATION		{ printf "\n%s\n" "row_avg on previous sheet"; print_sheet (row_avg ($1) ($5) ($2)); row_avg ($1) ($5) ($2) } |
	formula i ASSOP COLAVG r TERMINATION		{ printf "\n%s\n" "col_avg on previous sheet"; print_sheet (col_avg ($1) ($5) ($2)); col_avg ($1) ($5) ($2) } |

	formula i ASSOP MIN r TERMINATION			{ printf "\n%s\n" "full_min on previous sheet"; print_sheet (full_min ($1) ($5) ($2)); full_min ($1) ($5) ($2) } |
	formula i ASSOP ROWMIN r TERMINATION		{ printf "\n%s\n" "row_min on previous sheet"; print_sheet (row_min ($1) ($5) ($2)); row_min ($1) ($5) ($2) } |
	formula i ASSOP COLMIN r TERMINATION		{ printf "\n%s\n" "col_min on previous sheet"; print_sheet (col_min ($1) ($5) ($2)); col_min ($1) ($5) ($2) } |

	formula i ASSOP MAX r TERMINATION			{ printf "\n%s\n" "full_max on previous sheet"; print_sheet (full_max ($1) ($5) ($2)); full_max ($1) ($5) ($2) } |
	formula i ASSOP ROWMAX r TERMINATION		{ printf "\n%s\n" "row_max on previous sheet"; print_sheet (row_max ($1) ($5) ($2)); row_max ($1) ($5) ($2) } |
	formula i ASSOP COLMAX r TERMINATION		{ printf "\n%s\n" "col_max on previous sheet"; print_sheet (col_max ($1) ($5) ($2)); col_max ($1) ($5) ($2) } |

	formula i ASSOP ADD r c TERMINATION			{ printf "\n%s\n" "add_const on previous sheet"; print_sheet (add_const ($1) ($5) ($6) ($2)); add_const ($1) ($5) ($6) ($2) } |
	formula i ASSOP ADD c r TERMINATION			{ printf "\n%s\n" "add_const on previous sheet"; print_sheet (add_const ($1) ($6) ($5) ($2)); add_const ($1) ($6) ($5) ($2) } |
	formula i ASSOP SUBT r c TERMINATION		{ printf "\n%s\n" "subt_const on previous sheet"; print_sheet (subt_const ($1) ($5) ($6) ($2)); subt_const ($1) ($5) ($6) ($2) } |
	formula i ASSOP SUBT c r TERMINATION		{ printf "\n%s\n" "subt_const on previous sheet"; print_sheet (subt_const ($1) ($6) (-.($5)) ($2)); subt_const ($1) ($6) (-.($5)) ($2) } |
	formula i ASSOP MULT r c TERMINATION		{ printf "\n%s\n" "mult_const on previous sheet"; print_sheet (mult_const ($1) ($5) ($6) ($2)); mult_const ($1) ($5) ($6) ($2) } |
	formula i ASSOP MULT c r TERMINATION		{ printf "\n%s\n" "mult_const on previous sheet"; print_sheet (mult_const ($1) ($6) ($5) ($2)); mult_const ($1) ($6) ($5) ($2) } |
	formula i ASSOP DIV r c TERMINATION			{ printf "\n%s\n" "div_const on previous sheet"; print_sheet (div_const ($1) ($5) ($6) ($2)); div_const ($1) ($5) ($6) ($2) } |
	formula i ASSOP DIV c r TERMINATION			{ printf "\n%s\n" "div_const on previous sheet"; print_sheet (div_const ($1) ($6) (1./.($5)) ($2)); div_const ($1) ($6) (1./.($5)) ($2) } |

	formula i ASSOP ADD r r TERMINATION			{ printf "\n%s\n" "add_range on previous sheet"; print_sheet (add_range ($1) ($5) ($6) ($2)); add_range ($1) ($5) ($6) ($2) } |
	formula i ASSOP SUBT r r TERMINATION			{ printf "\n%s\n" "subt_range on previous sheet"; print_sheet (subt_range ($1) ($5) ($6) ($2)); subt_range ($1) ($5) ($6) ($2) } |
	formula i ASSOP MULT r r TERMINATION			{ printf "\n%s\n" "mult_range on previous sheet"; print_sheet (mult_range ($1) ($5) ($6) ($2)); mult_range ($1) ($5) ($6) ($2) } |
	formula i ASSOP DIV r r TERMINATION			{ printf "\n%s\n" "div_range on previous sheet"; print_sheet (div_range ($1) ($5) ($6) ($2)); div_range ($1) ($5) ($6) ($2) }	
;
;

/*Range*/
r:
	  LPAREN i COLON i RPAREN		{ (($2),($4)) }
;

/*Index*/
i:
	  LBRACKET c COMMA c RBRACKET		{ (($2),($4)) } /*Type will be type checked in backend*/
;

/*Constant*/
c:
	 expr	{ $1 }
;

/*Reducing Mathematical operations*/
expr:
	  term	{ $1 }
	| expr PLUS term	{ $1 +. $3 }
	| expr MINUS term		{ $1 -. $3 }
;

term:
	  factor	{ $1 }
	| term DIVIDE factor	{ $1 /. $3 }
	| term TIMES factor		{ $1 *. $3 }
;

factor:
	  FLOAT		{ float_of_string($1) }
	| MINUS factor			{ -. $2 }
	| LPAREN expr RPAREN 	{ $2 }
;
