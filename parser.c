/*****************************************************************************
File Name: parser.c
Compiler: Visual Studio 2013 Compiler
Author: Brandon Keohane
Course: Computer Engineering Technology Lab: 012
Assignment: Assignment 01 - The Parser
Date: 8th December, 2016
Professor: Svillen Ranev
Purpose: Parser through buffer to see if the program has correct syntax
Function List:
-> mlwpar_next_token() parser(Buffer*), match(int, int), syn_eh(int),
syn_printe(), gen_incode(char*), program(), opt_statements(),
statements(), statements_p(), statement(), assignment_statement(),
assignment_expression(), selection_statement(), iteration_statement(),
input_statement(), variable_list(), variable_list_p(), opt_variable_list(),
variable_identifier(), output_statement(), output_list(),
arithmetic_expression(), unary_arithmetic_expression(),
additive_arithmetic_expression(), additive_arithmetic_expression_p(),
multiplicative_arithmetic_expression(), multiplicative_arithmetic_expression_p(),
primary_arithmetic_expression(), string_expression(), string_expression_p(),
primary_string_expression(), conditional_expression(), logical_or_expression(),
logical_or_expression_p(), logical_and_expression(), logical_and_expression_p(),
relational_expression(), primary_a_relational_expression(),
primary_s_relational_expression(), primary_a_relational_expression_p(),
primary_s_relational_expression_p()
*****************************************************************************/
#include "parser.h"

static Token lookahead;
static Buffer* sc_buf;
int synerrno;

/*****************************************************************************
Purpose:			Starts parsing the buffer
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	mlwpar_next_token, program, match, gen_incode
Parameters:			in_buf - buffer to parse
Return Value:		void
Algorithm:			Initialize variables and starts the parsing
*****************************************************************************/
void parser(Buffer* in_buf){
	/* Error checking */
	if (in_buf == NULL) return;
	/* Proceed with initialization and parsing */
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*****************************************************************************
Purpose:			Checks if the next token corresponds to what the parser needs
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	mlwpar_next_token, syn_printe, syn_eh
Parameters:			pr_token_code - code the parser needs
					pr_token_attribute - attribute the parser needs or 
						NO_ATTR(-1) if there is not corresponding attribute 
						needing to be checked
Return Value:		void
Algorithm:			
	-> If there is no attribute to check
		-> If the lookahead code matches the pr code
			-> Advance the lookahead to the next token
			-> If next token is error print error, advance token, 
				increment error, and return
			-> Else call syn_eh
	-> Else check if code and attribute match
		-> If the lookahead code matches the pr code
			-> Advance the lookahead to the next token
			-> If next token is error print error, advance token, 
				increment error, and return
*****************************************************************************/
void match(int pr_token_code, int pr_token_attribute){

	/* If code does not match */
	if (lookahead.code != pr_token_code) {
		/* Call simple panic mode error recovery */
		syn_eh(pr_token_code);
		return;
	}

	/* If not attribute for comparison */
	if (pr_token_attribute == NO_ATTR) {
		/* Advance to next token */
		lookahead = mlwpar_next_token(sc_buf);

		/* If error occured in loop */
		if (lookahead.code == ERR_T){
			/* Print error */
			syn_printe();
			/* Advances to the next input token */
			lookahead = mlwpar_next_token(sc_buf);
			/* Increment the error counter */
			synerrno++;
			/* Return after completing error */
			return;
		}
		return;
	}

	/* If codes equal Contains an attribute to check */
	switch (pr_token_code){
		/* Keyword token code */
		case KW_T: case LOG_OP_T: case ART_OP_T: case REL_OP_T:
			if (pr_token_attribute == lookahead.attribute.get_int) {
				/* Advance to next token */
				lookahead = mlwpar_next_token(sc_buf);

				/* If error occured in loop */
				if (lookahead.code == ERR_T){
					/* Print error */
					syn_printe();
					/* Advances to the next input token */
					lookahead = mlwpar_next_token(sc_buf);
					/* Increment the error counter */
					synerrno++;
					/* Return after completing error */
					return;
				}
				return;
			}
			break;
		default: 
			break;
	}
	/* Didnt find match */
	syn_eh(pr_token_code);
}

/*****************************************************************************
Purpose:			Used for implementing simple panic mode error recovery
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	syn_printe, mlwpar_next_token, exit
Parameters:			sync_token_code - Loops to try and find this code in buffer
Return Value:		void
Algorithm:			
-> First calls the syn printe
-> Increments the error count
-> Loop until you find the correct token
-> If found SEOF_T and you were not looking for it call exit (error number as code)
-> If found SEOF_T and you were looking for it just return
-> If match was found advance to next token and return
*****************************************************************************/
void syn_eh(int sync_token_code){
	/* First call the syn printe */
	syn_printe();
	/* Increment error count */
	synerrno++;
	/* Panic mode error recovery */
	/* Loop until you find the correct code or SEOF */
	while (lookahead.code != sync_token_code && lookahead.code != SEOF_T)
		lookahead = mlwpar_next_token(sc_buf);

	/* If SEOF code and it was not looking for it */
	if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) exit(synerrno);
	/* If code to find was SEOF */
	if (lookahead.code == SEOF_T && sync_token_code == SEOF_T) return;
	/* If match was found */
	if (lookahead.code == sync_token_code){
		/* Increment one more */
		lookahead = mlwpar_next_token(sc_buf);
		return;
	}
}

/*****************************************************************************
Purpose:			Prints unique error and what line it occured on
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	printf, b_cbhead, 
Parameters:			void
Return Value:		void
Algorithm:			Print the error corresponding to the token
*****************************************************************************/
void syn_printe(void){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
		case  ERR_T: /* ERR_T 0 Error token */
			printf("%s\n", t.attribute.err_lex);
			break;
		case  SEOF_T: /*SEOF_T 1 Source end-of-file token */
			printf("NA\n");
			break;
		case  AVID_T: /* AVID_T 2 Arithmetic Variable identifier token */
		case  SVID_T:/* SVID_T 3 String Variable identifier token */
			printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
			break;
		case  FPL_T: /* FPL_T 4 Floating point literal token */
			printf("%5.1f\n", t.attribute.flt_value);
			break;
		case INL_T: /* INL_T 5 Integer literal token */
			printf("%d\n", t.attribute.get_int);
			break;
		case STR_T:/* STR_T 6 String literal token */
			printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
			break;

		case SCC_OP_T: /* 7 String concatenation operator token */
			printf("NA\n");
			break;

		case  ASS_OP_T:/* ASS_OP_T 8 Assignment operator token */
			printf("NA\n");
			break;
		case  ART_OP_T:/* ART_OP_T 9 Arithmetic operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  REL_OP_T: /* REL_OP_T 10 Relational operator token */
			printf("%d\n", t.attribute.get_int);
			break;
		case  LOG_OP_T:/* LOG_OP_T 11  Logical operator token */
			printf("%d\n", t.attribute.get_int);
			break;

		case  LPR_T: /* LPR_T 12  Left parenthesis token */
			printf("NA\n");
			break;
		case  RPR_T: /* RPR_T 13  Right parenthesis token */
			printf("NA\n");
			break;
		case LBR_T: /* 14 Left brace token */
			printf("NA\n");
			break;
		case RBR_T: /* 15 Right brace token */
			printf("NA\n");
			break;

		case KW_T: /* 16 Keyword token */
			printf("%s\n", kw_table[t.attribute.get_int]);
			break;

		case COM_T: /* 17 Comma token */
			printf("NA\n");
			break;
		case EOS_T: /* 18 End of statement *(semi - colon) */
			printf("NA\n");
			break;
		default:
			printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	} /* end switch */
} /* end syn_printe */

/*****************************************************************************
Purpose:			Generates the output of the parser
Author:				Brandon Keohane
History/Version:	v1.0
Called Functions:	printf
Parameters:			void
Return Value:		void
Algorithm:			Generate output
*****************************************************************************/
void gen_incode(char* string){
	printf("%s\n", string);
}

/*****************************************************************************
Production:	<program> -> 
				PLATYPUS {<opt_statements>}
FIRST(<program>) -> {KW_T(PLATYPUS)}
*****************************************************************************/
void program(void){
	match(KW_T, PLATYPUS);	/* PLATYPUS */
	match(LBR_T, NO_ATTR);	/* { */
	opt_statements();		/* opt_statements(); */
	match(RBR_T, NO_ATTR);	/* } */
	gen_incode("PLATY: Program parsed");
}

/*****************************************************************************
Production: <statements> | e
FIRST(<opt_statements>) -> { AVID, SVID, IF, USING, INPUT, OUTPUT, e}
*****************************************************************************/
void opt_statements(void){
	switch (lookahead.code){
		case AVID_T: case SVID_T: 
			statements(); 
			break;
		case KW_T:
			if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT){
				statements();
				break;
			}
		default:
			gen_incode("PLATY: Opt_statements parsed");
	}
}

/*****************************************************************************
Production: <statements> -> 
					<statement><statements’>
FIRST(<statements>) -> { AVID, SVID, KW_T(IF), KW_T(USING), 
							KW_T(INPUT), KW_T(OUTPUT) }
*****************************************************************************/
void statements(void){
	statement(); statements_p();
}

/*****************************************************************************
Production: <statements’ -> 
					<statement><statements’> | e
FIRST(<statements’>) -> { AVID, SVID, KW_T(IF), KW_T(USING), 
							KW_T(INPUT), KW_T(OUTPUT), e}
*****************************************************************************/
void statements_p(void){
	switch (lookahead.code){
		case AVID_T: case SVID_T:
			statement(); statements_p(); 
			break;
		case KW_T:
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT){
				statement(); statements_p();
			}
			break;
		default: break;
	}
}

/*****************************************************************************
Production: <statement> ->
	<assignment statement>
	| <selection statement>
	| <iteration statement>
	| <input statement>
	| <output statement>
FIRST(<statement>) -> {AVID, SVID, KW_T(IF), KW_T(USING), 
							KW_T(INPUT), KW_T(OUTPUT)}
*****************************************************************************/
void statement(void){
	switch (lookahead.code){
		case AVID_T: case SVID_T:
			assignment_statement(); 
			break;
		case KW_T:
			switch (lookahead.attribute.get_int){
				case IF:		
					selection_statement(); 
					break;
				case USING:		
					iteration_statement(); 
					break;
				case INPUT:		
					input_statement(); 
					break;
				case OUTPUT:	
					output_statement(); 
					break;
			}
			break;
		default:
			syn_printe();
	}
}

/*****************************************************************************
Production: <assignment statement> -> 
							<assignment expression>;
FIRST(<assignment statement>) -> {AVID, SVID}
*****************************************************************************/
void assignment_statement(void){
	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*****************************************************************************
Production: <assignment expression> -> 
										AVID = <arithmetic expression>
										| SVID = <string expression>
FIRST(<assignment expression>) -> {AVID, SVID}
*****************************************************************************/
void assignment_expression(void){
	switch (lookahead.code){
		case AVID_T:
			match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); arithmetic_expression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed");
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); string_expression();
			gen_incode("PLATY: Assignment expression (string) parsed");
			break;
		default:
			syn_printe();
	}
}

/*****************************************************************************
Production: <selection statement> -> 
				IF (<conditional expression>) 
				THEN <opt_statements>
				ELSE {<opt_statements>};
FIRST(<selection statement>) -> {KW_T(IF)}
*****************************************************************************/
void selection_statement(void){
	match(KW_T, IF); match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed");
}

/*****************************************************************************
Production: <iteration statement> ->
			USING (<assignment expression>,	
					<conditional expression>, 
					<assignment expression>)
			REPEAT {
				<opt_statements>
			};
FIRST(<assignment statement>) -> {USING}
*****************************************************************************/
void iteration_statement(void){
	match(KW_T, USING);
	match(LPR_T, NO_ATTR);
	assignment_expression();
	match(COM_T, NO_ATTR);
	conditional_expression();
	match(COM_T, NO_ATTR);
	assignment_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: USING statement parsed");
}

/*****************************************************************************
Production: <input statement> -> 
								INPUT (<variable list>);
FIRST(<input statement>) -> {KW_T(INPUT)}
*****************************************************************************/
void input_statement(void){
	match(KW_T, INPUT);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed");
}

/*****************************************************************************
Production: <variable list> ->
						<variable identifier><variable list’>
FIRST(<variable list>) -> {AVID_T, SVID_T}
*****************************************************************************/
void variable_list(void){
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

/*****************************************************************************
Production: ,<variable identifier><variable list’> | e
FIRST(<variable list’>) -> {‘,’ , e}
*****************************************************************************/
void variable_list_p(void){
	if (lookahead.code == COM_T){
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/*****************************************************************************
Production: <opt_variable list> ->
						<variable list> | e
FIRST(<opt_variable list>) -> {AVID_T, SVID_T, e}
*****************************************************************************/
void opt_variable_list(void){
	switch (lookahead.code){
		case AVID_T: case SVID_T:
			variable_list();
			break;
		default:
			gen_incode("PLATY: opt variable list parsed");
			break;
	}
}

/*****************************************************************************
Production: <variable identifier> -> 
								AVID_T | SVID_T
FIRST(<variable identifier>) -> {AVID_T, SVID_T}
*****************************************************************************/
void variable_identifier(void){
	switch (lookahead.code){
		case AVID_T: 
			match(AVID_T, NO_ATTR); break;
		case SVID_T: 
			match(SVID_T, NO_ATTR); break;
		default: 
			syn_printe();
	}
}

/*****************************************************************************
Production: <output statement> ->
						OUTPUT (<output list>);
FIRST(<output statement>) -> {KW_T(OUTPUT)}
*****************************************************************************/
void output_statement(void){
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed");
}

/*****************************************************************************
Production: <output list> -> 
				<opt_variable list> | STR_T
FIRST(<output list>) -> {AVID_T, SVID_T, e, STR_T}
*****************************************************************************/
void output_list(void){
	switch (lookahead.code){
		case AVID_T: case SVID_T:
			opt_variable_list();
			break;
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Output list (string literal) parsed");
			break;
		default:
			gen_incode("PLATY: Output list (empty) parsed");
	}
}

/*****************************************************************************
Production: <arithmetic expression> -> 
				<unary arithmetic expression> | <additive arithmetic expression>
FIRST(<arithmetic expression>) -> {-, +, AVID_T, FPL_T, INT_L, ( }
*****************************************************************************/
void arithmetic_expression(void){
	switch (lookahead.code){
		case ART_OP_T:
			if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS){
				unary_arithmetic_expression();
				gen_incode("PLATY: Arithmetic expression parsed");
			}
			else
				syn_printe();
			break;
		case AVID_T: case FPL_T: case INL_T: case LPR_T:
			additive_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************************************************
Production: <unary arithmetic expression> -> 
								-<primary arithmetic expression>
								| +<primary arithmetic expression>
FIRST(<unary arithmetic expression >) -> {-,+}
*****************************************************************************/
void unary_arithmetic_expression(void){
	switch (lookahead.attribute.arr_op){
		case PLUS:
			match(ART_OP_T, PLUS); primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			break;
		case MINUS:
			match(ART_OP_T, MINUS); primary_arithmetic_expression();
			gen_incode("PLATY: Unary arithmetic expression parsed");
			break;
		default:
			syn_printe();
			synerrno++;
			break;
	}
}

/*****************************************************************************
Production: <additive arithmetic expression> ->
	<multiplicative arithmetic expression><additive arithmetic expression’>
FIRST(<additive arithmetic expression>) -> { AVID_T, FPL_T, INT_L, ( }
*****************************************************************************/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*****************************************************************************
Production: <additive arithmetic expression’> -> 
	+<multiplicative arithmetic expression><additive arithmetic expression’>  
| -<multiplicative arithmetic expression><additive arithmetic expression’>  
| e
FIRST(<additive arithmetic expression’>) -> {+,-,e}
*****************************************************************************/
void additive_arithmetic_expression_p(void){
	switch (lookahead.code){
		case ART_OP_T:
			if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS){
				match(ART_OP_T, lookahead.attribute.arr_op);
				multiplicative_arithmetic_expression();
				additive_arithmetic_expression_p();
				gen_incode("PLATY: Additive arithmetic expression parsed");
				break;
			}
			syn_printe();
			break;
		default:
			break;
	}
}

/*****************************************************************************
Production: <multiplicative arithmetic expression> ->
	<primary arithmetic expression><multiplicative arithmetic expression’>
FIRST(<multiplicative arithmetic expression>) -> { AVID_T, FPL_T, INT_L, ( }
*****************************************************************************/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*****************************************************************************
Production: <multiplicative arithmetic expression’> ->
	*<primary arithmetic expression><multiplicative arithmetic expression’> 
	| /<primary arithmetic expression><multiplicative arithmetic expression’> 
	| e
FIRST(<multiplicative arithmetic expression’>) -> {*,/,e}
*****************************************************************************/
void multiplicative_arithmetic_expression_p(void){
	switch (lookahead.code){
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
	default:
		break;
	}
}

/*****************************************************************************
Production: <primary arithmetic expression> ->
			AVID_T
			| FPL_T
			| INT_T
			| (<arithmetic expression>)
FIRST(<primary arithmetic expression>) -> { AVID_T, FPL_T, INT_L, ( }
*****************************************************************************/
void primary_arithmetic_expression(void){
	switch (lookahead.code){
		case AVID_T: 
			match(AVID_T, NO_ATTR); 
			gen_incode("PLATY: Primary arithmetic expression parsed"); 
			break;
		case FPL_T: 
			match(FPL_T, NO_ATTR); 
			gen_incode("PLATY: Primary arithmetic expression parsed"); 
			break;
		case INL_T: 
			match(INL_T, NO_ATTR); 
			gen_incode("PLATY: Primary arithmetic expression parsed"); 
			break;
		case LPR_T:
			match(LPR_T, NO_ATTR);
			arithmetic_expression();
			match(RPR_T, NO_ATTR);
			gen_incode("PLATY: Primary arithmetic expression parsed"); 
			break;
		default: syn_printe(); 
			break;
	}
}

/*****************************************************************************
Production: <string expression> -> 
				<primary string expression><string expression’>
FIRST(<string expression>) -> {SVID_T, STR_T}
*****************************************************************************/
void string_expression(void){
	primary_string_expression();
	gen_incode("PLATY: Primary string expression parsed");
	string_expression_p();
}

/*****************************************************************************
Production: <string expression’> -> 
				# <primary string expression><string expression’> | e
FIRST(<string expression’>) -> {#,e}
*****************************************************************************/
void string_expression_p(void){
	switch (lookahead.code){
	case SCC_OP_T: 
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		gen_incode("PLATY: Primary string expression parsed");
		string_expression_p();
		break;
	default:
		gen_incode("PLATY: String expression parsed");
		break;
	}
}

/*****************************************************************************
Production: <primary string expression> -> SVID_T | STR_T
FIRST(<primary string expression>) -> {SVID_T, STR_T}
*****************************************************************************/
void primary_string_expression(void){
	switch (lookahead.code){
		case SVID_T: 
			match(SVID_T, NO_ATTR); 
			break;
		case STR_T: 
			match(STR_T, NO_ATTR); 
			break;
		default: syn_printe(); 
			break;
	}
}

/*****************************************************************************
Production: <conditional expression> -> 
								<logical OR expression>
FIRST(<conditional expression>) -> { AVID_T, FPL_T, INT_T, SVID_T, STR_T }
*****************************************************************************/
void conditional_expression(void){
	logical_or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*****************************************************************************
Production: <logical OR expression> -> 
	<logical AND expression><logical OR expression’>
FIRST(<logical OR expression>) -> { AVID_T, FPL_T, INT_T, SVID_T, STR_T }
*****************************************************************************/
void logical_or_expression(void){
	logical_and_expression();
	logical_or_expression_p();
}

/*****************************************************************************
Production: <logical OR expression’> ->
	.OR. <logical AND expression><logical OR expression’> | e 
FIRST(<logical OR expression’>) -> {.OR., e}
*****************************************************************************/
void logical_or_expression_p(void){
	switch (lookahead.code){
		case LOG_OP_T:
			if (lookahead.attribute.log_op == OR){
				match(LOG_OP_T, OR); logical_and_expression(); logical_or_expression_p();
				gen_incode("PLATY: Logical OR expression parsed");
				break;
			}
		default:
			break;
	}
}

/*****************************************************************************
Production: <logical AND expression> ->
	<relational expression><logical AND expression’>
FIRST(<logical AND expression>) -> { AVID_T, FPL_T, INT_T, SVID_T, STR_T }
*****************************************************************************/
void logical_and_expression(void){
	relational_expression();
	logical_and_expression_p();
}

/*****************************************************************************
Production: <logical AND expression’> -> 
			.AND. <relational expression><logical AND expression’> | e
FIRST(<logical AND expression’>) -> {.AND., e}
*****************************************************************************/
void logical_and_expression_p(void){
	switch (lookahead.code){
		case LOG_OP_T:
			if (lookahead.attribute.log_op == AND){
				match(LOG_OP_T, AND); relational_expression(); logical_and_expression_p();
				gen_incode("PLATY: Logical AND expression parsed");
				break;
			}
		default:
			break;
	}
}

/*****************************************************************************
Production: <relational expression> -> 
	<primary a_relational expression><primary a_relational expression’> 
	| <primary s_relational expression><primary s_relational expression’>
FIRST(<relational expression>) -> { AVID_T, FPL_T, INT_T, SVID_T, STR_T }
*****************************************************************************/
void relational_expression(void){
	switch (lookahead.code){
		case AVID_T: case FPL_T: case INL_T:
			primary_a_relational_expression(); 
			gen_incode("PLATY: Primary a_relational expression parsed");
			primary_a_relational_expression_p();
			gen_incode("PLATY: Primary a_relational expression parsed");
			gen_incode("PLATY: Relational expression parsed");
			break;
		case SVID_T: case STR_T:
			primary_s_relational_expression(); 
			gen_incode("PLATY: Primary s_relational expression parsed");
			primary_s_relational_expression_p();
			gen_incode("PLATY: Primary s_relational expression parsed");
			gen_incode("PLATY: Relational expression parsed");
			break;
		default:
			syn_printe();
			gen_incode("PLATY: Relational expression parsed");
			break;
	}
}

/*****************************************************************************
Production: <primary a_relational expression> ->
			AVID_T
			| FPL_T
			| INT_T
FIRST(<primary a_relational expression>) -> {AVID_T, FPL_T, INT_T}
*****************************************************************************/
void primary_a_relational_expression(void){
	switch (lookahead.code){
		case AVID_T: 
			match(AVID_T, NO_ATTR); 
			break;
		case FPL_T: 
			match(FPL_T, NO_ATTR);
			break;
		case INL_T: 
			match(INL_T, NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************************************************
Production: <primary s_relational expression> -> 
			<primary string expression>
FIRST(<primary s_relational expression>) -> { SVID_T, STR_T }
*****************************************************************************/
void primary_s_relational_expression(void){
	switch (lookahead.code){
		case SVID_T: 
			match(SVID_T, NO_ATTR);
			gen_incode("PLATY: Primary string expression parsed");
			break;
		case STR_T: 
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Primary string expression parsed");
			break;
		default:
			syn_printe();
			break;
	}
}

/*****************************************************************************
Production: <primary a_relational expression’> ->
			== <primary a_relational expression> 
			|<> <primary a_relational expression> 
			| > <primary a_relational expression> 
			| < <primary a_relational expression>
			| e
FIRST(<primary a_relational expression’>) -> {==,<>,>,<,e}
*****************************************************************************/
void primary_a_relational_expression_p(void){
	switch (lookahead.code){
		case REL_OP_T:
			switch (lookahead.attribute.rel_op){
				case EQ: 
					match(REL_OP_T, EQ); primary_a_relational_expression(); 
					break;
				case NE: 
					match(REL_OP_T, NE); primary_a_relational_expression(); 
					break;
				case GT: 
					match(REL_OP_T, GT); primary_a_relational_expression(); 
					break;
				case LT: 
					match(REL_OP_T, LT); primary_a_relational_expression();
					break;
				default:
					break;
			}
			break;
		default:
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;
	}
}

/*****************************************************************************
Production: <primary s_relational expression’> -> 
			== <primary s_relational expression>
			| <> <primary s_relational expression>
			| > <primary s_relational expression> 
			| < <primary s_relational expression> 
			| e
FIRST(<primary s_relational expression’>) -> {==,<>,>,<,e}
*****************************************************************************/
void primary_s_relational_expression_p(void){
	switch (lookahead.code){
		case REL_OP_T:
			switch (lookahead.attribute.rel_op){
				case EQ: match(REL_OP_T, EQ); primary_s_relational_expression(); break;
				case NE: match(REL_OP_T, NE); primary_s_relational_expression(); break;
				case GT: match(REL_OP_T, GT); primary_s_relational_expression(); break;
				case LT: match(REL_OP_T, LT); primary_s_relational_expression(); break;
				default: break;
			}
			break;
		default:
			gen_incode("PLATY: Primary s_relational expression parsed");
			break;
	}
}