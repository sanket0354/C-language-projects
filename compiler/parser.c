/*********************************************************************************

File Name :		parser.c
Compiler :		MS VISUAL STUDIO 2013
Author :		Sanket Patel (040809189) & Yasser Noor (040815665)
Course :		CST8152
Assignment :	4
Date :			08 - December - 2016
Professor :		Svillen Ranev
Purpose :		
Function List :  parser(Buffer*), match(int, int ), syn_eh(int), syn_printe(), 
				 gen_incode(char*), program(), opt_statements(), statements(), 
				 statement(), statements_p(), 
				 assignment_statement(), assignment_expression(), selection_statement(), 
				 iteration_statement(),
				 input_statement(), output_statement(), variable_list(),
				 variable_list_p(), variable_identifier(), conditional_expression(),
				 logical_OR_expression(), logical_OR_expression_p(), logical_AND_expression(), 
				 logical_AND_expression_p(), relational_expression(), 
				 primary_a_relational_expression(),
				 primary_a_relational_expression_p(), primary_s_relational_expression(),
				 primary_s_relational_expression_p(), arithmetic_expression(),
				 unary_arithmetic_expression(),
				 additive_arithmetic_expression(), primary_aithmetic_expression(),
				 additive_arithmetic_expression_p(), multiplicative_arithmetic_expression(),
				 multiplicative_arithmetic_expression_p(), string_expression(),
				 primary_string_expression(), string_expression_p(), output_list(),

**********************************************************************************/

#include "parser.h"
void parser(Buffer* in_buff){
	sc_buf = in_buff;
	lookahead = mlwpar_next_token(sc_buf);
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed\n");
}


/***********************************************************************************

Purpose :			To match the token required by parser
Author :			Sanket Patel & Yasser Noor
History/Version :	1.0
Called functions :	mlwpar_next_token(), syn_eh(), syn_printe()
parameters :		pr_token_code: int, token code required
					pr_token_attribute: int, toke attribute required if any
Return value :		none
Algorithm :			-check for token code
					- check for SEOF
					- check for token attribute if any
					- call the next token
					- end of error token


***********************************************************************************/
void match(int pr_token_code, int pr_token_attribute){
	/*
	* If the lookahead code is not the token code we are looking for
	* then call the error handling function and return
	*/
	if (lookahead.code != pr_token_code){
		syn_eh(pr_token_code);
		return;
	}
	/*
	* if lookahead code is Source-EOF, then return
	*/
	if (lookahead.code == SEOF_T)
		return;

	switch (pr_token_code){
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.log_op){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.arr_op){
			syn_eh(pr_token_code);
			return;
		}
		break;
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.rel_op){
			syn_eh(pr_token_code);
			return;
		}
		break;

	}

	lookahead = mlwpar_next_token(sc_buf);

	if (lookahead.code == ERR_T){
		syn_printe();
		lookahead = mlwpar_next_token(sc_buf);
		synerrno++;
		return;
	}

}

/***********************************************************************************

Purpose :			To count syntax errors and move further in program
Author :			Sanket Patel
History/Version :	1.0
Called functions :	syn_printe(), mlwpar_next_token(), exit()
parameters :		sync_token_code, int : found token code
Return value :		none
Algorithm :			- call the error printing function
					- increase the number of errors
					- lookup until valid token is found, 
					- end the program if SEOF is found

***********************************************************************************/
void syn_eh(int sync_token_code){
	syn_printe();
	synerrno++;

	do{
		lookahead = mlwpar_next_token(sc_buf);
		if (lookahead.code == sync_token_code){
			lookahead = mlwpar_next_token(sc_buf);
			return;
		}

		if (lookahead.code == SEOF_T){
			exit(synerrno);
			return;
		}

	} while (lookahead.code != sync_token_code);
}

/*********************************************************************************
Parser error printing function, Assignmet 4, F16
************************************************************************************/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/* end switch*/
}/* end syn_printe()*/

void gen_incode(char* print_string){
	printf("%s", print_string);
}
/**********************************************************************************
* <program>  -> PLATYPUS {<opt_statements>}
* FIRST(<program>) = {KW_T(PLATYPUS)}
***********************************************************************************/
void program(void){
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

/**********************************************************************************
* Author : Sanket Patel
* < opt_statements >  -> <statements> | E 
* FIRST(<opt_statements>) = {E, AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
***********************************************************************************/
void opt_statements(){
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code){
	case AVID_T:
	case SVID_T: statements(); break;
	case KW_T:
		/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
		statements_p()*/
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT){
			statements();
			break;
		}
	default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed\n");
	}
}

/*********************************************************************************
* Author : Sanket Patel
* <statements> -> <statement><statements’>
* FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT), KW_T(OUTPUT)}
***********************************************************************************/
void statements(void){
	statement();
	statements_p();
}

/*********************************************************************************
* Author : Sanket Patel
<statement> -> <assignment statement> | <selection statement> |
				 <iteration statement> | <input statement> | 
				 <output statement>

FIRST(<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT),               

***********************************************************************************/
void statement(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T: assignment_statement(); break;
	case KW_T:
		if (lookahead.attribute.get_int == IF)
			selection_statement();
		else if (lookahead.attribute.get_int == USING)
			iteration_statement();
		else if (lookahead.attribute.get_int == INPUT)
			input_statement();
		else if (lookahead.attribute.get_int == OUTPUT)
			output_statement();
		break;

	}
}
/*********************************************************************************
* Author : Sanket Patel
<statements’> -> <statement><statements’> | E

FIRST(<statements>) = {E, AVID_T, SVID_T, KW_T(IF), KW_T(USING), KW_T(INPUT),                 KW_T(OUTPUT)}

***********************************************************************************/
void statements_p(void){
	switch (lookahead.code){
	case AVID_T:
	case SVID_T:
		statement();
		statements_p();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS
			&& lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN
			&& lookahead.attribute.get_int != REPEAT){
			statement();
			statements_p();
		}
	default:
		break;
	}
}

/*********************************************************************************
* Author : Yasser Noor
<assignment statement> -> <assignment expression>;

FIRST(<assignment statement>) = {AVID_T, SVID_T}
***********************************************************************************/
void assignment_statement(void){
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*********************************************************************************
* Author : Yasser Noor
< assignment expression> ->AVID = <arithmetic expression> | SVID = <string expression>

FIRST(<assignment expression>) = {AVID_T, SVID_T}

***********************************************************************************/
void assignment_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed\n");
		break;

	default:
		syn_printe();
	}
}
/*********************************************************************************
* Author : Yasser Noor
<selection statement> -> IF (<conditional expression>)  THEN  <opt_statements>
						 ELSE { <opt_statements> } ;

FIRST(<selection statement>) = {KW_T(IF)}
***********************************************************************************/
void selection_statement(void){
	match(KW_T, IF);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	opt_statements();
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: IF statement parsed\n");
}
/*********************************************************************************
* Author : Yasser Noor
<iteration statement> -> USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
						 REPEAT {<opt_statements>};

FIRST(<iteration statement>) = {KW_T(USING)}
***********************************************************************************/
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
	gen_incode("PLATY: USING statement parsed\n");
}

/*********************************************************************************
* Author : Yasser Noor
<input statement> -> INPUT (<variable list>);

FIRST(<input statement>) = {KW_T(INPUT)}
***********************************************************************************/
void input_statement(void){
	match(KW_T, INPUT);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed\n");
}

/*********************************************************************************
* Author : Yasser Noor
<variable list> -> <variable identifier><variable list’>

FIRST(<variable list>) = {AVID_T, SVID_T}
***********************************************************************************/
void variable_list(void){
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed\n");
}
/*********************************************************************************
* Author : Yasser Noor
<variable list’> -> ,<variable identifier><variable list’> | E

FIRST(<variable list’>) = {E, COM_T}
***********************************************************************************/
void variable_list_p(void){
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
		break;
	default:
		break;
	}
}

/*********************************************************************************
* Author : Yasser Noor
<variable identifier> ->AVID_T | SVID_T

FIRST(<variable identifier>) = { AVID_T, SVID_T }
***********************************************************************************/
void variable_identifier(void){
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*********************************************************************************
* Author : Sanket Patel
<output statement> -> OUTPUT (<output list>);

FIRST(<output statement>) = {KW_T(OUTPUT)}
***********************************************************************************/
void output_statement(void){
	match(KW_T, OUTPUT);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: OUTPUT statement parsed\n");

}

/*********************************************************************************
* Author : Sanket Patel
<output list> -> <variable list> | STR_T | E

FIRST(<output list>) = {E, AVID_T, SVID_T, STR_T}
***********************************************************************************/
void output_list(void){
	switch (lookahead.code){
	case SVID_T:
	case AVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed\n");
		break;
	}
}

/*********************************************************************************
* Author : Sanket Patel
<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>

FIRST(<arithmetic expression>) = { ART_OP_T(PLUS), ART_OP_T(MINUS), AVID_T, FPL_T, INL_T, LPR_T }
***********************************************************************************/
void arithmetic_expression(void){
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
		gen_incode("PLATY: Arithmetic expression parsed\n");
		break;

	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed\n");
		break;

	default:
		syn_printe();
		break;
	}

}

/*********************************************************************************
* Author : Sanket Patel
<unary arithmetic expression> -> -  <primary arithmetic expression> | + <primary arithmetic expression>

FIRST(<unary arithmetic expression>) = { ART_OP_T(PLUS), ART_OP_T(MINUS)}
***********************************************************************************/
void unary_arithmetic_expression(void){
	switch (lookahead.code){

	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_aithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_aithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
	default:
		break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed\n");
}

/*********************************************************************************
* Author : Sanket Patel
<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression’>

FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
***********************************************************************************/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*********************************************************************************
* Author : Sanket Patel
<additive arithmetic expression’> -> +<multiplicative arithmetic expression><additive arithmetic expression’>
| -<multiplicative arithmetic expression><additive arithmetic expression’>
| E

FIRST(<additive arithmetic expression’>) = { E, ART_OP_T(PLUS), ART_OP_T(MINUS)}

***********************************************************************************/
void additive_arithmetic_expression_p(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed\n");
			break;

		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_p();
			gen_incode("PLATY: Additive arithmetic expression parsed\n");
			break;
		default:
			syn_printe();
			break;
		}
	}
}

/*********************************************************************************
* Author : Sanket Patel
<multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression’>

FIRST(<multiplicative arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
***********************************************************************************/
void multiplicative_arithmetic_expression(void){
	primary_aithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*********************************************************************************
* Author : Sanket Patel
<multiplicative arithmetic expression’> -> *<primary arithmetic expression> <multiplicative arithmetic expression’> 
										   | /<primary arithmetic expression> <multiplicative arithmetic expression’>
										   | E

FIRST(<multiplicative arithmetic expression’>) = {E, ART_OP_T(DIV), ART_OP_T(MULT)}
***********************************************************************************/
void multiplicative_arithmetic_expression_p(void){
	switch (lookahead.code){
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MULT:
			match(ART_OP_T, MULT);
			primary_aithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
			break;

		case DIV:
			match(ART_OP_T, DIV);
			primary_aithmetic_expression();
			multiplicative_arithmetic_expression_p();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
			break;
		}
	}
}



/*********************************************************************************
* Author : Sanket Patel
<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)

FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, LPR_T }
***********************************************************************************/
void primary_aithmetic_expression(void){
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed\n");
}

/*********************************************************************************
* Author : Sanket Patel
<string expression> -> <primary string expression><string expression’>

FIRST(<string expression>) = { SVID_T, STR_T }
***********************************************************************************/
void string_expression(void){
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed\n");
}

/*********************************************************************************
* Author : Sanket Patel
<string expression’> -> # <primary string expression><string expression’> | E

FIRST(<string expression’>) = { E, SCC_OP_T }
***********************************************************************************/
void string_expression_p(void){
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
		break;
	default:
		break;
	}
}

/*********************************************************************************
* Author : Sanket Patel
<primary string expression> -> SVID_T | STR_T

FIRST(<primary string expression>) = { SVID_T, STR_T }
***********************************************************************************/
void primary_string_expression(void){
	switch (lookahead.code)
	{
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary string expression parsed\n");
}
/*********************************************************************************
* Author : Yasser Noor
<conditional expression> -> <logical OR  expression>

FIRST(<conditional expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
***********************************************************************************/
void conditional_expression(void){
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed\n");
}

/*********************************************************************************
* Author : Yasser Noor
<logical  OR expression> -> <logical AND expression><logical OR expression’>

FIRST(<logical  OR expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
*/
void logical_OR_expression(void){
	logical_AND_expression();
	logical_OR_expression_p();
}

/*********************************************************************************
* Author : Yasser Noor
<logical OR expression’> -> .OR.<logical AND expression><logical OR expression’> | E

FIRST(<logical OR expression’>) = { E, LOG_OP_T(OR) }
***********************************************************************************/
void logical_OR_expression_p(void){
	switch (lookahead.code){

	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_p();
			gen_incode("PLATY: Logical OR expression parsed\n");

			break;
		default:
			break;
		}
	default:
		break;
	}
}

/*********************************************************************************
* Author : Yasser Noor
<logical  AND expression> -> <relational expression><logical AND expression’>

FIRST(<logical AND expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }
***********************************************************************************/
void logical_AND_expression(void){
	relational_expression();
	logical_AND_expression_p();
}


/*********************************************************************************
* Author : Yasser Noor
<logical AND expression’> -> .AND.<relational expression><logical AND expression’> | E

FIRST(<logical AND expression’>) = { E, LOG_OP_T(AND)  }

***********************************************************************************/
void logical_AND_expression_p(void){
	switch (lookahead.code){

	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_p();
			gen_incode("PLATY: Logical AND expression parsed\n");
			break;
		default:
			break;
		}
	default:
		break;
	}
}

/*********************************************************************************
* Author : Yasser Noor
<relational expression> -> <primary a_relational expression><primary a_relational expression tail>
						   | <primary s_relational expression><primary s_relational expression tail>

FIRST(<relational expression>) = { STR_T, SVID_T, AVID_T, FPL_T, INL_T }

***********************************************************************************/
void relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		primary_a_relational_expression_p();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		primary_s_relational_expression_p();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed\n");
}

/*********************************************************************************
* Author : Yasser Noor
<primary a_relational expression'> -> == <primary a_relational expression>
								      | <> <primary a_relational expression>
									  |  <  <primary a_relational expression>
									  |  >  <primary a_relational expression>

FIRST(<primary a_relational expression'>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT) }
***********************************************************************************/
void primary_a_relational_expression_p(void){
	switch (lookahead.code){

	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
			match(REL_OP_T, EQ);
			//primary_a_relational_expression();
			break;
		case NE:
			match(REL_OP_T, NE);
			//primary_a_relational_expression();
			break;
		case LT:
			match(REL_OP_T, LT);
			//primary_a_relational_expression();
			break;
		case GT:
			match(REL_OP_T, GT);
			//primary_a_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
	default:
		break;
	}
	primary_a_relational_expression();

}

/*********************************************************************************
* Author : Yasser Noor
<primary s_relational expression'> -> == <primary s_relational expression>
										  | <> <primary s_relational expression>
										  |  <  <primary s_relational expression>
										  |  >  <primary s_relational expression>

FIRST(<primary s_relational expression'>) = { REL_OP_T(EQ), REL_OP_T(NE), REL_OP_T(LT), REL_OP_T(GT) }

***********************************************************************************/
void primary_s_relational_expression_p(void){

	switch (lookahead.code){
	case REL_OP_T:
		switch (lookahead.attribute.rel_op)
		{
		case EQ:
			match(REL_OP_T, EQ);
			//primary_s_relational_expression();
			break;
		case NE:
			match(REL_OP_T, NE);
			//primary_s_relational_expression();
			break;
		case LT:
			match(REL_OP_T, LT);
			//primary_s_relational_expression();
			break;
		case GT:
			match(REL_OP_T, GT);
			//primary_s_relational_expression();
			break;
		default:
			syn_printe();
			break;
		}
	default:
		break;
	}
	primary_s_relational_expression();
}

/*********************************************************************************
* Author : Yasser Noor
<primary a_relational expression> -> AVID_T | FPL_T | INL_T

FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
***********************************************************************************/
void primary_a_relational_expression(void){
	switch (lookahead.code){
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed\n");

		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed\n");

		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed\n");

		break;
	case SVID_T:
	case STR_T:
		syn_printe();
		gen_incode("PLATY: Primary a_relational expression parsed\n");
		break;
	default:
		syn_printe();
		break;

	}


}

/*********************************************************************************
* Author : Yasser Noor
<primary s_relational expression> -> <primary string expression>

FIRST(<primary s_relational expression>) = { STR_T, SVID_T }
***********************************************************************************/
void primary_s_relational_expression(void){
	switch (lookahead.code){
	case SVID_T:
	case STR_T:
		primary_string_expression();
		gen_incode("PLATY: Primary s_relational expression parsed\n");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
		syn_printe();
		gen_incode("PLATY: Primary s_relational expression parsed\n");
		break;
	default:
		syn_printe();
		break;
	}



}

