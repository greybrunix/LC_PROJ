# WPC Grammar

flux        ::= SKIP

              | comp

              | choice

comp        ::= command COMP flux

choice      ::= SKIP CHOICE flux

              | comp CHOICE flux

              | SKIP CHOICE flux choices

              | comp CHOICE flux choices

choices     ::= CHOICE flux choices

              | CHOICE flux

command     ::= assume

              | force

              | attrib

              | havoc

assume      ::= ASSUME log_expr

force       ::= FORCE log_expr

attrib      ::= vector ATTRIB expr_vector

havoc       ::= HAVOC vector

vector      ::= LPAREN vector_ids

vector_ids  ::= ID vec_ids_m

              | RPAREN

vec_ids_m   ::= COMMA vector_ids

              | vector_ids

log_expr    ::= log_term op_ex log_expr

              | log_term

log_term    ::= log_factor op_term log_term

              | log_factor

log_factor  ::= LPAREN log_factor RPAREN

              | ID

              | INT

              | NOT expr

              | True

              | False

              | Forall vector DOT expr

              | Exists vector DOT expr

op_ex       ::= LT 

              | LE 
            
              | GT 
              
              | GE 
              
              | EQ 
              
              | NE

expr_vector ::= LPAREN exprs

exprs       ::= expr exprs_m

              | RPAREN

exprs_m     ::= COMMA exprs

              | exprs

expr        ::= term ADD expr 

              | term SUB expr

              | term

term        ::= factor MUL term 

              | factor DIV term 

              | factor

factor      ::= LPAREN expr RPAREN 

              | INT 

              | ID
