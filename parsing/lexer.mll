{
  open Lexing
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
      List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
      tbl

  let keyword_table =
    create_hashtable 12 [
      ("if", IF);
      ("in", IN);
      ("then", THEN);
      ("else", ELSE);
      ("let", LET);
      ("begin", BEGIN);
      ("end", END);
      ("fun", FUNCTION);
      ("external", EXTERN);
      ("open", OPEN);
    ]

  (* To buffer string literals *)
  let initial_string_buffer = String.create 256
  let string_buff = ref initial_string_buffer
  let string_index = ref 0

  let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

  let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

  let get_stored_string () =
    let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s


}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9'] *
  let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
    let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
    let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
    let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
    let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']


rule token = parse
      [' ' '\t' '\n' '\r'] { token lexbuf }     (* skip blanks *)
  | int_literal as lxm   { INT (int_of_string lxm) }
  | float_literal as lxm { FLOAT (float_of_string lxm) }
  | "\""                 { reset_string_buffer();
                           let string_start = lexbuf.lex_start_p in
                               string lexbuf;
                               lexbuf.lex_start_p <- string_start;
                               STRING (get_stored_string())
                         }
  | "(*"                 { reset_string_buffer();
                           let string_start = lexbuf.lex_start_p in
                               string lexbuf;
                               lexbuf.lex_start_p <- string_start;
                               COMMENT (get_stored_string())
                         }
  | '='    { EQUAL }
  | '+'    { PLUS }
  | '-'    { MINUS }
  | '*'    { TIMES }
  | '/'    { DIV }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '{'    { LBRACE }
  | '}'    { RBRACE }
  | ':'    { COLON }
  | ';'    { SEMI }
  | '['    { LSQ_BRACKET }
  | ']'    { RSQ_BRACKET }
  | ";;"   { DOUBLE_SEMI }
  | ','    { COMMA }
  | '@'    { AT }
  | '>'    { GT }
  | '<'    { LT }
  | '!'    { BANG }
  | '|'    { BAR }
  | "|>"   { BAR_GT }
  | "<|"   { LT_BAR }
  | "=="   { EQ }
  | ">="   { GE }
  | "<="   { LE }
  | '\\'   { BACKSLASH }
  | '.'    { DOT }
  | ".."   { RANGE_OP }
  | "`"    { BACKTICK }
  | "->"   { ARROW }
  | ['=' '<' '>' '|' '&' '$' ':' ] symbolchar *
            { INFIXOP0 (Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1 (Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2 (Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }

  | id as word
        { try
            let token = Hashtbl.find keyword_table word in
              token
          with Not_found ->
            ID word
        }
  | eof            { raise Eof }
and string = parse
  | '\"'  { () }
  | "\n"  { failwith "unterminated string" }
  | eof   { raise Eof }
  | _     { store_string_char(Lexing.lexeme_char lexbuf 0);
            string lexbuf }
and comment = parse 
  | "*)"  { () }
  | eof   { raise Eof }
  | _     { store_string_char(Lexing.lexeme_char lexbuf 0);
            string lexbuf }

