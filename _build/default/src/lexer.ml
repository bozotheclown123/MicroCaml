open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = 
  let rec tok index s =
    if index >= String.length s then
      []
      else
     (* This is placed as the first if statement so it doesn't conflict with the subtraction case
        Negative integers must be surrounded by parentheses (without extra whitespace) to 
        differentiate from subtraction (examples below). 
        You may find the functions int_of_string and String.sub useful in lexing this token type. *)
      if (Str.string_match (Str.regexp "(-[0-9]+)") s index) then
        let token = Str.matched_string s in
          let substring = String.sub token 1 (String.length token - 2) in
          (Tok_Int (int_of_string substring))::(tok (index + String.length token) s)
  
      else if (Str.string_match (Str.regexp ")") s index) then
        Tok_RParen::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "(") s index) then
        Tok_LParen::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "=") s index) then
        Tok_Equal::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "<>") s index) then
        Tok_NotEqual::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp ">") s index) then
        Tok_Greater::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "<") s index) then
        Tok_Less::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp ">=") s index) then
        Tok_GreaterEqual::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "<=") s index) then
        Tok_LessEqual::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "->") s index) then
        Tok_Arrow::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "||") s index) then
        Tok_Or::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "&&") s index) then
        Tok_And::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "not") s index) then
        Tok_Not::(tok (index + 3) s)
      else if (Str.string_match (Str.regexp "if") s index) then
        Tok_If::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "then") s index) then
        Tok_Then::(tok (index + 4) s)
      else if (Str.string_match (Str.regexp "else") s index) then
        Tok_Else::(tok (index + 4) s)
      else if (Str.string_match (Str.regexp "\\+") s index) then
        Tok_Add::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "-") s index) then
        Tok_Sub::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "\\*") s index) then
        Tok_Mult::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "/") s index) then
        Tok_Div::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "\\^") s index) then
        Tok_Concat::(tok (index + 1) s)
      else if (Str.string_match (Str.regexp "let") s index) then
        Tok_Let::(tok (index + 3) s)
      else if (Str.string_match (Str.regexp "rec") s index) then
        Tok_Rec::(tok (index + 3) s)
      else if (Str.string_match (Str.regexp "in") s index) then
        Tok_In::(tok (index + 2) s)
      else if (Str.string_match (Str.regexp "def") s index) then
        Tok_Def::(tok (index + 3) s)
      else if (Str.string_match (Str.regexp "fun") s index) then
        Tok_Fun::(tok (index + 3) s)
      
        (* positive integer *)
      else if (Str.string_match (Str.regexp "[0-9]+") s index) then
        let token = Str.matched_string s in
        (Tok_Int (int_of_string token))::(tok (index + String.length token) s)   
      
        (* boolean *)
      else if (Str.string_match (Str.regexp "true\\|false" ) s index) then
        let token = Str.matched_string s in
            (Tok_Bool (bool_of_string token))::(tok (index + String.length token) s)
      
        (* string of string  *)
      else if (Str.string_match (Str.regexp "\"[^\"]*\"" ) s index) then
        let token = Str.matched_string s in
          let substring = String.sub token 1 (String.length token - 2) in
          (Tok_String substring)::(tok (index + String.length token) s)

        (* IDs (which are strings) *)
      else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*" ) s index) then
        let token = Str.matched_string s in
        (Tok_ID token):: (tok (index + (String.length token)) s)
  
      else if (Str.string_match (Str.regexp ";;" ) s index) then
        Tok_DoubleSemi::(tok (index + 2) s)
        
        (* whitespace (Spaces, tabs ('\t') and newlines ('\n') , carriage return ('\r')) *)
      else if (Str.string_match (Str.regexp "[ \t\r\n]+" ) s index) then
        let token = Str.matched_string s in
        (tok (index + (String.length token)) s)
  
      else
        raise (InvalidInputException "no match for the token!")
      in
      tok 0 input
  
  