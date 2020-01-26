package body parser is
   function Make_Token(p_class : Class; p_lexeme : Unbounded_String) return Token is
      v_token : Token;
   begin
      v_token := 
        ( f_class => p_class,
          f_lexeme => p_lexeme
         );
      return v_token;
      
   end Make_Token;
   
   function Make_Token(p_class : Abstract_Syntax_Class; p_lexeme : Unbounded_String) return Abstract_Syntax_Token is
      v_token : Abstract_Syntax_Token;
   begin
      v_token := 
        ( f_class => p_class,
          f_lexeme => p_lexeme
         );
      return v_token;
      
   end Make_Token;
   
   function Count(p_tree: Tree) return Natural is 
   begin
      return Natural (Node_Count(p_tree)) - 1;
   end Count;
   
   function Includes_Token(p_tree: Tree; p_token: Abstract_Syntax_Token) return Boolean is 
   begin
      return Find(p_tree, p_token) /= Regex_AST.No_Element;
   end Includes_Token;
   
   function Parse_Character(p_input : Vector; p_position: Natural; p_tree : in out Tree; p_cursor : in out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Character then 
         v_new_token := 
           ( f_class => Character,
             f_lexeme => v_element.f_lexeme
           );
         Append_Child(p_tree, p_cursor, v_new_token);
         
         return True;
      else
         return False;
      end if;
   end Parse_Character;
   
   function Parse_Newline(p_input : Vector; p_position: Natural; p_tree : in out Tree; p_cursor : in out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Newline then 
         v_new_token := 
           ( f_class => Newline,
             f_lexeme => v_element.f_lexeme
           );
         Append_Child(p_tree, p_cursor, v_new_token);
         
         return True;
      else
         return False;
      end if;
   end Parse_Newline;
   
   function Parse_Pipe(p_input : Vector; p_position: Natural; p_tree : in out Tree; p_cursor : in out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Pipe then 
         v_new_token := 
           ( f_class => Pipe,
             f_lexeme => v_element.f_lexeme
           );
         Append_Child(p_tree, p_cursor, v_new_token);
         
         return True;
      else
         return False;
      end if;
   end Parse_Pipe;
   
   function Parse(p_input : Vector; p_tree : out Tree) return Boolean is 
      v_tree : Tree := Empty_Tree;
      v_position : Natural := 0;
      v_cursor : Regex_AST.Cursor;
      v_success : Boolean;
   begin
      v_cursor := Root(v_tree);
      v_success := 
        Parse_Character(p_input, v_position, v_tree, v_cursor) or else
        Parse_Pipe(p_input, v_position, v_tree, v_cursor) or else
        Parse_Newline(p_input, v_position, v_tree, v_cursor);
      if v_success then
         p_tree := v_tree;
      end if;
      
      return v_success;
   end Parse;
   
   
   
   

end parser;
