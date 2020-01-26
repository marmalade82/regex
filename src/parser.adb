package body parser is
   type Parse_Result is record
      f_success : Boolean;
      f_tree : Regex_AST.Tree;
      f_cursor : Regex_AST.Cursor;
   end record;
   
   procedure One_Node_Tree(p_tree : out Tree; p_cursor: out Regex_AST.Cursor; p_token: Abstract_Syntax_Token ) is
   begin
      p_tree := Empty_Tree;
      p_cursor := Root(p_tree);
      Append_Child(p_tree, p_cursor, p_token);
      p_cursor := Last_Child(p_cursor);
   end One_Node_Tree;
   
   procedure Attach_Binary_Operator_Subtrees
     (p_tree: in out Tree; p_position: Regex_AST.Cursor; 
      p_left : Regex_AST.Cursor; p_right : Regex_Ast.Cursor) is
   begin
      Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_left);
      Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_right);
   end Attach_Binary_Operator_Subtrees;
   
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
   
   function EOF return Token is 
   begin 
      return ( f_class => EOF,
               f_lexeme => To_Unbounded_String("")
              );
   end;
   
   
   function Count(p_tree: Tree) return Natural is 
   begin
      return Natural (Node_Count(p_tree)) - 1;
   end Count;
   
   function Includes_Token(p_tree: Tree; p_token: Abstract_Syntax_Token) return Boolean is 
   begin
      return Find(p_tree, p_token) /= Regex_AST.No_Element;
   end Includes_Token;
   
   function Parse_Union_Operator(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Pipe then 
         v_new_token := 
           ( f_class => Union,
             f_lexeme => v_element.f_lexeme
            );
         One_Node_Tree(p_tree, p_cursor, v_new_token);
         p_position := p_position + 1;
         
         
         return True;
      else
         return False;
      end if;
   end Parse_Union_Operator;
   
   function Parse_Standard_Character(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Character then 
         v_new_token := 
           ( f_class => Character,
             f_lexeme => v_element.f_lexeme
            );
         One_Node_Tree(p_tree, p_cursor, v_new_token);
         p_position := p_position + 1;
         
         return True;
      else
         return False;
      end if;
   end Parse_Standard_Character;
   
   function Parse_Newline(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = Newline then 
         v_new_token := 
           ( f_class => Newline,
             f_lexeme => v_element.f_lexeme
            );
         One_Node_Tree(p_tree, p_cursor, v_new_token);
         p_position := p_position + 1;
         
         return True;
      else
         return False;
      end if;
   end Parse_Newline;
   
   function Parse_Union(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean := False;
      v_left_operand : Tree;
      v_left_cursor : Regex_AST.Cursor;
      v_right_operand : Tree;
      v_right_cursor : Regex_AST.Cursor;
      v_operator : Tree;
      v_operator_cursor : Regex_AST.Cursor;
      p_new_position : Natural := p_position;
   begin
      
      v_success := 
        Parse_Standard_Character(p_input, p_new_position, v_left_operand, v_left_cursor) and then
        Parse_Union_Operator(p_input, p_new_position, v_operator, v_operator_cursor) and then
        Parse_Standard_Character(p_input, p_new_position, v_right_operand, v_right_cursor);
      
      if v_success then 
         Attach_Binary_Operator_Subtrees(v_operator, v_operator_cursor, v_left_cursor, v_right_cursor);
         p_tree := v_operator;
         p_cursor := v_operator_cursor;
         p_position := p_new_position;
         
         return v_success;
      end if;
      
      
      return False;
   end Parse_Union;
   
   function Parse_Concat
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean := False;
      v_left_operand : Tree;
      v_left_cursor : Regex_AST.Cursor;
      v_right_operand : Tree;
      v_right_cursor : Regex_AST.Cursor;
      v_operator : Tree; 
      v_operator_cursor : Regex_AST.Cursor;
      p_new_position : Natural := p_position;
   begin 
      v_success := 
        Parse_Standard_Character(p_input, p_new_position, v_left_operand, v_left_cursor) and then
        Parse_Standard_Character(p_input, p_new_position, v_right_operand, v_right_cursor);
      
      if v_success then 
         One_Node_Tree(v_operator, v_operator_cursor, (
                      f_class => Concat,
                      f_lexeme => To_Unbounded_String("")
                      ));
         Attach_Binary_Operator_Subtrees(v_operator, v_operator_cursor, v_left_cursor, v_right_cursor);
         p_tree := v_operator;
         p_cursor := v_operator_cursor;
         p_position := p_new_position;
         
         return v_success;
      end if;
      
      
      return False;
   end Parse_Concat;
   
   function Parse_Expression
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean := False;
      v_left : Tree;
      v_left_cursor : Regex_AST.Cursor;
      v_right : Tree;
      v_right_cursor : Regex_AST.Cursor;
      v_main : Tree; 
      v_main_cursor : Regex_AST.Cursor;
      v_new_position : Natural := p_position;
   begin
      v_success := 
        Parse_Concat(p_input, v_new_position, v_left, v_left_cursor);
        
        
      return False;
      
   end Parse_Expression;
   
   function Parse(p_input : Vector; p_tree : out Tree) return Boolean is 
      v_subtree : Tree := Empty_Tree;
      v_position : Natural := 0;
      v_cursor : Regex_AST.Cursor;
      v_success : Boolean;
   begin
      v_success := 
        Parse_Concat(p_input, v_position, v_subtree, v_cursor) or else
        Parse_Union(p_input, v_position, v_subtree, v_cursor) or else
        Parse_Standard_Character(p_input, v_position, v_subtree, v_cursor) or else
        Parse_Newline(p_input, v_position, v_subtree, v_cursor);
      if v_success then
         p_tree := v_subtree;
      end if;
      
      return v_success;
   end Parse;
   

end parser;
