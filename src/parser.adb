with Ada.Text_IO; use Ada.Text_IO;

package body parser is
   
   
   procedure Pass_Up_Parse_Results
     (Source_Position: Natural; Target_Position: out Natural;
      Source_Tree: Tree; Target_Tree: out Tree; Target_Cursor : out Regex_AST.Cursor) is
      
   begin
      Target_Position := Source_Position;
      Target_Tree := Copy(Source_Tree);
      Target_Cursor := Last_Child(Root(Target_Tree));
   end Pass_Up_Parse_Results;
   
   procedure One_Node_Tree(p_tree : out Tree; p_cursor: out Regex_AST.Cursor; p_token: Abstract_Syntax_Token ) is
      v_tree : Tree := Copy(Empty_Tree);
      v_cursor : Regex_AST.Cursor;
   begin
      v_cursor := Root(v_tree);
      Append_Child(v_tree, v_cursor, p_token);
      p_tree := Copy(v_tree);
      p_cursor := Last_Child(Root(p_tree));
   end One_Node_Tree;
   
   procedure Attach_Binary_Operator_Subtrees
     (p_tree: in out Tree; p_position: Regex_AST.Cursor; 
      p_left : Regex_AST.Cursor; p_right : Regex_Ast.Cursor) is
   begin
      if not (p_left = Regex_AST.No_Element) then 
         Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_left);
      end if;
      
      if not (p_right = Regex_AST.No_Element) then 
         Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_right);
      end if;
   end Attach_Binary_Operator_Subtrees;
   
   procedure Attach_Leftmost_Subtree
     (p_tree: in out Tree; p_position: Regex_AST.Cursor;
      p_subtree : Regex_AST.Cursor) is 
      
   begin 
      if not (p_subtree = Regex_AST.No_Element) then 
         if First_Child(p_position) = Regex_AST.No_Element then 
            Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_subtree);
         else 
            Copy_Subtree(p_tree, p_position, First_Child(p_position), p_subtree);
         end if;
      end if;
   end Attach_Leftmost_Subtree;
   
   procedure Attach_Rightmost_Subtree
     (p_tree: in out Tree; p_position: Regex_AST.Cursor;
      p_subtree : Regex_AST.Cursor) is 
      
   begin 
      if not (p_subtree = Regex_AST.No_Element) then 
         Copy_Subtree(p_tree, p_position, Regex_AST.No_Element, p_subtree);
      end if;
   end Attach_Rightmost_Subtree;
   
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
          f_lexeme => "" & p_lexeme
         );
      return v_token;
      
   end Make_Token;
   
   function EOF return Token is 
   begin 
      return ( f_class => EOF,
               f_lexeme => To_Unbounded_String("!")
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
         v_new_token := Make_Token(Character, v_element.f_lexeme);
           
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
   
   function Parse_Any_Char(p_input: Vector; p_position : in out Natural; 
                           p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is 
      v_success : Boolean := False;
      v_new_position : Natural := p_position;
      v_tree : Tree;
      v_cursor : Regex_AST.Cursor;
   begin
      
      v_success := Parse_Standard_Character(p_input, v_new_position, v_tree, v_cursor);
      if v_success then 
         Pass_Up_Parse_Results(v_new_position, p_position, v_tree, p_tree, p_cursor);
         return v_success;
      end if;
      
      v_new_position := p_position;
      v_success := Parse_Newline(p_input, v_new_position, v_tree, v_cursor);
      if v_success then 
         Pass_Up_Parse_Results(v_new_position, p_position, v_tree, p_tree, p_cursor);
         return v_success;
      end if;
      
      
      return False;
   end Parse_Any_Char;
   
   
   
   
   
   function Parse_EOF
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_element: Token;
      v_new_token : Abstract_Syntax_Token;
   begin
      v_element := Token_Vector.Element(p_input, p_position);
      if v_element.f_class = EOF then 
         v_new_token := Make_Token(Character, v_element.f_lexeme);
         One_Node_Tree(p_tree, p_cursor, v_new_token);
         p_position := p_position + 1;
         
         return True;
      else
         return False;
      end if;
   end Parse_EOF;
   
   function Parse_Char_Expr
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean is
      
      v_position : Natural;
      v_success : Boolean;
      v_tree : Tree;
      v_cursor: Regex_AST.Cursor;
   begin
      v_position := p_position;
      v_success := 
        Parse_Any_Char(p_input, v_position, v_tree, v_cursor);
      
      if v_success then 
         Pass_Up_Parse_Results(v_position, p_position, v_tree, p_tree, p_cursor);
      end if;
      
      return v_success;
      
   end Parse_Char_Expr;
   
   function Parse_Unary_Expr
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean is
      
      v_position : Natural;
      v_success : Boolean;
      v_char_tree : Tree;
      v_char_cursor: Regex_AST.Cursor;
   begin 
      v_position := p_position;
      v_success := 
        Parse_Char_Expr(p_input, v_position, v_char_tree, v_char_cursor);
      
      if v_success then 
         Pass_Up_Parse_Results(v_position, p_position, v_char_tree, p_tree, p_cursor);
      end if;
      
      return v_success;
      
   end Parse_Unary_Expr;
     
   function Parse_Concat_Expr
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean;
   
   function Parse_Concat_Opt
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean is
      
      v_position: Natural;
      v_success : Boolean;
      v_tree : Tree;
      v_cursor: Regex_AST.Cursor;
   begin
      v_position := p_position;
      v_success := 
        Parse_Concat_Expr(p_input, v_position, v_tree, v_cursor);
      
      if v_success then 
         Pass_Up_Parse_Results(v_position, p_position, v_tree, p_tree, p_cursor);
      else
         Pass_Up_Parse_Results(p_position, p_position, Empty_Tree, p_tree, p_cursor);
      end if;
      
      return True;
      
   end Parse_Concat_Opt;
   
   function Parse_Concat_Expr
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean is
      
      v_position: Natural;
      v_success: Boolean;
      v_unary_tree : Tree;
      v_unary_cursor: Regex_AST.Cursor;
      v_concat_tree : Tree;
      v_concat_cursor: Regex_AST.Cursor;
      v_operator_tree : Tree;
      v_operator_cursor: Regex_AST.Cursor;
   begin
      v_position := p_position;
      v_success := 
        Parse_Unary_Expr(p_input, v_position, v_unary_tree, v_unary_cursor) and then
        Parse_Concat_Opt(p_input, v_position, v_concat_tree, v_concat_cursor);
                         
      if v_success then 
         if not (v_concat_cursor = Regex_AST.No_Element) then 
            One_Node_Tree(v_operator_tree, v_operator_cursor, (
                      f_class => Concat,
                      f_lexeme => To_Unbounded_String("`")
                      ));
            Attach_Binary_Operator_Subtrees(v_operator_tree, v_operator_cursor, v_unary_cursor, v_concat_cursor);
            Pass_Up_Parse_Results(v_position, p_position, v_operator_tree, p_tree, p_cursor);
         else 
            Pass_Up_Parse_Results(v_position, p_position, v_unary_tree, p_tree, p_cursor);
         end if;
      end if;                
      
      return v_success;
      
   end Parse_Concat_Expr;
   
   
   function Parse_Expr(p_input : Vector; p_position: in out Natural; 
                       p_tree : out Tree; p_cursor: out Regex_AST.Cursor) return Boolean;

   function Parse_Expr_Opt
     (p_input : Vector; p_position: in out Natural; p_tree : out Tree; 
      p_cursor: out Regex_AST.Cursor) return Boolean is
      
      v_position: Natural;
      v_success: Boolean;
      v_operator_tree : Tree;
      v_operator_cursor : Regex_AST.Cursor;
      v_expr_tree : Tree;
      v_expr_cursor : Regex_AST.Cursor;
   begin
      v_position := p_position;
      v_success :=
        Parse_Union_Operator(p_input, v_position, v_operator_tree, v_operator_cursor) and then
        Parse_Expr(p_input, v_position, v_expr_tree, v_expr_cursor);
      
      if v_success then 
         Pass_Up_Parse_Results(v_position, p_position, v_expr_tree, p_tree, p_cursor);
      else
         Pass_Up_Parse_Results(p_position, p_position, Empty_Tree, p_tree, p_cursor);
      end if;
      
      return True;
   end Parse_Expr_Opt;
   
   function Parse_Expr(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor: out Regex_AST.Cursor) return Boolean is 
      v_position: Natural;
      v_success: Boolean;
      v_concat_tree : Tree;
      v_concat_cursor: Regex_AST.Cursor;
      v_expr_tree: Tree;
      v_expr_cursor: Regex_AST.Cursor;
      v_operator_tree : Tree;
      v_operator_cursor: Regex_AST.Cursor;
   begin 
      v_position := p_position;
      v_success := 
        Parse_Concat_Expr(p_input, v_position, v_concat_tree, v_concat_cursor) and then 
        Parse_Expr_Opt(p_input, v_position, v_expr_tree, v_expr_cursor);
      if v_success then 
         if not (v_expr_cursor = Regex_AST.No_Element) then 
            One_Node_Tree(v_operator_tree, v_operator_cursor, (
                      f_class => Union,
                      f_lexeme => To_Unbounded_String("|")
                      ));
            Attach_Binary_Operator_Subtrees(v_operator_tree, v_operator_cursor, v_concat_cursor, v_expr_cursor);
            Pass_Up_Parse_Results(v_position, p_position, v_operator_tree, p_tree, p_cursor);
         else 
            Pass_Up_Parse_Results(v_position, p_position, v_concat_tree, p_tree, p_cursor);
         end if;
         
      end if;
      
      return v_success;
   
   end Parse_Expr;
   
   function Parse_Program(p_input : Vector; p_position: in out Natural; p_tree : out Tree; p_cursor: out Regex_AST.Cursor) return Boolean is 
      v_tree : Tree;
      v_cursor: Regex_AST.Cursor;
      v_position: Natural;
      v_success: Boolean;
      v_eof_tree: Tree;
      v_eof_cursor: Regex_AST.Cursor;
   begin
                                     
      v_position := p_position;
      v_success := 
        Parse_Expr(p_input, v_position, v_tree, v_cursor) and then
        Parse_EOF(p_input, v_position, v_eof_tree, v_eof_cursor);
      if v_success then
         Pass_Up_Parse_Results(v_position, p_position, v_tree, p_tree, p_cursor);
      end if;
      
      return v_success;
      
   end Parse_Program;
   
   
   function Parse(p_input : Vector; p_tree : out Tree) return Boolean is 
      v_subtree : Tree := Empty_Tree;
      v_position : Natural := 0;
      v_cursor : Regex_AST.Cursor;
      v_success : Boolean;
      v_eof_tree : Tree;
      v_eof_cursor : Regex_AST.Cursor;
   begin
      
      v_success := Parse_Program(p_input, v_position, v_subtree, v_cursor);
        --Parse_Expression(p_input, v_position, v_subtree, v_cursor) and then
        --Parse_EOF(p_input, v_position, v_eof_tree, v_eof_cursor);
      if v_success then
         p_tree := Copy(v_subtree);
      end if;
      
      return v_success;
   end Parse;
   
   function Get_Tree ( p_tree : Tree ) return Unbounded_String is 
      input : Unbounded_String := To_Unbounded_String("");
      depth : Integer := -5;
      procedure Gather_Lexemes(Position : Regex_AST.Cursor) is 
         
      begin
         if Integer(Regex_AST.Depth(Position)) > depth then 
            depth := Integer(Regex_AST.Depth(Position));
            input := input & ",";
         end if;
         
         if Integer(Regex_AST.Depth(Position)) < depth then 
            input := input & "!";
         end if;
                             
         input := input & Element(Position).f_lexeme;
      end Gather_Lexemes;
   begin
      Iterate(p_tree, Gather_Lexemes'Access);
      return input;
   end Get_Tree;
   
end parser;
