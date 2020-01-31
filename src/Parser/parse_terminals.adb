package body Parse_Terminals is
   
   procedure Pass_Up_Parse_Results
     (Source_Position: Natural; Target_Position: out Natural;
      Source_Tree: Tree; Target_Tree: out Tree; Target_Cursor : out Regex_AST.Cursor) is
      
   begin
      Target_Position := Source_Position;
      Target_Tree := Copy(Source_Tree);
      Target_Cursor := Last_Child(Root(Target_Tree));
   end Pass_Up_Parse_Results;
   
   procedure One_Node_Tree(p_tree : out Tree; p_cursor: out Regex_AST.Cursor; 
                           p_token: Abstract_Syntax_Token ) is
      v_tree : Tree := Copy(Empty_Tree);
      v_cursor : Regex_AST.Cursor;
   begin
      v_cursor := Root(v_tree);
      Append_Child(v_tree, v_cursor, p_token);
      p_tree := Copy(v_tree);
      p_cursor := Last_Child(Root(p_tree));
   end One_Node_Tree;
   
   function Match_Token_And_Build_Tree(Match : Class; P_Token: Token;
                                       New_Class : Abstract_Syntax_Class; P_Tree : out Tree; 
                                       P_Cursor : out Regex_Ast.Cursor; P_Position : in out Natural) return Boolean is
      v_new_token : Abstract_Syntax_Token;
   begin
      if Match = P_Token.f_class then 
         v_new_token := Make_Token(New_Class, P_Token.f_lexeme);
         One_Node_Tree(P_Tree, p_cursor, v_new_token);
         p_position := p_position + 1;    
         
         return True;
      else 
         return False;
      end if;
      
   end Match_Token_And_Build_Tree;
   
   function Match_Token(Match : Class; P_Token: Token;
                        P_Tree : out Tree; P_Cursor : out Regex_Ast.Cursor; 
                        P_Position : in out Natural) return Boolean is
      
   begin 
      if Match = P_Token.f_class then 
         Pass_Up_Parse_Results(P_Position + 1, P_Position, Empty_Tree, P_Tree, P_Cursor);        
         
         return True;
      else
         return False;
      end if;
   end Match_Token;
   
   function Parse_Union_Operator(p_input : Vector; p_position: in out Natural; 
                                 p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin
      v_success := Match_Token_And_Build_Tree(Pipe, Token_Vector.Element(p_input, p_position), 
                                              Union, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Union_Operator;
   
   function Parse_Zero_Or_More_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Asterisk, Token_Vector.Element(p_input, p_position), 
                                              Zero_Or_More, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Zero_Or_More_Operator;
   
   function Parse_One_Or_More_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Plus, Token_Vector.Element(p_input, p_position), 
                                              One_Or_More, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_One_Or_More_Operator;
   
   function Parse_Optional_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Question, Token_Vector.Element(p_input, p_position), 
                                              Optional, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Optional_Operator;
   
   function Parse_Start_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Caret, Token_Vector.Element(p_input, p_position), 
                                              Match_Start, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Start_Operator;
   
   function Parse_End_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Dollar, Token_Vector.Element(p_input, p_position), 
                                              Match_End, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_End_Operator;
   
   function Parse_Interval_Operator(p_input : Vector; p_position: in out Natural; 
                                 p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Hyphen, Token_Vector.Element(p_input, p_position), 
                                              Range_Interval, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Interval_Operator;
   
   function Parse_Left_Bracket(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin
      v_success := Match_Token
        (Left_Bracket, Token_Vector.Element(p_input, p_position), 
         p_tree, p_cursor, p_position
        );
      
      return v_success;
      
   end Parse_Left_Bracket;
   
   function Parse_Right_Bracket(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin
      v_success := Match_Token
        (Right_Bracket, Token_Vector.Element(p_input, p_position), 
         p_tree, p_cursor, p_position
        );
      
      return v_success;
      
   end Parse_Right_Bracket;
   
   function Parse_Left_Paren(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin
      v_success := Match_Token
        (Left_Paren, Token_Vector.Element(p_input, p_position), 
         p_tree, p_cursor, p_position
        );
      
      return v_success;
      
   end Parse_Left_Paren;
   
    function Parse_Right_Paren(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin
      v_success := Match_Token
        (Right_Paren, Token_Vector.Element(p_input, p_position), 
         p_tree, p_cursor, p_position
        );
      
      return v_success;
      
   end Parse_Right_Paren;
   
   function Parse_Complement_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin
      v_success := Match_Token
        (Caret, Token_Vector.Element(p_input, p_position), 
         p_tree, p_cursor, p_position
        );
      
      return v_success;
      
   end Parse_Complement_Operator;
   
   function Parse_Standard_Character
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Parse_Types.Character, Token_Vector.Element(p_input, p_position), 
                                              Parse_Types.Character, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Standard_Character;
   
   function Parse_Newline(p_input : Vector; p_position: in out Natural; 
                          p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is
      v_success : Boolean;
   begin 
      
      v_success := Match_Token_And_Build_Tree(Newline, Token_Vector.Element(p_input, p_position), 
                                              Newline, p_tree, p_cursor, p_position);
      return v_success;
      
   end Parse_Newline;
   
   function Parse_Tab(p_input : Vector; p_position: in out Natural; 
                      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is 
      v_success : Boolean;
   begin
      v_success := Match_Token_And_Build_Tree(Tab, Token_Vector.Element(p_input, p_position),
                                              Tab, p_tree, p_cursor, p_position);
      return v_success;
   end Parse_Tab;
   
   function Parse_Return(p_input : Vector; p_position: in out Natural; 
                         p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean is 
      v_success : Boolean;
   begin
      v_success := Match_Token_And_Build_Tree(Carriage_Return, Token_Vector.Element(p_input, p_position),
                                              Carriage_Return, p_tree, p_cursor, p_position);
      return v_success;
   end Parse_Return;
   

end Parse_Terminals;
