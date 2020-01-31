
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Parse_Types; use Parse_Types; use Parse_Types.Token_Vector;
use Parse_Types.Regex_AST;

package Parse_Terminals is
   procedure One_Node_Tree(p_tree : out Tree; p_cursor: out Regex_AST.Cursor; 
                           p_token: Abstract_Syntax_Token );
   
   function Parse_Union_Operator(p_input : Vector; p_position: in out Natural; 
                                 p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Zero_Or_More_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_One_Or_More_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Optional_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Start_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_End_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Interval_Operator(p_input : Vector; p_position: in out Natural; 
                                    p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Left_Bracket(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Right_Bracket(p_input : Vector; p_position: in out Natural; 
                                p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Left_Paren(p_input : Vector; p_position: in out Natural; 
                             p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
    function Parse_Right_Paren(p_input : Vector; p_position: in out Natural; 
                               p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Complement_Operator
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Standard_Character
     (p_input : Vector; p_position: in out Natural; 
      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Newline(p_input : Vector; p_position: in out Natural; 
                          p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Tab(p_input : Vector; p_position: in out Natural; 
                      p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   function Parse_Return(p_input : Vector; p_position: in out Natural; 
                         p_tree : out Tree; p_cursor : out Regex_AST.Cursor) return Boolean;
   
   procedure Pass_Up_Parse_Results
     (Source_Position: Natural; Target_Position: out Natural;
      Source_Tree: Tree; Target_Tree: out Tree; Target_Cursor : out Regex_AST.Cursor);
   

end Parse_Terminals;
