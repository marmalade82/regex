with AUnit.Assertions; use AUnit.Assertions;
with Parser; use Parser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Parse_Types; use Parse_Types;
use Parse_Types.Token_Vector;
use Parse_Types.Regex_AST;
with Ada.Text_IO;


package body Parser_Tests is
   function Name (T : Parser_Test) return Test_String is 
   begin
      return Format("Parser Tests");
   end Name;
   
   procedure Test_Parse_Letter(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      null;
   end Test_Parse_Letter;
   
   procedure Test_Parse_Nested_Letter(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      null;
   end Test_Parse_Nested_Letter;
   
   procedure Test_Parse_Newline(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Newline, To_Unbounded_String("\n")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",\n", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Newline, To_Unbounded_String("\n"))), "AST does not have correct token");
      null;
   end Test_Parse_Newline;
   
   procedure Test_Parse_Tab(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Tab, To_Unbounded_String("\t")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",\t", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Tab, To_Unbounded_String("\t"))), "AST does not have correct token");
      null;
   end Test_Parse_Tab;
   
   procedure Test_Parse_Return(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Carriage_Return, To_Unbounded_String("\r")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",\r", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Carriage_Return, 
              To_Unbounded_String("\r"))), "AST does not have correct token");
      null;
   end Test_Parse_Return;
   
   procedure Test_Parse_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
   end Test_Parse_Union;
   
   procedure Test_Parse_Nested_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
   end Test_Parse_Nested_Union;
   
   procedure Test_Parse_Multiple_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Union;
   
   procedure Test_Parse_Multiple_Nested_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",|,|,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Nested_Union;
   
   procedure Test_Parse_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      null;
   end Test_Parse_Concat;
   
   procedure Test_Parse_Nested_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      null;
   end Test_Parse_Nested_Concat;
   
   procedure Test_Parse_Multiple_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        Make_Token( Parse_Types.Character, To_Unbounded_String("d")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,a`,b`,cd", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 7, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Concat;
   
   procedure Test_Parse_Multiple_Nested_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,`,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Nested_Concat;
   
   procedure Test_Parse_Mix_Union_Concat_1(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector & 
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",|,a`,bc", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
   end Test_Parse_Mix_Union_Concat_1;
   
   procedure Test_Parse_Mix_Union_Concat_2(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector & 
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) & 
        Make_Token( Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",|,`,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("c"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Union, To_Unbounded_String("|"))), "AST does not have correct token");
   end Test_Parse_Mix_Union_Concat_2;
   
   procedure Test_Parse_Range_One_Char(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
      
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_One_Char;
   
   procedure Test_Parse_Range_Multi_Char(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("\d")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("\e")) &
        Make_Token( Parse_Types.Newline, To_Unbounded_String("\n")) &
        Make_Token( Parse_Types.Tab, To_Unbounded_String("\r")) &
        Make_Token( Parse_Types.Carriage_Return, To_Unbounded_String("\t")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 9, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,abc\d\e\n\r\t", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Multi_Char;
   
   procedure Test_Parse_Range_One_Interval(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 4, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,-,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_One_Interval;
   
   procedure Test_Parse_Range_Multi_Interval(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("e")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",[,-,ab!-,bc!-,cd!-,de", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 13, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Range_Multi_Interval;
   
   procedure Test_Parse_Range_Mix_Char_Interval_1(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,c-,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_1;
   
   procedure Test_Parse_Range_Mix_Char_Interval_2(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,-,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_2;
   
   procedure Test_Parse_Range_Mix_Char_Interval_3(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("Z")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("3")) &
        Make_Token(Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("5")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",[,-,ab!Z-,35", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 8, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_3;
   
   procedure Test_Parse_Range_Mix_Char_Interval_4(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("A")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("Z")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",[,A-,ab!Z", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 6, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parse_Types.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_4;
   
   
   
   procedure Test_Parse_Range_Complement(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",^,-,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
   end Test_Parse_Range_Complement;
   
   procedure Test_Parse_Wildcard(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Asterisk, To_Unbounded_String("*")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",*,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Wildcard;
   
   procedure Test_Parse_Plus(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Plus, To_Unbounded_String("+")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",+,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Plus;
   
   procedure Test_Parse_Question(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Question, To_Unbounded_String("?")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",?,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Question;
   
   procedure Test_Parse_Unary_Mix(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Question, To_Unbounded_String("?")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token( Parse_Types.Plus, To_Unbounded_String("+")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token( Parse_Types.Asterisk, To_Unbounded_String("*")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("e")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",`,a`,?,b!`,+,c!`,*,d!e", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 12, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Unary_Mix;
   
   procedure Test_Parse_Nested_Unary(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token( Parse_Types.Asterisk, To_Unbounded_String("*")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",*,`,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 4, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Nested_Unary;
   
   procedure Test_Parse_Nested_Unary_2(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token( Parse_Types.Asterisk, To_Unbounded_String("+")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",+,[,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 4, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Nested_Unary_2;
   
   procedure Test_Parse_Nested_Unary_3(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token( Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token( Parse_Types.Asterisk, To_Unbounded_String("+")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",+,^,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 4, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Nested_Unary_3;
   
   procedure Test_Parse_Match_Start(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",^,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Match_Start;
   
   procedure Test_Parse_Match_End(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Dollar, To_Unbounded_String("$")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",$,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Match_End;
   
   procedure Test_Parse_Match_Start_End(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token( Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token( Parse_Types.Dollar, To_Unbounded_String("$")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",^,$,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Match_Start_End;
   
   procedure Register_Tests (T: in out Parser_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Parse_Letter'Access, "Single letter");
      Register_Routine(T, Test_Parse_Newline'Access, "Single newline");
      Register_Routine(T, Test_Parse_Tab'Access, "Single tab");
      Register_Routine(T, Test_Parse_Return'Access, "Single carriage return");
      Register_Routine(T, Test_Parse_Union'Access, "Single Union");
      Register_Routine(T, Test_Parse_Concat'Access, "Single concatenation");
      Register_Routine(T, Test_Parse_Multiple_Concat'Access, "Multiple concatenation");
      Register_Routine(T, Test_Parse_Multiple_Union'Access, "Multiple Union");
      Register_Routine(T, Test_Parse_Mix_Union_Concat_1'Access, "Union before concat");
      Register_Routine(T, Test_Parse_Mix_Union_Concat_2'Access, "Concat before union");
      Register_Routine(T, Test_Parse_Range_One_Char'Access, "Range one char");
      Register_Routine(T, Test_Parse_Range_Multi_Char'Access, "Range multiple characters, including escaped characters");
      Register_Routine(T, Test_Parse_Range_One_Interval'Access, "Range one interval");
      Register_Routine(T, Test_Parse_Range_Multi_Interval'Access, "Range multiple intervals");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_1'Access, "Char before interval");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_2'Access, "Interval before char");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_3'Access, "Char between two intervals");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_4'Access, "Interval between two chars");
      Register_Routine(T, Test_Parse_Range_Complement'Access, "Range complement");
      Register_Routine(T, Test_Parse_Wildcard'Access, "Single wildcard");
      Register_Routine(T, Test_Parse_Plus'Access, "Single plus");
      Register_Routine(T, Test_Parse_Question'Access, "Single question");
      Register_Routine(T, Test_Parse_Unary_Mix'Access, "Mix of unary expressions");
      Register_Routine(T, Test_Parse_Nested_Letter'Access, "Single nested_letter");
      Register_Routine(T, Test_Parse_Nested_Union'Access, "Single nested Union");
      Register_Routine(T, Test_Parse_Nested_Concat'Access, "Single nested concatenation");
      Register_Routine(T, Test_Parse_Multiple_Nested_Union'Access, "Multiple nested Union");
      Register_Routine(T, Test_Parse_Multiple_Nested_Concat'Access, "Multiple nested concatenation");
      Register_Routine(T, Test_Parse_Match_Start'Access, "Start of input");
      Register_Routine(T, Test_Parse_Match_End'Access, "End of input");
      Register_Routine(T, Test_Parse_Match_Start_End'Access, "Start and End of input");
      Register_Routine(T, Test_Parse_Nested_Unary'Access, "Unary operator on nested expression");
      Register_Routine(T, Test_Parse_Nested_Unary_2'Access, "Unary operator on range");	
      Register_Routine(T, Test_Parse_Nested_Unary_3'Access, "Unary operator on range complement");      


   end Register_Tests;

end Parser_Tests;
