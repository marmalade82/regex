with AUnit.Assertions; use AUnit.Assertions;
with Parser; use Parser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
use Parser.Token_Vector;
use Parser.Regex_AST;
with Ada.Text_IO;


package body Parser_Tests is
   function Name (T : Parser_Test) return Test_String is 
   begin
      return Format("Parser Tests");
   end Name;
   
   procedure Test_Parse_Letter(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      null;
   end Test_Parse_Letter;
   
   procedure Test_Parse_Newline(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Newline, To_Unbounded_String("\n")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",\n", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Newline, To_Unbounded_String("\n"))), "AST does not have correct token");
      null;
   end Test_Parse_Newline;
   
   procedure Test_Parse_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
   end Test_Parse_Union;
   
   procedure Test_Parse_Multiple_Union(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        Make_Token( Parser.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Union, To_Unbounded_String("|"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Union;
   
   procedure Test_Parse_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 3, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      null;
   end Test_Parse_Concat;
   
   procedure Test_Parse_Multiple_Concat(T: in out Test_Case'Class) is 
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        Make_Token( Parser.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",`,a`,bc", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("c"))), "AST does not have correct token");
   end Test_Parse_Multiple_Concat;
   
   procedure Test_Parse_Mix_Union_Concat_1(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector & 
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        Make_Token( Parser.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   
   begin
      v_success := Parse(v_input, v_tree);
      Assert(Get_Tree(v_tree) = ",|,a`,bc", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("c"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Union, To_Unbounded_String("|"))), "AST does not have correct token");
   end Test_Parse_Mix_Union_Concat_1;
   
   procedure Test_Parse_Mix_Union_Concat_2(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector & 
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) & 
        Make_Token( Parser.Pipe, To_Unbounded_String("|")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) & 
        EOF;
      v_success : Boolean;
   
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",|,`,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("b"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("c"))), "AST does not have correct token");
      Assert( Includes_Token(v_tree, Make_Token( Parser.Union, To_Unbounded_String("|"))), "AST does not have correct token");
   end Test_Parse_Mix_Union_Concat_2;
   
   procedure Test_Parse_Range_One_Char(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
      
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 2, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,a", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_One_Char;
   
   procedure Test_Parse_Range_Multi_Char(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Character, To_Unbounded_String("d")) &
        Make_Token( Parser.Character, To_Unbounded_String("e")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 6, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,abcde", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Multi_Char;
   
   procedure Test_Parse_Range_One_Interval(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 4, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,-,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_One_Interval;
   
   procedure Test_Parse_Range_Multi_Interval(T: in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("d")) &
        Make_Token( Parser.Character, To_Unbounded_String("d")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("e")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
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
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,c-,ab", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_1;
   
   procedure Test_Parse_Range_Mix_Char_Interval_2(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 5, "AST does not have correct count: " & Count(v_tree)'Image );
      Assert(Get_Tree(v_tree) = ",[,-,ab!c", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
   end Test_Parse_Range_Mix_Char_Interval_2;
   
   procedure Test_Parse_Range_Complement(T : in out Test_Case'Class) is 
      v_tree : Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token( Parser.Caret, To_Unbounded_String("^")) &
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Hyphen, To_Unbounded_String("-")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Right_Bracket, To_Unbounded_String("]")) &
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
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Asterisk, To_Unbounded_String("*")) &
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
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Plus, To_Unbounded_String("+")) &
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
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Question, To_Unbounded_String("?")) &
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
        Make_Token( Parser.Character, To_Unbounded_String("a")) &
        Make_Token( Parser.Character, To_Unbounded_String("b")) &
        Make_Token( Parser.Question, To_Unbounded_String("?")) &
        Make_Token( Parser.Character, To_Unbounded_String("c")) &
        Make_Token( Parser.Plus, To_Unbounded_String("+")) &
        Make_Token( Parser.Character, To_Unbounded_String("d")) &
        Make_Token( Parser.Asterisk, To_Unbounded_String("*")) &
        Make_Token( Parser.Character, To_Unbounded_String("e")) &
        EOF;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse did not succeed");
      Assert(Get_Tree(v_tree) = ",`,a`,?,b!`,+,c!`,*,d!e", "AST is actually: " & To_String(Get_Tree(v_tree)));
      Assert( Count(v_tree) = 12, "AST does not have correct count: " & Count(v_tree)'Image );
   end Test_Parse_Unary_Mix;
   
   procedure Register_Tests (T: in out Parser_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Parse_Letter'Access, "Single letter");
      Register_Routine(T, Test_Parse_Newline'Access, "Single newline");
      Register_Routine(T, Test_Parse_Union'Access, "Single Union");
      Register_Routine(T, Test_Parse_Concat'Access, "Single concatenation");
      Register_Routine(T, Test_Parse_Multiple_Concat'Access, "Multiple concatenation");
      Register_Routine(T, Test_Parse_Multiple_Union'Access, "Multiple Union");
      Register_Routine(T, Test_Parse_Mix_Union_Concat_1'Access, "Union before concat");
      Register_Routine(T, Test_Parse_Mix_Union_Concat_2'Access, "Concat before union");
      Register_Routine(T, Test_Parse_Range_One_Char'Access, "Range one char");
      Register_Routine(T, Test_Parse_Range_Multi_Char'Access, "Range multiple characters");
      Register_Routine(T, Test_Parse_Range_One_Interval'Access, "Range one interval");
      Register_Routine(T, Test_Parse_Range_Multi_Interval'Access, "Range multiple intervals");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_1'Access, "Char before interval");
      Register_Routine(T, Test_Parse_Range_Mix_Char_Interval_2'Access, "Interval before char");
      Register_Routine(T, Test_Parse_Range_Complement'Access, "Range complement");
      Register_Routine(T, Test_Parse_Wildcard'Access, "Single wildcard");
      Register_Routine(T, Test_Parse_Plus'Access, "Single plus");
      Register_Routine(T, Test_Parse_Question'Access, "Single question");
      Register_Routine(T, Test_Parse_Unary_Mix'Access, "Mix of unary expressions");



   end Register_Tests;

end Parser_Tests;
