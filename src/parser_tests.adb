with AUnit.Assertions; use AUnit.Assertions;
with Parser; use Parser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
use Parser.Token_Vector;
use Parser.Regex_AST;

package body Parser_Tests is
   function Name (T : Parser_Test) return Test_String is 
   begin
      return Format("Parser Tests");
   end Name;
   
   procedure Test_Parse_Char(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Character, To_Unbounded_String("a"));
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Character, To_Unbounded_String("a"))), "AST does not have correct token");
      null;
   end Test_Parse_Char;
   
   procedure Test_Parse_Newline(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Newline, To_Unbounded_String("\n"));
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Newline, To_Unbounded_String("\n"))), "AST does not have correct token");
      null;
   end Test_Parse_Newline;
   
   procedure Test_Parse_Pipe(T : in out Test_Case'Class) is
      v_tree : Tree := Empty_Tree;
      v_input : Vector := Empty_Vector &
        Make_Token( Parser.Pipe, To_Unbounded_String("|"));
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert( Count(v_tree) = 1, "AST does not have correct count" );
      Assert( Includes_Token(v_tree, Make_Token( Parser.Pipe, To_Unbounded_String("|"))), "AST does not have correct token");
      null;
   end Test_Parse_Pipe;
   
   procedure Register_Tests (T: in out Parser_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Parse_Char'Access, "Single char");
      Register_Routine(T, Test_Parse_Newline'Access, "Single newline");
      Register_Routine(T, Test_Parse_Pipe'Access, "Single pipe");


   end Register_Tests;

end Parser_Tests;
