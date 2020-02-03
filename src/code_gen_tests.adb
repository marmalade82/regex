with AUnit.Assertions; use AUnit.Assertions;
with Code_Gen; use Code_Gen;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Parser; use Parser;
with Parse_Types; use Parse_Types; use Parse_Types.Token_Vector; use Parse_Types.Regex_AST;


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Code_Gen_Tests is
   function Name (T : Code_Gen_Test) return Test_String is 
   begin
      return Format("Code Gen Tests");
   end Name;
   
   procedure Test_Parse_Char(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 3, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
   end Test_Parse_Char;
   
   procedure Register_Tests(T: in out Code_Gen_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Parse_Char'Access, "Test that the NFA processes a single char");
   end Register_Tests;

end Code_Gen_Tests;
