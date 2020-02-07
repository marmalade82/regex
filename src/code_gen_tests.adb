with AUnit.Assertions; use AUnit.Assertions;
with Code_Gen; use Code_Gen;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Parser; use Parser;
with Parse_Types; use Parse_Types; use Parse_Types.Token_Vector; use Parse_Types.Regex_AST;
with Ada.Text_IO; use Ada.Text_IO;


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Code_Gen_Tests is
   function Name (T : Code_Gen_Test) return Test_String is 
   begin
      return Format("Code Gen Tests");
   end Name;
   
   procedure Test_Gen_Char(T : in out Test_Case'Class) is 
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
      Assert(Count_State(v_machine) = 2, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
   end Test_Gen_Char;
   
   procedure Test_Gen_Simple_Wildcard(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Asterisk, To_Unbounded_String("*")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 4, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aa")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aaa")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
   end Test_Gen_Simple_Wildcard;
   
   procedure Test_Gen_Concat_Wildcard_Precedence(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Asterisk, To_Unbounded_String("*")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 6, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aa")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aaa")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
   end Test_Gen_Concat_Wildcard_Precedence;
   
   procedure Test_Gen_Concat_Wildcard(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token(Parse_Types.Asterisk, To_Unbounded_String("*")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 6, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aa")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aaaa")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("a")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("aaa")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
   end Test_Gen_Concat_Wildcard;
   
   procedure Test_Gen_Simple_Plus(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Plus, To_Unbounded_String("+")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aa")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aaa")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
      -- 2 states from initial char nfa, and then 4 from the following wildcard nfa
      Assert(Count_State(v_machine) = 6, "Generates incorrect number of states: " & Count_State(v_machine)'Image);	
   end Test_Gen_Simple_Plus;
   
   procedure Test_Gen_Union_Plus_Precedence(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Plus, To_Unbounded_String("+")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 9, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("bb")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("aa")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("")), "Fails to reject unintended string");
   end Test_Gen_Union_Plus_Precedence;
   
   procedure Test_Gen_Union_Plus(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Paren, To_Unbounded_String("(")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Right_Paren, To_Unbounded_String(")")) &
        Make_Token(Parse_Types.Plus, To_Unbounded_String("+")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 12, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("aa")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("bb")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("c")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("")), "Fails to reject unintended string");
   end Test_Gen_Union_Plus;
   
   procedure Test_Gen_Concat(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin 
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 4, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("ab")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("ba")), "Fails to reject unintended string");
   end Test_Gen_Concat;
   
   procedure Test_Gen_Multi_Concat(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin 
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 8, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Count_Epsilon_Transitions(v_machine) = 3, "Generates incorrect number of epsilon transitions");
      Assert(Recognize(v_machine, To_Unbounded_String("abcd")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("abcc")), "Fails to reject unintended string");
   end Test_Gen_Multi_Concat;
   
   procedure Test_Gen_Union(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin 
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 5, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("c")), "Fails to reject unintended string");
   end Test_Gen_Union;
   
   procedure Test_Gen_Multi_Union(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin 
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      -- 8 from the four character NFAs, and 3 from the three unions' new starts
      Assert(Count_State(v_machine) = 11, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("c")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("d")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("e")), "Fails to reject unintended string");
   end Test_Gen_Multi_Union;
   
   procedure Test_Gen_Mix_Union_Concat(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      -- 6 from the character NFAs, and one from the union
      Assert(Count_State(v_machine) = 7, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("bc")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("ac")), "Fails to reject unintended string");
   end Test_Gen_Mix_Union_Concat;
   
    procedure Test_Gen_Mix_Concat_Union(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      -- 6 from the character NFAs, and one from the union
      Assert(Count_State(v_machine) = 7, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("ab")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("c")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("ac")), "Fails to reject unintended string");
   end Test_Gen_Mix_Concat_Union;
   
   procedure Test_Gen_Multi_Concat_Union(T: in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("e")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      -- 10 from the character NFAs, and 2 from the unions
      Assert(Count_State(v_machine) = 12, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("ab")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("c")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("de")), "Does not recognize the intended string");
   end Test_Gen_Multi_Concat_Union;
   
   procedure Test_Gen_Simple_Range(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 2, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
   end Test_Gen_Simple_Range;
   
   procedure Test_Gen_Multi_Char_Range(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 2, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("c")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("d")), "Fails to reject unintended string");
   end Test_Gen_Multi_Char_Range;
   
   procedure Test_Gen_Simple_Complement(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 2, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("a")), "Fails to reject unintended string");
   end Test_Gen_Simple_Complement;
   
   procedure Test_Gen_Multi_Char_Complement(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 2, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("d")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("a")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("b")), "Fails to reject unintended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("c")), "Fails to reject unintended string");

   end Test_Gen_Multi_Char_Complement;
   
   procedure Test_Gen_Range_Concats(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 4, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("ac")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("ad")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("bc")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("bd")), "Does not recognize the intended string");
   end Test_Gen_Range_Concats;
   
   procedure Test_Gen_Range_Unions(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token(Parse_Types.Pipe, To_Unbounded_String("|")) &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("e")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("f")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 8, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("a")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("b")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("c")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("d")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("e")), "Does not recognize the intended string");
      Assert(Recognize(v_machine, To_Unbounded_String("f")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("g")), "Fails to reject unintended string");
   end Test_Gen_Range_Unions;
   
    procedure Test_Gen_Complement_Concats(T : in out Test_Case'Class) is 
      v_machine: NFA;
      v_input: Vector := Empty_Vector &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("a")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("b")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        Make_Token(Parse_Types.Left_Bracket, To_Unbounded_String("[")) &
        Make_Token(Parse_Types.Caret, To_Unbounded_String("^")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("c")) &
        Make_Token(Parse_Types.Character, To_Unbounded_String("d")) &
        Make_Token(Parse_Types.Right_Bracket, To_Unbounded_String("]")) &
        EOF;
      v_tree: Tree;
      v_success : Boolean;
   begin
      v_success := Parse(v_input, v_tree);
      Assert(v_success, "Parse failed");
      v_machine := Gen_NFA(v_tree);
      Assert(Count_State(v_machine) = 4, "Generates incorrect number of states: " & Count_State(v_machine)'Image);
      Assert(Recognize(v_machine, To_Unbounded_String("cb")), "Does not recognize the intended string");
      Assert(not Recognize(v_machine, To_Unbounded_String("ad")), "Does not reject the unintended string");
   end Test_Gen_Complement_Concats;
   
   procedure Register_Tests(T: in out Code_Gen_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Gen_Char'Access, "Processes a single char");
      Register_Routine(T, Test_Gen_Concat'Access, "Processes a single concatenation");
      Register_Routine(T, Test_Gen_Multi_Concat'Access, "Processes multiple concatenation");
      Register_Routine(T, Test_Gen_Union'Access, "Processes a single union");
      Register_Routine(T, Test_Gen_Multi_Union'Access, "Processes multiple union");
      Register_Routine(T, Test_Gen_Mix_Union_Concat'Access, "Processes union then concat");
      Register_Routine(T, Test_Gen_Mix_Concat_Union'Access, "Processes concat then union");
      Register_Routine(T, Test_Gen_Multi_Concat_Union'Access, "Processes multiple mixed concats and unions");
      Register_Routine(T, Test_Gen_Simple_Range'Access, "Processes a simple range of one character");
      Register_Routine(T, Test_Gen_Multi_Char_Range'Access, "Processes a range of multiple single characters");
      Register_Routine(T, Test_Gen_Simple_Complement'Access, "Processes a simple range complement of one character");
      Register_Routine(T, Test_Gen_Multi_Char_Complement'Access, "Processes a range complement of multiple single characters");
      Register_Routine(T, Test_Gen_Range_Concats'Access, "Processes a concatenation of multiple ranges");
      Register_Routine(T, Test_Gen_Range_Unions'Access, "Processes a union of multiple ranges");
      Register_Routine(T, Test_Gen_Complement_Concats'Access, "Processes a concatenation of multiple range complements");
      Register_Routine(T, Test_Gen_Simple_Wildcard'Access, "Processes a wildcard of one character");
      Register_Routine(T, Test_Gen_Concat_Wildcard_Precedence'Access, "Confirms wildcard has higher precedence");
      Register_Routine(T, Test_Gen_Concat_Wildcard'Access, "Processes a wildcard of a concatenated fragment");
      Register_Routine(T, Test_Gen_Simple_Plus'Access, "Processes a plus of one character");
      Register_Routine(T, Test_Gen_Union_Plus_Precedence'Access, "Confirms Plus has higher precedence");
      Register_Routine(T, Test_Gen_Union_Plus'Access, "Processes a Plus of a Union Fragment");
      
      
   end Register_Tests;

end Code_Gen_Tests;
