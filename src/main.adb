with Ada.Text_IO;
with Lexer_Suite;
with Parser_Suite;
with Code_Gen_NFA_Suite;
with Code_Gen_DFA_Suite;
with Regex_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Main is
   procedure Run_Lexer is new AUnit.Run.Test_Runner(Lexer_Suite.Suite);
   procedure Run_Parser is new AUnit.Run.Test_Runner(Parser_Suite.Suite);
   procedure Run_Code_Gen_NFA is new AUnit.Run.Test_Runner(Code_Gen_NFA_Suite.Suite);
   procedure Run_Code_Gen_DFA is new AUnit.Run.Test_Runner(Code_Gen_DFA_Suite.Suite);
   procedure Run_Regex is new AUnit.Run.Test_Runner(Regex_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_Lexer (Reporter);
   Run_Parser(Reporter);
   Run_Code_Gen_NFA(Reporter);
   Run_Code_Gen_DFA(Reporter);
   Run_Regex(Reporter);
   null;
end Main;
