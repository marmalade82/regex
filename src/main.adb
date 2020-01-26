with Ada.Text_IO;
with Lexer_Suite;
with Parser_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Main is
   procedure Run_Lexer is new AUnit.Run.Test_Runner(Lexer_Suite.Suite);
   procedure Run_Parser is new AUnit.Run.Test_Runner(Parser_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run_Lexer (Reporter);
   Run_Parser(Reporter);
   null;
end Main;
