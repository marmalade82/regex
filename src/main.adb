with Ada.Text_IO;
with Lexer_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Main is
   procedure Run is new AUnit.Run.Test_Runner(Lexer_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
   null;
end Main;
