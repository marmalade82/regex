with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Lexer_Tests is

   type Lexer_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out Lexer_Test);
   
   function Name(T: Lexer_Test) return Message_String;
   
end Lexer_Tests;
