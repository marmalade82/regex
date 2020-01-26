with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Parser_Tests is

   type Parser_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out Parser_Test);
   
   function Name(T: Parser_Test) return Message_String;
   

end Parser_Tests;
