with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Regex_Tests is
   type Regex_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out Regex_Test);
   
   function Name(T: Regex_Test) return Message_String;
   

end Regex_Tests;
