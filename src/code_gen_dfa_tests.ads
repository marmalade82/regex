with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Code_Gen_DFA_Tests is

   type Code_Gen_DFA_Test is new Test_Cases.Test_Case with null record;
   
   procedure Register_Tests(T: in out Code_Gen_DFA_Test);
   
   function Name(T: Code_Gen_DFA_Test) return Message_String;

end Code_Gen_DFA_Tests;
