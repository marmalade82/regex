with Code_Gen_DFA_Tests;

package body Code_Gen_DFA_Suite is
   Result : aliased Test_Suite;
   
   Test_1 : aliased Code_Gen_DFA_Tests.Code_Gen_DFA_Test;
      
   function Suite return Access_Test_Suite is 
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;
   

end Code_Gen_DFA_Suite;
