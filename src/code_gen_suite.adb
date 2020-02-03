with Code_Gen_Tests;

package body Code_Gen_Suite is

   Result : aliased Test_Suite;
   
   Test_1 : aliased Code_Gen_Tests.Code_Gen_Test;
   
   function Suite return Access_Test_Suite is 
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Code_Gen_Suite;
