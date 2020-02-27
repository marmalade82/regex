with Regex_Tests; use Regex_Tests;

package body Regex_Suite is

   Result : aliased Test_Suite;
   
   Test_1 : aliased Regex_Test;
   
   function Suite return Access_Test_Suite is 
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Regex_Suite;
