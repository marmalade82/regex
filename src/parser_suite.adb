with Parser_Tests;


package body Parser_Suite is
   Result : aliased Test_Suite;
   
   Test_1 : aliased Parser_Tests.Parser_Test;
   
   function Suite return Access_Test_Suite is 
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;
   

end Parser_Suite;
