with Lexer_Tests;

package body Lexer_Suite is
   
   Result : aliased Test_Suite;
   
   Test_1 : aliased Lexer_Tests.Lexer_Test;
   
   function Suite return Access_Test_Suite is 
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;
     
   

end Lexer_Suite;
