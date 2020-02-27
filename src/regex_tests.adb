package body Regex_Tests is
   function Name (T : Regex_Test) return Test_String is 
   begin
      return Format("Regex Tests");
   end Name;
   
   procedure Register_Tests(T: in out Regex_Test) is 
   begin 
      null;
   end Register_Tests;
   

end Regex_Tests;
