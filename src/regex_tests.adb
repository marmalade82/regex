with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;
with Regex; use Regex;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package body Regex_Tests is
   function Name (T : Regex_Test) return Test_String is 
   begin
      return Format("Regex Tests");
   end Name;
   
   procedure Test_Letter(T : in out Test_Case'Class) is
      My_Input: String := "a";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Test(My_Machine, "a"), "Does not recognize whole input"  );
      Assert( not Regex.Test(My_Machine, "b"), "Does not reject incorrect input");
   end Test_Letter;
   
   procedure Test_Union(T : in out Test_Case'Class) is
      My_Input: String := "a|b";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Test(My_Machine, "a"), "Does not recognize whole input"  );
      Assert( Regex.Test(My_Machine, "b"), "Does not recognize whole input"  );
      Assert( not Regex.Test(My_Machine, "c"), "Does not reject incorrect input");
   end Test_Union;
   
   procedure Test_Concat(T : in out Test_Case'Class) is
      My_Input: String := "ab";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Test(My_Machine, "ab"), "Does not recognize whole input"  );
      Assert( not Regex.Test(My_Machine, "cb"), "Does not reject incorrect input");
   end Test_Concat;
   
   procedure Test_Wildcard(T : in out Test_Case'Class) is
      My_Input: String := "a*";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Test(My_Machine, ""), "Does not recognize whole input"  );
      Assert( Regex.Test(My_Machine, "a"), "Does not recognize whole input"  );
      Assert( Regex.Test(My_Machine, "aa"), "Does not recognize whole input"  );
      Assert( Regex.Test(My_Machine, "aaa"), "Does not recognize whole input"  );
      Assert( Regex.Test(My_Machine, "b"), "Does not recognize start state matching");
   end Test_Wildcard;
   
   procedure Test_Letter_In_Middle(T : in out Test_Case'Class) is
      My_Input: String := "a";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Test(My_Machine, "bbabb"), "Does not recognize single match"  );
      Assert( not Regex.Test(My_Machine, "bbbbb"), "Does not reject incorrect input");
   end Test_Letter_In_Middle;
   
   procedure Register_Tests(T: in out Regex_Test) is 
      use AUnit.Test_Cases.Registration;
   begin 
      Register_Routine(T, Test_Letter'Access, "Single letter");
      Register_Routine(T, Test_Union'Access, "Single union");
      Register_Routine(T, Test_Concat'Access, "Single concat");
      Register_Routine(T, Test_Wildcard'Access, "Single wildcard");
      Register_Routine(T, Test_Letter_In_Middle'Access, "Single letter in middle");
   end Register_Tests;
   

end Regex_Tests;
