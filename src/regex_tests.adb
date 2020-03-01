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
   
   procedure Match_Letter(T : in out Test_Case'Class) is
      use Regex.String_Vector;
      My_Input: String := "a";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
      My_Expected: Results := Empty_Results & 
        To_Unbounded_String("a");
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Match(My_Machine, "a") = My_Expected, "Does not recognize whole input"  );
      Assert( Is_Empty( Regex.Match(My_Machine, "b") ), "Does not reject incorrect input");
   end Match_Letter;
   
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
   
   procedure Match_Union(T : in out Test_Case'Class) is
      use Regex.String_Vector;
      My_Input: String := "a|b";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
      My_Expected_1 : Results := Empty_Results & To_Unbounded_String("a");
      My_Expected_2 : Results := Empty_Results & To_Unbounded_String("b");
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Match(My_Machine, "a") = My_Expected_1, "Does not recognize whole input"  );
      Assert( Regex.Match(My_Machine, "b") = My_Expected_2, "Does not recognize whole input"  );
      Assert( Is_Empty( Regex.Match(My_Machine, "c")), "Does not reject incorrect input");
   end Match_Union;
   
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
    
   procedure Match_Concat(T : in out Test_Case'Class) is
      use Regex.String_Vector;
      My_Input: String := "ab";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
      My_Expected : Results := Empty_Results & To_Unbounded_String("ab");
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Regex.Match(My_Machine, "ab") = My_Expected, "Does not recognize whole input"  );
      Assert( Is_Empty( Regex.Match(My_Machine, "cb") ), "Does not reject incorrect input");
   end Match_Concat;
   
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
   
   procedure Match_Wildcard(T : in out Test_Case'Class) is
      use Regex.String_Vector;
      My_Input: String := "a*";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( Is_Empty(Regex.Match(My_Machine, "")), "Does not recognize whole input"  );
      
      Assert( As_String(Regex.Match(My_Machine, "a")) = "a,.", "Does not recognize whole input, got " & As_String(Regex.Match(My_Machine, "a"))  );
      
      Assert( As_String(Regex.Match(My_Machine, "aa")) = "a,aa,a,.", "Does not recognize whole input, got " & As_String(Regex.Match(My_Machine, "aa"))  );
      
      Assert( Is_Empty( Regex.Match(My_Machine, "b") ), "Does not recognize start state matching, got " & As_String(Regex.Match(My_Machine, "b")) );
   end Match_Wildcard;
   
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
   
   procedure Match_Letter_In_Middle(T : in out Test_Case'Class) is
      use Regex.String_Vector;
      My_Input: String := "a";
      My_Machine: Regex.Regex;
      My_Success : Boolean;
      My_Error : Unbounded_String;
      My_Expected: Results := Empty_Results & To_Unbounded_String("a");
   begin
      My_Success := Compile(My_Machine, My_Input, My_Error);
      Assert(My_Success, "Does not compile");
      
      Assert( As_String(Regex.Match(My_Machine, "bbabbb")) = "a,.", "Does not recognize single match, got " & As_String(Regex.Match(My_Machine, "bbabb"))  );
      Assert( Is_Empty( Regex.Match(My_Machine, "bbbbbb") ), "Does not reject incorrect input");
   end Match_Letter_In_Middle;
   
   procedure Register_Tests(T: in out Regex_Test) is 
      use AUnit.Test_Cases.Registration;
   begin 
      Register_Routine(T, Test_Letter'Access, "Testing single letter");
      Register_Routine(T, Match_Letter'Access, "Matching single letter");
      Register_Routine(T, Test_Union'Access, "Testing Single union");
      Register_Routine(T, Match_Union'Access, "Matching Single union");
      Register_Routine(T, Test_Concat'Access, "Testing Single concat");
      Register_Routine(T, Match_Concat'Access, "Matching Single concat");
      Register_Routine(T, Test_Wildcard'Access, "Testing Single wildcard");
      Register_Routine(T, Match_Wildcard'Access, "Matching Single wildcard");
      Register_Routine(T, Test_Letter_In_Middle'Access, "Testing single letter in middle");
      Register_Routine(T, Match_Letter_In_Middle'Access, "Matching single letter in middle");
   end Register_Tests;
   

end Regex_Tests;
