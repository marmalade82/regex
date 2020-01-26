with AUnit.Assertions; use AUnit.Assertions;
with Lexer; use Lexer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Lexer_Tests is
   function Name (T : Lexer_Test) return Test_String is 
   begin
      return Format("Lexer Tests");
   end Name;
   
   procedure Test_Lex_Char(T : in out Test_Case'Class)
   is
      v_output : Output;
   begin
      v_output := Lex("a");
      Assert( Get_Class(v_output) = Lexer.Character , 
              "Lexer does not identify lexeme as a Character");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("a"), 
              "Lexer does not save correct lexeme"); 
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Char;
     
   procedure Test_Lex_Escaped_Char(T : in out Test_Case'Class)
   is
      v_output : Output;
   begin
      v_output := Lex("\a");
      Assert( Get_Class(v_output) = Lexer.Character , 
              "Lexer does not identify lexeme as a Character");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("\a"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Escaped_Char;
   
   procedure Test_Lex_Asterisk(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("*");
      Assert( Get_Class(v_output) = Lexer.RepeatOrZero , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("*"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Asterisk;
   
   procedure Test_Lex_Plus(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("+");
      Assert( Get_Class(v_output) = Lexer.RepeatOrOne , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("+"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Plus;
   
   procedure Test_Lex_Question(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("?");
      Assert( Get_Class(v_output) = Lexer.Optional , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("?"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Question;
   
   procedure Test_Lex_Pipe(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("|");
      Assert( Get_Class(v_output) = Lexer.Union , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("|"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Pipe;
   
   procedure Test_Lex_Left_Bracket(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("[");
      Assert( Get_Class(v_output) = Lexer.LeftBracket , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("["), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Left_Bracket;
   
   procedure Test_Lex_Right_Bracket(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("]");
      Assert( Get_Class(v_output) = Lexer.RightBracket , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("]"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Right_Bracket;
   
   procedure Test_Lex_Left_Paren(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("(");
      Assert( Get_Class(v_output) = Lexer.LeftParen , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("("), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Left_Paren;
   
   procedure Test_Lex_Right_Paren(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex(")");
      Assert( Get_Class(v_output) = Lexer.RightParen , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String(")"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Right_Paren;
   
   procedure Test_Lex_Hyphen(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("-");
      Assert( Get_Class(v_output) = Lexer.Hyphen , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("-"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Hyphen;
   
   procedure Test_Lex_Empty(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("");
      Assert( Get_Class(v_output) = Lexer.EOF , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String(""), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Empty;
   
   procedure Test_Lex_Newline(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("\n");
      Assert( Get_Class(v_output) = Lexer.Newline , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("\n"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Newline;
   
   procedure Test_Lex_Tab(T : in out Test_Case'Class)
   is
      v_output: Output;
   begin
      v_output := Lex("\t");
      Assert( Get_Class(v_output) = Lexer.Tab , 
              "Lexer does not give lexeme correct class");
      Assert( Get_Lexeme(v_output) = To_Unbounded_String("\t"), 
              "Lexer does not save correct lexeme");
      Assert( Get_Remaining(v_output) = "", 
              "Lexer does not save correct remaining input");
   end Test_Lex_Tab;
   
   
   
   
   procedure Register_Tests (T: in out Lexer_Test) is 
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Test_Lex_Char'Access, "Test lexing single char");
      Register_Routine(T, Test_Lex_Escaped_Char'Access, "Test lexing escaped char");
      Register_Routine(T, Test_Lex_Asterisk'Access, "Test lexing '*'");
      Register_Routine(T, Test_Lex_Plus'Access, "Test lexing '+'");
      Register_Routine(T, Test_Lex_Question'Access, "Test lexing '?'");
      Register_Routine(T, Test_Lex_Pipe'Access, "Test lexing '|'");
      Register_Routine(T, Test_Lex_Left_Bracket'Access, "Test lexing '['");
      Register_Routine(T, Test_Lex_Right_Bracket'Access, "Test lexing ']'");
      Register_Routine(T, Test_Lex_Left_Paren'Access, "Test lexing '('");
      Register_Routine(T, Test_Lex_Right_Paren'Access, "Test lexing ')'");
      Register_Routine(T, Test_Lex_Hyphen'Access, "Test lexing '-'");
      Register_Routine(T, Test_Lex_Empty'Access, "Test lexing empty input");
      Register_Routine(T, Test_Lex_Newline'Access, "Test lexing '\n'");
      Register_Routine(T, Test_Lex_Tab'Access, "Test lexing '\t'");


   end Register_Tests;
   
   

end Lexer_Tests;
