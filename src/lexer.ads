with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Lexer is
   type Class is 
     ( Character,
       LeftBracket, RightBracket,
       LeftParen, RightParen,
       Union, Optional, RepeatOrZero,
       RepeatOrOne, Error, Hyphen, EOF,
       Newline, Tab
  
     );
   
   type Token is record
      f_class: Class;
      f_lexeme: Unbounded_String;
   end record;

   type Output is record 
      f_remaining : Unbounded_String;
      f_token: Token;
   end record;
      
   
   function Lex (input : Unbounded_String) return Output;
   
   function Lex (input : String) return Output;
   
   function Get_Lexeme(O : Output) return Unbounded_String;
   
   function Get_Lexeme(T: Token) return Unbounded_String;
   
   function Get_Class(O : Output) return Class;
   
   function Get_Class(T: Token) return Class;
   
   function Get_Token(O: Output) return Token;
   
   function Get_Remaining(O: Output) return Unbounded_String;
     
end Lexer;
