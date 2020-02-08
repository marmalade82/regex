with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;

package Parse_Types is

   Unexpected_Token : exception;
   
   type Class is ( Character, Pipe, EOF,
                   Left_Bracket, Right_Bracket, Hyphen,
                   Caret, Asterisk, Plus, Question,
                   Left_Paren, Right_Paren, Dollar,
                   Tab, Carriage_Return, Newline
                  );
   
   type Token is record
      f_class : Class;
      f_lexeme: Unbounded_String;
   end record;
   
   package Token_Vector is new Ada.Containers.Vectors 
     (
      Index_Type => Natural,
      Element_Type => Token
     );
   
   type Abstract_Syntax_Class is 
     ( Character, Union, Concat, Range_Group,
       Range_Interval, Grouping, Range_Complement,
       Zero_Or_More, One_Or_More, Optional, Match_Start,
       Match_End, Tab, Carriage_Return, Newline
     );
   
   type Abstract_Syntax_Token is record
      f_class : Abstract_Syntax_Class;
      f_lexeme: Unbounded_String;
   end record;
     
   package Regex_AST is new Ada.Containers.Multiway_Trees
     ( Element_Type => Abstract_Syntax_Token
      );

   function Make_Token (p_class : Class; p_lexeme : Unbounded_String) return Token;
   
   function EOF return Token;
   
   function Make_Token (p_class : Abstract_Syntax_Class; p_lexeme : Unbounded_String) return Abstract_Syntax_Token;
   
end Parse_Types;
