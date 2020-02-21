with Ada.Text_IO; use Ada.Text_IO;
with Lexer; use Lexer;
with Parse_Types; use Parse_Types;
with Parser;

package body Regex is
   type Lexer_Output is record 
      M_Token: Lexer.Token;
      M_Index: Natural;
   end record;
   function Convert_To_Token(The_Output: Lexer_Output) return Parse_Types.Token is
   begin 
      return ( f_lexeme => The_Output.M_token.f_lexeme,
               f_index => The_Output.M_Index,
               f_class => (case The_Output.M_token.f_class is 
                              when Lexer.Character => Parse_Types.Character,
                              when LeftBracket => Left_Bracket, 
                              when RightBracket => Right_Bracket,
                          
                              when LeftParen => Left_Paren, 
                              when RightParen => Right_Paren,
                              when Union => Pipe, 
                              when Optional => Question, 
                              when RepeatOrZero => Asterisk,
                              when RepeatOrOne => Plus, 
                              when Hyphen => Hyphen, 
                              when EOF => EOF,
                              when Newline => Newline, 
                              when Tab => Tab,
                              when others =>
                                raise Cannot_Convert with "Lexer token does not have corresponding parser token: " & The_Output.M_Token.f_class'Image
                          )
               
              );
   end Convert_To_Token;
   
   function Lex(The_String : Unbounded_String; The_Index: Natural; The_Remaining: out Unbounded_String) return Lexer_Output is 
      My_Output: Output;
   begin 
      My_Output := Lex(The_String);
      The_Remaining := My_Output.f_remaining;
      
      return ( M_Token => My_Output.f_token,
               M_Index => The_Index
              );
   end Lex;

   function Compile(The_Machine: in out Regex; S: String; The_Error: out Unbounded_String) return Boolean is 
      use Parse_Types.Token_Vector;
      My_S : Unbounded_String;
      My_Output : Lexer_Output;
      My_Vector : Token_Vector.Vector := Empty_Vector;
      My_Index : Natural := 0;
      My_AST : Parse_Types.Regex_AST.Tree;
      My_Parse_Success : Boolean;
   begin 
      The_Error := To_Unbounded_String("");
      My_S := To_Unbounded_String(S);
      
      while Length(My_S) > 0 loop
         My_Output := Lex(My_S, My_Index, My_S);
         
         case My_Output.m_token.f_class is 
            when Error => 
               The_Error := To_Unbounded_String("Invalid character sequence found around position " & My_Output.m_index'Image);
            when others =>
               My_Vector.Append( Convert_To_Token(My_Output) );
         end case;
         My_Index := My_Index + Length(My_Output.M_Token.f_lexeme);
      end loop;
      
      -- After this loop, we have a full vector of tokens for the parser.
      -- But we should handle this in a separate function really, to allow for early exist when errors are encountered.
      My_Parse_Success := Parser.Parse(My_Vector, My_AST);
      
      if My_Parse_Success then 
         
      else 
         
      end if;
      
      return True;
   end Compile;
   
   function Compile(The_Machine: in out Regex; S: String) return Boolean is
      My_Error : Unbounded_String;
      My_Success : Boolean;
   begin 
      My_Success := Compile(The_Machine, S, My_Error);
      if not My_Success then 
         Put_Line(To_String(My_Error));
      end if;
      
      return My_Success;
   end Compile;

end Regex;
