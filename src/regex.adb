with Ada.Text_IO; use Ada.Text_IO;
with Lexer; use Lexer;
with Parse_Types; use Parse_Types;
with Parser;
with Code_Gen; use Code_Gen;
with Code_Gen_Types; use Code_Gen_Types;

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
                              when Error =>
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

   function Compile(The_Machine: out Regex; S: String; The_Error: out Unbounded_String) return Boolean is 
      use Parse_Types.Token_Vector;
      My_S : Unbounded_String;
      My_Remaining: Unbounded_String;
      My_Output : Lexer_Output;
      My_Vector : Token_Vector.Vector := Empty_Vector;
      My_Index : Natural := 0;
      My_AST : Parse_Types.Regex_AST.Tree;
      My_Parse_Success : Boolean;
      My_DFA : DFA;
   begin 
      The_Error := To_Unbounded_String("");
      My_S := To_Unbounded_String(S);
      
      while Length(My_S) > 0 loop
         My_Output := Lex(My_S, My_Index, My_Remaining);
         
         case My_Output.m_token.f_class is 
            when Error => 
               The_Error := To_Unbounded_String("Invalid character sequence found around position " & My_Output.m_index'Image);
            when others =>
               My_Vector.Append( Convert_To_Token(My_Output) );
         end case;
         My_Index := My_Index + Length(My_Output.M_Token.f_lexeme);
         My_S := My_Remaining;
      end loop;
      
      -- After this loop, we have a full vector of tokens for the parser, besides the EOF.
      My_Vector.Append(Parse_Types.EOF);
      
      -- But we should handle this in a separate function really, to allow for early exist when errors are encountered.
      My_Parse_Success := Parser.Parse(My_Vector, My_AST);
      
      if My_Parse_Success then 
         My_DFA := Gen_DFA(My_AST);
         
         The_Machine := ( M_Automaton => Make_Automaton(My_DFA)
                            
                         );
         null;
         -- Here is where we would process parse warnings, since we have errors despite successfully parsing.
      else
         -- Here is where we would process parse errors, since the parse failed -- we don't have a tree that 
         -- can successfully do an NFA generation.
         null;
      end if;
      
      return True;
   end Compile;
   
   function Compile(The_Machine: out Regex; S: String) return Boolean is
      My_Error : Unbounded_String;
      My_Success : Boolean;
   begin 
      My_Success := Compile(The_Machine, S, My_Error);
      if not My_Success then 
         Put_Line(To_String(My_Error));
      end if;
      
      return My_Success;
   end Compile;
   
   function Test(The_Machine: in out Regex; S: String) return Boolean is 
      My_Stream: Str_Char_Stream;
      Next_Result : Automaton_Result;
   begin 
      -- Run for empty input evaluation
      Reset(The_Machine.M_Automaton);
      My_Stream := Make_Stream(To_Unbounded_String(S));
      if Evaluate_Result(The_Machine.M_Automaton).M_Is_Accepted then 
         return True;
      end if;
      
      for I in S'First .. S'Last loop 
         Reset(The_Machine.M_Automaton);
         My_Stream := Make_Stream(To_Unbounded_String(S (I..S'Last)));
         
         -- If, while consuming input, we are ever in an accepting state, then some substring
         -- of the total input successfully matches the regex.
         Next_Result := Evaluate_Result(The_Machine.M_Automaton);
         while My_Stream.Has_Next and not Next_Result.M_Is_Failed loop 
            Next_Result := The_Machine.M_Automaton.Consume(My_Stream.Next);
            
            if not Next_Result.M_Is_Failed and then Next_Result.M_Is_Accepted then 
               return True;
            end if;
         end loop;
      end loop;
      
      -- If we get here, we found no matches at all.
      return False;
   end Test;
   
   procedure Add_Result(The_Results: in out Results; New_Result : Unbounded_String) is begin 
      String_Vector.Append(The_Results, New_Result);
   end Add_Result;
   
   function Match(The_Machine: in out Regex; S: String) return Results is 
      My_Stream: Str_Char_Stream;
      Next_Result : Automaton_Result;
      My_Results : Results;
   begin 
      for I in S'First .. S'Last loop 
         Reset(The_Machine.M_Automaton);
         My_Stream := Make_Stream(To_Unbounded_String(S (I..S'Last)));
         
         Next_Result := Evaluate_Result(The_Machine.M_Automaton);
         
         -- from beginning to end, we need to consume input until failure, and check upon failure if the last 
         -- state before failure was accepting. If it was, we matched.
         while My_Stream.Has_Next and not Next_Result.M_Is_Failed loop 
            Next_Result := The_Machine.M_Automaton.Consume(My_Stream.Next);
            
            if Next_Result.M_Is_Accepted and then not Next_Result.M_Is_Failed and then Length(Next_Result.M_Consumed) > 0 then 
               Add_Result(My_Results, Next_Result.M_Consumed);
            end if;
         end loop;
      end loop;
      
      return My_Results;
   end Match;

   function Empty_Results return Results is begin 
      return String_Vector.Empty_Vector;
   end Empty_Results;

   function As_String(The_Results: Results) return String is 
      My_String : Unbounded_String := To_Unbounded_String("");
      procedure Build_String(The_Position: String_Vector.Cursor) is begin 
         Ada.Strings.Unbounded.Append(My_String, String_Vector.Element(The_Position));
         Ada.Strings.Unbounded.Append(My_String, ",");
      end Build_String;
   begin 
      String_Vector.Iterate(The_Results, Build_String'Access);
      Ada.Strings.Unbounded.Append(My_String, ".");
      return To_String(My_String);
   end As_String;
   
   function To_Regex_Result(The_Result: Automaton_Result) return Regex_Result is 
   begin 
      return ( M_Consumed => The_Result.M_Consumed,
               M_Is_Failed => The_Result.M_Is_Failed,
               M_Is_Accepted => The_Result.M_Is_Accepted
             );
   end To_Regex_Result;
   
   function Consume(The_Machine: in out Regex; C: Character) return Regex_Result is 
   begin 
      return To_Regex_Result( Consume(The_Machine.M_Automaton, C) );
   end Consume;
   
   function Evaluate(The_Machine: in out Regex) return Regex_Result is
   begin 
      return To_Regex_Result( Evaluate_Result(The_Machine.M_Automaton) );
   end Evaluate;
   
   procedure Reset(The_Machine: in out Regex) is begin 
      Reset(The_Machine.M_Automaton);
   end Reset;

end Regex;
