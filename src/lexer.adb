package body Lexer is
   function Lex (input : String) return Output is 
   begin
      return Lex(To_Unbounded_String(input));
   end Lex;
   
   function Lex (input : Unbounded_String) return Output is
      v_output : Output;
      v_length: Natural;
      v_class : Class;
      v_lexeme: Unbounded_String;
   begin
      v_length := Length(input);
      if v_length > 0 then 
         if Element(input, 1) = '\' and v_length > 1 then
            v_lexeme := Unbounded_Slice(input, 1, 2);
            case Element(input, 2) is
               when 'n' =>
                  v_class := Newline;
               when 't' =>
                  v_class := Tab;
               when others => 
                  v_class := Character;
            end case;
            
            v_output :=
               ( f_remaining => Unbounded_Slice(input, 3, Length(input)),
                f_token => 
                  ( f_class => v_class,
                    f_lexeme => v_lexeme
                   )
               );
           
         elsif Element(input, 1) /= '\' then 
            case Element(input, 1) is
               when '*' =>
                  v_class := RepeatOrZero;
               when '+' =>
                  v_class := RepeatOrOne;
               when '?' => 
                  v_class := Optional;
               when '|' => 
                  v_class := Union;
               when '[' =>
                  v_class := LeftBracket;
               when ']' =>
                  v_class := RightBracket;
               when '(' =>
                  v_class := LeftParen;
               when ')' =>
                  v_class := RightParen;
               when '-' =>
                  v_class := Hyphen;
               when others =>
                  v_class := Character;
            end case;
            v_output := 
                    ( f_remaining => Unbounded_Slice(input, 2, Length(input)),
                      f_token => 
                        ( f_class => v_class,
                          f_lexeme => Unbounded_Slice(input, 1, 1)
                        )
                     );
         else -- The previous cases are the only valid cases. This is the error case, which consumes the remaining input.
            v_output := 
              ( f_remaining => To_Unbounded_String(""),
                f_token => 
                  ( f_class => Error,
                    f_lexeme => input
                   )
               );
         end if;
      else 
         v_output := 
           ( f_remaining => input,
             f_token => 
               ( f_class => EOF,
                 f_lexeme => To_Unbounded_String("")
                )
            );
      end if;
      return v_output;
   end;
     
   function Get_Lexeme (O : Output) return Unbounded_String is  
   begin
      return Get_Lexeme(Get_Token(O));
   end Get_Lexeme;
   
   function Get_Lexeme(T: Token) return Unbounded_String is
   begin
      return T.f_lexeme;
   end Get_Lexeme;
   
   function Get_Class(O: Output) return Class is 
   begin
      return Get_Class(Get_Token(O));
   end Get_Class;
   
   function Get_Class(T: Token) return Class is 
   begin
      return T.f_class;
   end Get_Class;
   
   function Get_Token(O: Output) return Token is 
   begin
      return O.f_token;
   end Get_Token;
   
   function Get_Remaining(O: Output) return Unbounded_String is 
   begin
      return O.f_remaining;
   end Get_Remaining;
   
     
     
   

end Lexer;
