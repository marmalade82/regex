package body Parse_Types is

   function Make_Token(p_class : Class; p_lexeme : Unbounded_String) return Token is
      v_token : Token;
   begin
      v_token :=
        ( f_class => p_class,
          f_lexeme => p_lexeme
         );
      return v_token;

   end Make_Token;

   function Make_Token(p_class : Abstract_Syntax_Class;
                       p_lexeme : Unbounded_String) return Abstract_Syntax_Token is
      v_token : Abstract_Syntax_Token;
   begin
      v_token :=
        ( f_class => p_class,
          f_lexeme => "" & p_lexeme
         );
      return v_token;

   end Make_Token;

   function EOF return Token is
   begin
      return ( f_class => EOF,
               f_lexeme => To_Unbounded_String("!")
              );
   end;

end Parse_Types;
