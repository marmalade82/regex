with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;

package parser is
   type Class is ( Character, Newline, Pipe
                   
                  );
   
   type Token is record
      f_class : Class;
      f_lexeme: Unbounded_String;
   end record;
   
   function Make_Token (p_class : Class; p_lexeme : Unbounded_String) return Token;
   
   package Token_Vector is new Ada.Containers.Vectors 
     (
      Index_Type => Natural,
      Element_Type => Token
     );
   
   use Token_Vector;
   
   type Abstract_Syntax_Class is 
     ( Newline, Character, Pipe
     );
   
   type Abstract_Syntax_Token is record
      f_class : Abstract_Syntax_Class;
      f_lexeme: Unbounded_String;
   end record;
   
   function Make_Token (p_class : Abstract_Syntax_Class; p_lexeme : Unbounded_String) return Abstract_Syntax_Token;
    
   package Regex_AST is new Ada.Containers.Multiway_Trees
     ( Element_Type => Abstract_Syntax_Token
      );
   
   use Regex_AST;
   
   function Count(p_tree: Tree) return Natural;
   
   function Includes_Token(p_tree: Tree; p_token: Abstract_Syntax_Token) return Boolean;
   
   function Parse(p_input : Vector; p_tree : out Tree) return Boolean;
   

end parser;
