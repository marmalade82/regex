with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Parse_Types; use Parse_Types; use Parse_Types.Token_Vector;
use Parse_Types.Regex_AST;

package Parser is
   
   function Get_Tree ( p_tree : Tree ) return Unbounded_String;
   
   function Count(p_tree: Tree) return Natural;
   
   function Includes_Token(p_tree: Tree; p_token: Abstract_Syntax_Token) return Boolean;
   
   function Parse(p_input : Vector; p_tree : out Tree) return Boolean;
   

end Parser;
