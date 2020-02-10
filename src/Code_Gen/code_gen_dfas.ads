with Parse_Types; use Parse_Types;
use Parse_Types.Regex_AST;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Code_Gen_Types; use Code_Gen_Types;

package Code_Gen_DFAs is

   
   function NFA_To_DFA(The_NFA : NFA) return DFA;

end Code_Gen_DFAs;
