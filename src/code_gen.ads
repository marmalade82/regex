with Parse_Types; use Parse_Types;
use Parse_Types.Regex_AST;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Code_Gen_Types; use Code_Gen_Types;

package Code_Gen is
     
   function Gen_NFA(The_AST: Tree) return NFA;
   
   function Gen_DFA(The_AST: Tree) return DFA;
   
   function Count_State(The_Machine: NFA) return Natural;
   
   function Count_Epsilon_Transitions(The_Machine: NFA) return Natural;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean;
   
   function Recognize(The_Machine: DFA; The_Input: Unbounded_String) return Boolean;
   
   function Count_State(The_Machine: DFA) return Natural;
   
   function Count_Epsilon_Transitions(The_Machine: DFA) return Natural;
   
   subtype Escape_Characters is Abstract_Syntax_Class with 
     Static_Predicate => Escape_Characters in Newline | Tab | Carriage_Return;
   
   subtype Non_NFA_Classes is Abstract_Syntax_Class with
     Static_Predicate => Non_NFA_Classes in Range_Interval | Grouping;
                            
                            
   
   

end Code_Gen;
