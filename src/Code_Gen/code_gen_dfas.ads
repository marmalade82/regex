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
   
   function Recognize(The_Machine: DFA; The_Input: Unbounded_String) return Boolean;
   
   function Recognize(The_Machine: DFA; The_Stream: in out Char_Stream'Class) return Boolean;
   
   type D_Finite_Automaton is new Finite_Automaton with record 
      M_Current_State: Natural;
      M_Machine: DFA;
      M_Consumed : Unbounded_String;
      M_Failed: Boolean;
   end record;
   function Consume(Self: in out D_Finite_Automaton; The_Input: Character) return Automaton_Result;
   function Evaluate_Result(Self: D_Finite_Automaton) return Automaton_Result;
   function Make_Automaton(The_Machine : DFA) return D_Finite_Automaton;

end Code_Gen_DFAs;
