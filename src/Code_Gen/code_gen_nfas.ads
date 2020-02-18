with Code_Gen_Types; use Code_Gen_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Parse_Types; use Parse_Types; use Parse_Types.Regex_AST;


package Code_Gen_NFAs is
   use State_Transitions;
   use P_NFA_Input_Transitions;
   use P_FA_States;
   
   Invalid_Subtree: exception;
   Unknown_AST_Token: exception;

   function Make_Character_NFA(The_Character : Character) return NFA;

   function Make_Concat_NFA(The_Left : NFA; The_Right : NFA) return NFA;
   
   function Make_Wildcard_NFA(The_NFA : NFA) return NFA;
   
   function Make_Union_NFA(The_Left : NFA; The_Right: NFA) return NFA;
   
   function Make_Range_NFA(The_Parent: Regex_AST.Cursor) return NFA;
   
   function Make_Complement_NFA(The_Parent: Regex_AST.Cursor) return NFA;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean;
   
   function Get_Epsilon_Closure(The_Current_States: P_FA_States.Set; The_State_Table: State_Transitions.Vector) return P_FA_States.Set;

   function Recognize(The_Machine: NFA; The_Stream: in out Char_Stream'Class) return Boolean;

   type N_Finite_Automaton is new Finite_Automaton with record 
      M_Current_States: Set;
      M_Machine: NFA;
      M_Consumed : Unbounded_String;
   end record;
   function Consume(Self: in out N_Finite_Automaton; The_Input: Character) return Automaton_Result;
   function Evaluate_Result(Self: N_Finite_Automaton) return Automaton_Result;
   function Make_Automaton(The_Machine : NFA) return N_Finite_Automaton;
   


      
end Code_Gen_NFAs;
