with Code_Gen_Types; use Code_Gen_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Parse_Types; use Parse_Types; use Parse_Types.Regex_AST;


package Code_Gen_NFAs is
   
   Invalid_Subtree: exception;
   Unknown_AST_Token: exception;

   function Make_Character_NFA(The_Character : Character) return NFA;

   function Make_Concat_NFA(The_Left : NFA; The_Right : NFA) return NFA;
   
   function Make_Wildcard_NFA(The_NFA : NFA) return NFA;
   
   function Make_Union_NFA(The_Left : NFA; The_Right: NFA) return NFA;
   
   function Make_Range_NFA(The_Parent: Regex_AST.Cursor) return NFA;
   
   function Make_Complement_NFA(The_Parent: Regex_AST.Cursor) return NFA;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean;
   
   function Get_Epsilon_Closure(The_Current_States: NFA_States.Set; The_State_Table: State_Transitions.Vector) return NFA_States.Set;

end Code_Gen_NFAs;
