with Parse_Types; use Parse_Types;
use Parse_Types.Regex_AST;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Code_Gen is
   
   Invalid_Subtree: exception;
   Unknown_AST_Token: exception;
   
   function Charac_Hash(The_Key: Character) return Ada.Containers.Hash_Type;
   
   package Inputs is new Ada.Containers.Hashed_Sets 
     ( Element_Type => Character,
       Hash => Charac_Hash,
       Equivalent_Elements => Standard."="
      );
   
   function Natural_Hash(The_El: Natural) return Ada.Containers.Hash_Type;
   
   package NFA_States is new Ada.Containers.Hashed_Sets
     ( Element_Type => Natural,
       Hash => Natural_Hash,
       Equivalent_Elements => Standard."="
      );
   
   
   function Equiv_Keys (The_Left, The_Right: Character) return Boolean;
   
   package NFA_Input_Transitions is new Ada.Containers.Hashed_Maps
     ( Key_Type => Character,
       Element_Type => NFA_States.Set,
       Hash => Charac_Hash,
       Equivalent_Keys => Equiv_Keys,
       "=" => NFA_States."="
      );
   
   type Transition_Kind is (By_Char, By_Range, By_Range_Complement);
   
   type Transitions is record 
      input_transitions: NFA_Input_Transitions.Map;
      epsilon_transitions: NFA_States.Set;
      kind: Transition_Kind;
      range_inputs: Inputs.Set;
      range_states: NFA_States.Set;
   end record;	
   
   function Equal_Transitions(The_Left, The_Right : Transitions) return Boolean;
   
   package State_Transitions is new Ada.Containers.Vectors 
     ( Index_Type => Natural,
       Element_Type => Transitions,
       "=" => Equal_Transitions
      );
      
   type NFA is record 
      start: Natural;
      states: State_Transitions.Vector;
      accepting: NFA_States.Set; 
   end record;
   
   function Gen_NFA(The_AST: Tree) return NFA;
   
   function Count_State(The_Machine: NFA) return Natural;
   
   function Count_Epsilon_Transitions(The_Machine: NFA) return Natural;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean;
   
   subtype Escape_Characters is Abstract_Syntax_Class with 
     Static_Predicate => Escape_Characters in Newline | Tab | Carriage_Return;
   
   subtype Non_NFA_Classes is Abstract_Syntax_Class with
     Static_Predicate => Non_NFA_Classes in Range_Interval | Grouping;
                            
                            
   
   

end Code_Gen;
