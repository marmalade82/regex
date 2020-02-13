with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Code_Gen_Types is
   
   
   
   
   
   
   function Charac_Hash(The_Key: Character) return Ada.Containers.Hash_Type;
   
   
   
   
   package Inputs is new Ada.Containers.Hashed_Sets 
     ( Element_Type => Character,
       Hash => Charac_Hash,
       Equivalent_Elements => Standard."="
      );
   
   subtype FA_Inputs is Inputs.Set;
   
   procedure Iter(The_Inputs: FA_Inputs; The_Proc: not null access procedure (The_Input : in Character));
   
   function Natural_Hash(The_El: Natural) return Ada.Containers.Hash_Type;
   
   package P_FA_States is new Ada.Containers.Hashed_Sets
     ( Element_Type => Natural,
       Hash => Natural_Hash,
       Equivalent_Elements => Standard."="
      );
   
   subtype FA_States is P_FA_States.Set;
   
   procedure Merge(The_Container : in out FA_States; The_New_States : FA_States);
   
   procedure Iter(The_States : FA_States; The_Proc: not null access procedure (The_Input : in Natural));
   
   function Equiv_Keys (The_Left, The_Right: Character) return Boolean;
   
   package P_NFA_Input_Transitions is new Ada.Containers.Hashed_Maps
     ( Key_Type => Character,
       Element_Type => P_FA_States.Set,
       Hash => Charac_Hash,
       Equivalent_Keys => Equiv_Keys,
       "=" => P_FA_States."="
      );
   
   subtype NFA_Input_Transitions is P_NFA_Input_Transitions.Map;
   
   function Transition_Exists(The_Transitions: NFA_Input_Transitions; The_Input : Character) return Boolean;
   
   procedure Add_Transitions_For_Input(The_Transitions: in out NFA_Input_Transitions; The_Input: Character; The_Dests : FA_States);
   
   procedure Iter(The_Transitions : NFA_Input_Transitions; 
                  The_Proc : not null access procedure (The_Key : in Character; The_Element : P_FA_States.Set));
   
   function Empty return P_NFA_Input_Transitions.Map;
   
   type Transition_Kind is (By_Char, By_Range, By_Range_Complement);
   
   type Transitions is record 
      input_transitions: P_NFA_Input_Transitions.Map;
      epsilon_transitions: P_FA_States.Set;
      kind: Transition_Kind;
      range_inputs: FA_Inputs;
      range_states: P_FA_States.Set;
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
      accepting: P_FA_States.Set;
   end record;
   
   function As_String(The_States: P_FA_States.Set) return String;
   
   function As_String(The_Input_Transitions: P_NFA_Input_Transitions.Map) return String;
   
   function Empty_Transitions return Transitions;
   
   function Join(The_First : State_Transitions.Vector; The_Second: State_Transitions.Vector) return State_Transitions.Vector;
   
   package DFA_Input_Transitions is new Ada.Containers.Hashed_Maps
     ( Key_Type => Character,
       Hash => Charac_Hash,
       Element_Type => Natural,
       Equivalent_Keys => Standard."="
      );
   
   function Empty return DFA_Input_Transitions.Map;
   
   type DFA_Transitions is record 
      input_transitions : DFA_Input_Transitions.Map;
      has_complement : Boolean;
      complement_inputs : Inputs.Set;
      complement_transition : Natural; -- The state to go to if nothing matches the input transitions
   end record;
   
   package DFA_States is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => DFA_Transitions
      );
   
   type DFA is record 
      start: Natural;
      states : DFA_States.Vector;
      accepting : P_FA_States.Set; -- Just like NFAs, DFAs have a set of accepting states.
   end record;
  
   type DFA_State is record 
      number: Natural;
      state: P_FA_States.Set;
   end record;
   
   function "=" (The_Left, The_Right : DFA_State ) return Boolean;
   
   -- This allows queues of DFA states (which remember, are sets of NFA states)
   --   during processing.
   package DFA_States_Queue is new Ada.Containers.Doubly_Linked_Lists
     ( Element_Type => DFA_State,
       "=" => "="
      );
   
   procedure Enqueue(The_Queue : in out DFA_States_Queue.List; The_Element : DFA_State);
   
   function Dequeue(The_Queue: in out DFA_States_Queue.List; The_Element : out DFA_State) return Boolean;
     
   type NFA_Range_Complement is record 
      complement: FA_Inputs;
      destinations: FA_States;
   end record;
   
   package P_NFA_Range_Complements is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => NFA_Range_Complement
         );
   
   subtype NFA_Range_Complements is P_NFA_Range_Complements.Vector;
   
   procedure Iter(The_Transitions : NFA_Range_Complements; 
                  The_Proc : not null access procedure (The_Element : NFA_Range_Complement));
   
   procedure Add_Complement(The_Container : in out NFA_Range_Complements; The_Complement : NFA_Range_Complement);
   
   type Transitions_For_State is record 
      input_transitions : NFA_Input_Transitions;
      range_complements: NFA_Range_Complements;
   end record;
   
   function Empty return P_NFA_Range_Complements.Vector;
   
   type Complement_Conversion is record
      input_transitions : NFA_Input_Transitions;
      complement_inputs : FA_Inputs;
      complement_transitions: FA_States;
   end record;
end Code_Gen_Types;
