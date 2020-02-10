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
   
   function As_String(The_States: NFA_States.Set) return String;
   
   function As_String(The_Input_Transitions: NFA_Input_Transitions.Map) return String;
   
   function Empty_Transitions return Transitions;
   
   function Join(The_First : State_Transitions.Vector; The_Second: State_Transitions.Vector) return State_Transitions.Vector;
   
   package DFA_Input_Transitions is new Ada.Containers.Hashed_Maps
     ( Key_Type => Character,
       Hash => Charac_Hash,
       Element_Type => Natural,
       Equivalent_Keys => Standard."="
      );
   
   type DFA_Transitions is record 
      input_transitions : DFA_Input_Transitions.Map;
   end record;
   
   package DFA_States is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => DFA_Transitions
      );
   
   type DFA is record 
      start: Natural;
      states : DFA_States.Vector;
      accepting : NFA_States.Set; -- Just like NFAs, DFAs have a set of accepting states.
   end record;
  
   type DFA_State is record 
      number: Natural;
      state: NFA_States.Set;
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
     

end Code_Gen_Types;
