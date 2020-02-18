with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

package body Code_Gen_Types is
   function Make_Stream(The_Str: Unbounded_String) return Str_Char_Stream is 
   begin 
      return ( M_Current => 0,
               M_String => The_Str
             
              );
   end Make_Stream;

   overriding
   function Next(Self: in out Str_Char_Stream) return Character is 
      My_Char : Character;
   begin
      Self.M_Current := Self.M_Current + 1;
      My_Char := Element(Self.M_String, Self.M_Current);
      return My_Char;
   end Next;
   
   overriding
   function Has_Next(Self: Str_Char_Stream) return Boolean is 
   begin 
      return Self.M_Current < Natural( Length(Self.M_String)) ;
   end Has_Next;
   
   
   
   use State_Transitions;
   use P_NFA_Input_Transitions;
   use P_FA_States;

   function Charac_Hash(The_Key: Character) return Ada.Containers.Hash_Type is 
   begin 
      return Ada.Strings.Hash((1 => The_Key));
   end Charac_Hash;
   
   function Equiv_Keys (The_Left, The_Right: Character) return Boolean is 
   begin
      return The_Left = The_Right;
   end Equiv_Keys;
   
   function Natural_Hash(The_El: Natural) return Ada.Containers.Hash_Type is 
      
   begin 
      return Ada.Containers.Hash_Type'Mod(The_El);
   end Natural_Hash;
   
   function Equal_Transitions(The_Left, The_Right : Transitions) return Boolean is 
   begin 
      return The_Left.input_transitions = The_Right.input_transitions and then The_Left.epsilon_transitions = The_Right.epsilon_transitions;
   end Equal_Transitions;
   
   function As_String(The_States: Set) return String is 
      My_Str : Unbounded_String := To_Unbounded_String("");
      procedure Accumulate(The_Position : P_FA_States.Cursor) is 
      begin
         My_Str := My_Str & Element(The_Position)'Image;
      end Accumulate;
   begin
      Iterate(The_States, Accumulate'Access);
      return To_String(My_Str);
   end As_String;
   
   function As_String(The_Input_Transitions: P_NFA_Input_Transitions.Map) return String is 
      My_Str : Unbounded_String := To_Unbounded_String("");
      procedure Accumulate(The_Position : P_NFA_Input_Transitions.Cursor) is 
      begin
         My_Str := My_Str & Key(The_Position) & "->" & As_String(Element(The_Position)) & " ";
      end Accumulate;
   begin 
      Iterate(The_Input_Transitions, Accumulate'Access);
      return To_String(My_Str);
   end As_String;
      
   function Empty_Transitions return Transitions is 
      
   begin
      return ( input_transitions => Empty_Map,
               epsilon_transitions => Empty_Set,
               kind => By_Char,
               range_inputs => Inputs.Empty_Set,
               range_states => P_FA_States.Empty_Set
              );
   end Empty_Transitions; 
   
   function Join(The_First : State_Transitions.Vector; The_Second: State_Transitions.Vector) return State_Transitions.Vector is 
      My_Vector: State_Transitions.Vector;
   begin 
      My_Vector := Empty_Vector;
      Append(My_Vector, The_First);
      Append(My_Vector, The_Second);
      return My_Vector;
   end Join;
   
   function "=" (The_Left, The_Right : DFA_State ) return Boolean is 
   begin 
      return The_Left.number = The_Right.number and then The_Left.state = The_Right.state;
   end "=";
   
   procedure Enqueue(The_Queue : in out DFA_States_Queue.List; The_Element : DFA_State) is 
   
   begin
      DFA_States_Queue.Prepend(The_Queue, The_Element, 1);
   end Enqueue;
   
   function Dequeue(The_Queue: in out DFA_States_Queue.List; The_Element : out DFA_State) return Boolean is 
      My_Success : Boolean;
   begin
      My_Success := not DFA_States_Queue.Is_Empty(The_Queue);
      
      if My_Success then
         The_Element := DFA_States_Queue.Last_Element(The_Queue);
         DFA_States_Queue.Delete_Last(The_Queue);
      end if;
      
      return My_Success;
   end Dequeue;
   
   procedure Merge(The_Container : in out FA_States; The_New_States : FA_States) is 
      use P_FA_States;
   begin
      Union(The_Container, The_New_States);
   end Merge;
   
   procedure Iter(The_Transitions : NFA_Range_Complements; 
                  The_Proc : not null access procedure (The_Element : NFA_Range_Complement)) is 
      use P_NFA_Range_Complements;
      procedure Run_Proc(The_Position : P_NFA_Range_Complements.Cursor) is 
      begin
         The_Proc( Element(The_Position));
      end Run_Proc;
   begin
      Iterate(The_Transitions, Run_Proc'Access);
   end Iter;

   procedure Iter(The_Inputs: FA_Inputs; The_Proc: not null access procedure (The_Input : in Character)) is 
      use Inputs;
      procedure Run_Proc(The_Position: Inputs.Cursor) is 
      begin
         The_Proc(Element(The_Position));
      end Run_Proc;
   begin 
      Iterate(The_Inputs, Run_Proc'Access);
   end Iter;
   
   procedure Iter(The_Transitions : NFA_Input_Transitions; 
                  The_Proc : not null access procedure (The_Key : in Character; The_Element : P_FA_States.Set)) is 
      use P_NFA_Input_Transitions;
      procedure Run_Proc(The_Position: P_NFA_Input_Transitions.Cursor) is 
      begin
         The_Proc(Key(The_Position), Element(The_Position));
      end Run_Proc;
   begin
      Iterate(The_Transitions, Run_Proc'Access);
   end Iter;
   
   procedure Iter(The_States : FA_States; The_Proc: not null access procedure (The_Input : in Natural)) is 
      use P_FA_States;
      procedure Run_Proc(The_Position : P_FA_States.Cursor) is 
      begin
         The_Proc(Element(The_Position));
      end Run_Proc;
   begin 
      Iterate(The_States, Run_Proc'Access);
   end Iter;
   
   function Empty return P_NFA_Input_Transitions.Map is 
   begin 
      return P_NFA_Input_Transitions.Empty_Map;
   end Empty;
   
   function Empty return DFA_Input_Transitions.Map is 
   begin 
      return DFA_Input_Transitions.Empty_Map;
   end Empty;
   
   function Empty return P_NFA_Range_Complements.Vector is 
   begin 
      return P_NFA_Range_Complements.Empty_Vector;
   end Empty;
   
   function Transition_Exists(The_Transitions: NFA_Input_Transitions; The_Input : Character) return Boolean is 
      use P_NFA_Input_Transitions;
   begin 
      return Find(The_Transitions, The_Input) /= P_NFA_Input_Transitions.No_Element;
   end Transition_Exists;
   
   procedure Add_Transitions_For_Input(The_Transitions: in out NFA_Input_Transitions; The_Input: Character; The_Dests : FA_States) is 
      use P_NFA_Input_Transitions;
      My_Old_Dests : FA_States;
   begin 
      if not Transition_Exists(The_Transitions, The_Input) then 
         Insert(The_Transitions, The_Input, The_Dests);
      else
         -- We replace with the union
         My_Old_Dests := Element(The_Transitions, The_Input);
         Replace(The_Transitions, The_Input, Union(The_Dests, My_Old_Dests)); 
      end if;
   end Add_Transitions_For_Input;

   procedure Add_Complement(The_Container : in out NFA_Range_Complements; The_Complement : NFA_Range_Complement) is 
   begin 
      P_NFA_Range_Complements.Append(The_Container, The_Complement);
   end Add_Complement;

end Code_Gen_Types;
