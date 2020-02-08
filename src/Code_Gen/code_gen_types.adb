with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

package body Code_Gen_Types is
   use State_Transitions;
   use NFA_Input_Transitions;
   use NFA_States;

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
      procedure Accumulate(The_Position : NFA_States.Cursor) is 
      begin
         My_Str := My_Str & Element(The_Position)'Image;
      end Accumulate;
   begin
      Iterate(The_States, Accumulate'Access);
      return To_String(My_Str);
   end As_String;
   
   function Empty_Transitions return Transitions is 
      
   begin
      return ( input_transitions => Empty_Map,
               epsilon_transitions => Empty_Set,
               kind => By_Char,
               range_inputs => Inputs.Empty_Set,
               range_states => NFA_States.Empty_Set
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

end Code_Gen_Types;
