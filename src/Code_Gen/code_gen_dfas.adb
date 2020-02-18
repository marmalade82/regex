with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Code_Gen_NFAs; use Code_Gen_NFAs;

package body Code_Gen_DFAs is
   use State_Transitions;
   use P_NFA_Input_Transitions;
   use P_FA_States;
   
   function String_Hash(The_String : Unbounded_String) return Ada.Containers.Hash_Type is 
   begin
      return Ada.Strings.Hash(To_String(The_String));
   end String_Hash;
   
   package Seen_States is new Ada.Containers.Hashed_Maps 
     (Key_Type        => Unbounded_String,
      Element_Type    => Natural,
      Hash            => String_Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Standard."=");   
   
   type DFA_Conversion is record 
      states : DFA_States.Vector;
      accepting: FA_States;
   end record;
   
   -- This function expands input transitions (from characters to next states in the NFA)
   -- to go from characters to the epsilon closures of the next states. This
   -- simulates the full NFA operation when it consumes an input.
   function Closurify_Input_Transitions
     ( The_Current_Transitions: NFA_Input_Transitions;
       The_NFA_State_Table : State_Transitions.Vector
      ) return NFA_Input_Transitions is
      
      My_Input_Transitions: NFA_Input_Transitions := Empty;
      procedure Add_By_Input(The_Input: Character; The_Destinations: FA_States) is 
         My_Closure : FA_States;
      begin 
         My_Closure := Get_Epsilon_Closure( The_Destinations, The_NFA_State_Table );
         Add_Transitions_For_Input(My_Input_Transitions, The_Input, My_Closure);
      end Add_By_Input;
   begin 
      Iter(The_Current_Transitions, Add_By_Input'Access);
      
      return My_Input_Transitions;
   end Closurify_Input_Transitions;
   
   -- This function merges two input transition maps by input character
   function Merge( The_Left : NFA_Input_Transitions; The_Right : NFA_Input_Transitions) return NFA_Input_Transitions is 
      use P_NFA_Input_Transitions;
      My_New_Map : NFA_Input_Transitions := Empty;
      procedure Add_Entry(The_Input : Character; The_States : FA_States) is 
      begin
         Add_Transitions_For_Input(My_New_Map, The_Input, The_States);
      end Add_Entry;
   begin
      My_New_Map := Copy(The_Left);
      Iter(The_Right, Add_Entry'Access);
         
      return My_New_Map;
   end Merge;
   
   -- This function takes the inputs that are part of a range, and the states
   -- that are the destination of the range, and converts it into
   -- an input transition map.
   function Convert_Range_To_Input_Transitions
     ( The_Inputs : FA_Inputs; 
       The_States: FA_States) return NFA_Input_Transitions is 
      My_Input_Transitions : NFA_Input_Transitions := Empty;
      procedure Convert(The_Input: Character) is
      begin 
         Add_Transitions_For_Input(My_Input_Transitions, The_Input, The_States);
      end Convert;
   begin 
      Iter( The_Inputs, Convert'Access);
      return My_Input_Transitions;
   end Convert_Range_To_Input_Transitions;
   
   -- This function takes a DFA state consisting of multiple NFA states,
   -- lookups up all the NFA states it can transition to through an input
   -- (range, complement, or regular character), and returns a data structure
   -- suitable for further transformation.
   function Build_Transitions_For_A_State
     (DFA_State : FA_States; 
      The_NFA_State_Table : State_Transitions.Vector) 
      return Transitions_For_State is
      
      My_Input_Transitions: NFA_Input_Transitions := Empty;
      My_Range_Complements: NFA_Range_Complements := Empty;
      
      procedure Build_Transition(The_State : Natural) is
         My_Transitions : Transitions;
         My_Range_Transitions: NFA_Input_Transitions;
      begin 
         My_Transitions := Element(The_NFA_State_Table, The_State);
         
         case My_Transitions.kind is 
            when By_Char =>
               My_Input_Transitions := Merge(My_Input_Transitions, Closurify_Input_Transitions(My_Transitions.input_Transitions, The_NFA_State_Table));
            when By_Range =>
               My_Range_Transitions := Convert_Range_To_Input_Transitions(My_Transitions.range_inputs, My_Transitions.range_states);
               My_Input_Transitions := 
                 Merge(My_Input_Transitions, 
                       Closurify_Input_Transitions(My_Range_Transitions, The_NFA_State_Table));
            when By_Range_Complement =>
               Add_Complement
                 ( My_Range_Complements, 
                   ( complement => My_Transitions.range_inputs, 
                     destinations => Get_Epsilon_Closure(My_Transitions.range_states, The_NFA_State_Table)
                    )
                  );
         end case;
      end Build_Transition;
   begin
      Iter(DFA_State, Build_Transition'Access);
      
      return ( input_transitions => My_Input_Transitions,
               range_complements => My_Range_Complements
              );
   end Build_Transitions_For_A_State;
   
   -- This function merges all the complement inputs together.
   function Merge_Inputs(The_Complements : NFA_Range_Complements) return FA_Inputs is 
      My_Set : FA_Inputs := Inputs.Empty_Set;
         
      procedure Build_Set(The_Complement : NFA_Range_Complement) is 
      begin 
         Inputs.Union(My_Set, The_Complement.complement);
      end Build_Set;
   begin 
      Iter(The_Complements, Build_Set'Access);
      return My_Set;
   end Merge_Inputs;
   
   -- This function maps each element of each complement to the states
   -- it should go to afterward
   function Generate_Complement_Input_Transitions(The_Complements: NFA_Range_Complements) return NFA_Input_Transitions is
      My_Total_Inputs : FA_Inputs := Inputs.Empty_Set;
      My_Complement_Map : NFA_Input_Transitions := Empty;
      
      procedure Build_Map(The_Complement : NFA_Range_Complement) is 
         My_Excluded_Inputs : FA_Inputs := Inputs.Empty_Set;
      begin          
         -- We check which inputs are not in the current complement.
         -- For these inputs, we should go to the destination of the current complement.
         My_Excluded_Inputs := Inputs.Difference(My_Total_Inputs, The_Complement.complement);
         My_Complement_Map := Merge(My_Complement_Map, Convert_Range_To_Input_Transitions(My_Excluded_Inputs, The_Complement.destinations));
      end Build_Map;
   begin
      My_Total_Inputs := Merge_Inputs(The_Complements);
      Iter(The_Complements, Build_Map'Access);
      return My_Complement_Map;
   end Generate_Complement_Input_Transitions;
   
   -- This function returns a set of NFA states: the union of all the complement destinations.
   function Generate_Complement_Destinations(The_Complements : NFA_Range_Complements) return FA_States is 
      My_Transitions : FA_States := P_FA_States.Empty_Set;
      procedure Build_Transition(The_Complement : NFA_Range_Complement) is 
      begin 
         Merge(My_Transitions, The_Complement.destinations);
      end Build_Transition;
   begin 
      Iter(The_Complements, Build_Transition'Access);
      return My_Transitions;
   end Generate_Complement_Destinations;
   
   -- For each input transition, if the input is missing from a complement,
   -- the complement's destinations are added to the input transition.
   function Add_Complement_Destinations
     (The_Input_Transitions : NFA_Input_Transitions;
      The_Complements : NFA_Range_Complements) return NFA_Input_Transitions is
      
      My_Map : NFA_Input_Transitions := Empty;
      procedure Check_Complements(The_Input : Character; The_Element: FA_States) is 
         use P_NFA_Input_Transitions;
         procedure Add_To_Map(The_Complement : NFA_Range_Complement) is 
         begin    
            if not Inputs.Contains(The_Complement.complement, The_Input) then 
               Add_Transitions_For_Input(My_Map, The_Input, The_Complement.destinations);
            end if;
         end Add_To_Map;
      begin         
         Iter(The_Complements, Add_To_Map'Access);
      end Check_Complements;
   begin
      Iter(The_Input_Transitions, Check_Complements'Access);
      return Merge(The_Input_Transitions, My_Map);
   end Add_Complement_Destinations;
   
   function Convert_Complement(The_Transitions : Transitions_For_State) return Complement_Conversion is 
      My_Input_Transitions : NFA_Input_Transitions := Empty;
      My_Complement_Transitions : FA_States := P_FA_States.Empty_Set;
      My_Complement_Input_Map : NFA_Input_Transitions;
   begin 
      
      -- 
      -- For each complement, we examine each complement state in turn. For each of the other complements, if this input
      -- isn't in the other complement, the other complement's epsilon closure gets merged into the input map.
      -- This continues until all the states in the complement have been processed. Once this is done, we know
      -- exactly where each of the inputs that are part of the complements will lead. Then, any input that iS NOT
      -- recognized by the input map must go to the union of all the complements' epsilon closures, which is its 
      -- own separate state.
      My_Complement_Input_Map := Generate_Complement_Input_Transitions(The_Transitions.range_complements);
      
      -- For each input in the input transitions, we check if it is missing from any of the complements. If it is,
      -- we add the complements destinations for that input.
      My_Input_Transitions := Merge
        (My_Complement_Input_Map, 
         Add_Complement_Destinations
           (The_Transitions.input_transitions, The_Transitions.range_complements
           )
        );
      
      -- We also need to include the union of all the complements' destinations, in case none of the inputs
      -- match what is in the input transitions.
      My_Complement_Transitions := Generate_Complement_Destinations(The_Transitions.range_complements);
                
      return ( input_transitions => My_Input_Transitions,
               complement_inputs => Merge_Inputs(The_Transitions.range_complements),
               complement_transitions => My_Complement_Transitions
              );
   end Convert_Complement;
   
   function To_Key(The_State: FA_States) return Unbounded_String is
      My_String : Unbounded_String := To_Unbounded_String("");
      package State_Sorter is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Natural,
         "="          => Standard."=");
      package State_Sorting is new State_Sorter.Generic_Sorting
        ("<" => Standard."<");
      My_Sorter : State_Sorter.Vector;
      
      procedure Build_Sorter(The_Position : P_FA_States.Cursor) is 
      begin 
         State_Sorter.Append(My_Sorter, P_FA_States.Element(The_Position));
      end Build_Sorter;
      
      procedure Build_String(The_Position: State_Sorter.Cursor) is 
      begin
         My_String := My_String & State_Sorter.Element(The_Position)'Image;
      end Build_String;
   begin 
      P_FA_States.Iterate(The_State, Build_Sorter'Access);
      State_Sorting.Sort(My_Sorter);
      State_Sorter.Iterate(My_Sorter, Build_String'Access);
      return My_String;
   end To_Key;
   
   function Is_New_State(Global_Seen_States: in out Seen_States.Map; The_State: FA_States) return Boolean is 
      use Seen_States;
   begin
      return Seen_States.Find(Global_Seen_States, To_Key(The_State)) = Seen_States.No_Element;
   end Is_New_State;
   
   --This function will throw if the state can't be found.
   function Get_State_Number(Global_Seen_States: in out Seen_States.Map; The_State: FA_States) return Natural is 
   begin
      return Seen_States.Element(Seen_States.Find(Global_Seen_States, To_Key(The_State)));
   end Get_State_Number;
   
   procedure Assign_State_Number(Global_Seen_States: in out Seen_States.Map; The_State: FA_States; The_Number: Natural) is 
   begin
      Seen_States.Insert(Global_Seen_States, To_Key(The_State), The_Number);
   end Assign_State_Number;
   
   function Build_DFA_Transitions
     (The_NFA_Transitions : Complement_Conversion; 
      The_Last_Used_State_Number: in out Natural;
      The_NFA_Accepting : FA_States;
      The_DFA_Accepting : in out FA_States;
      The_Queue : in out DFA_States_Queue.List;
      The_Complement_Number: out Natural;
      Global_Seen_States: in out Seen_States.Map
     ) 
      return DFA_Input_Transitions.Map is 
      
      My_DFA_Transitions: DFA_Input_Transitions.Map;
      My_State_Number : Natural;
      procedure Build_Transitions(The_Position : P_NFA_Input_Transitions.Cursor) is 
         My_Input : Character;
         My_State : FA_States;
      begin 
         -- During DFA generation from an NFA, how do make sure that we see each state once,
         -- and only once? For example, a wildcard involves looping back on itself, and can always
         -- loop back on itself. So when we see the state an input leads to, we need to check
         -- if it's been seen before.
         -- If it has, we don't assign a new state number -- we use the old one, and we don't queue it.
         -- If it has not, we assign a new state number and put it into the hash table
         -- for looking up if it's been seen before, and we queue it to be processed later.
         --
         -- What's a sustainable way for determining whether we've seen a state (collection of NFA states) before?
         -- Supose we have the collection of NFA states { 1, 3, 7, 9 } (pre-sorting for clarity). We could turn this into
         -- the string "1379", hash it, and store the corresponding DFA state number in a hash table.
         -- We could also add it up into the number 1379, but here there's a possibility for overflow. Of course,
         -- doing this as a string could lead to really long keys in the hash table. But perhaps that's okay? At least for now.
         
         My_State := P_NFA_Input_Transitions.Element(The_Position);
         My_Input := Key(The_Position);
         
         if Is_New_State(Global_Seen_States, My_State) then 
            My_State_Number := My_State_Number + 1;
            
            -- we've queued up the state that this input goes to.
            Enqueue(The_Queue, (number => My_State_Number, state => My_State));
            DFA_Input_Transitions.Insert(My_DFA_Transitions, My_Input, My_State_Number);
            Assign_State_Number(Global_Seen_States, My_State, My_State_Number);
         
            -- We can also check whether the current set of NFA states has any intersection with 
            -- the set of NFA accepting states. If so, the current state is an accepting state.
            if not P_FA_States.Is_Empty( P_FA_States.Intersection(My_State, The_NFA_Accepting) ) then
               P_FA_States.Insert(The_DFA_Accepting, My_State_Number);
            end if;
         else 
            -- if we've seen this state before, we've analyzed it already.
            -- so we just let the current state transition to it.
            DFA_Input_Transitions.Insert(My_DFA_Transitions, My_Input, Get_State_Number(Global_Seen_States, My_State));
         end if;
         
         
 
      end Build_Transitions;
   begin 
      My_State_Number := The_Last_Used_State_Number;
      
      -- For each input transition, assign a state number, queue it up, and update the accepting states.
      Iterate(The_NFA_Transitions.input_transitions, Build_Transitions'Access);
      
      -- Once we're done processing each input transition, we process the complement transitions, if any.
      -- During DFA generation from an NFA, how do make sure that we see each state once,
         -- and only once? For example, a wildcard involves looping back on itself, and can always
         -- loop back on itself. So when we see the state a complement leads to, we need to check if 
         -- it's been seen before.
         -- If it has, we don't assign a new state number -- we use the old one, and we don't queue it.
         -- If it has not, we assign a new state number and put it into the hash table
         -- for looking up if it's been seen before, and queue this new state to be processed.
      if not Is_Empty(The_NFA_Transitions.complement_transitions) then 
         if Is_New_State(Global_Seen_States, The_NFA_Transitions.complement_transitions) then 
            My_State_Number := My_State_Number + 1;
            Enqueue(The_Queue, (number => My_State_Number, state => The_NFA_Transitions.complement_transitions));
         
            if not P_FA_States.Is_Empty
              ( P_FA_States.Intersection
                  (The_NFA_Transitions.complement_transitions, The_NFA_Accepting) ) then
               P_FA_States.Insert(The_DFA_Accepting, My_State_Number);
            end if;
            The_Complement_Number := My_State_Number;
            Assign_State_Number(Global_Seen_States, The_NFA_Transitions.complement_transitions, My_State_Number);
         else 
            The_Complement_Number := Get_State_Number(Global_Seen_States, The_NFA_Transitions.complement_transitions);
         end if;
      end if;
      
      The_Last_Used_State_Number := My_State_Number;
      return My_DFA_Transitions; 
   end Build_DFA_Transitions;
   
   function Build_DFA_States
     (Global_Seen_States: in out Seen_States.Map;
      The_States_Queue: in out DFA_States_Queue.List;
      The_NFA : NFA;
      The_Start_Is_Accepting: Boolean
     ) return DFA_Conversion is 
      
      My_DFA_States : DFA_States.Vector := DFA_States.Empty_Vector;
      My_State : DFA_State;
      My_Input_Transitions : NFA_Input_Transitions := Empty; -- for this state, we will build of the input transitions
      My_DFA_Transitions : DFA_Input_Transitions.Map := DFA_Input_Transitions.Empty_Map;
      My_State_Number : Natural;
      My_Accepting : FA_States := P_FA_States.Empty_Set;
      My_NFA_Transitions : Transitions_For_State;
      My_NFA_Complement_Conversion : Complement_Conversion;
      My_Complement_State_Number : Natural;
   begin 
      My_State_Number := 0; -- initially the last known state number is 0, since the queue starts with one thing in it.
      while Dequeue(The_States_Queue, My_State) loop
         -- Note : The 0th state never gets assessed for having one of the accepting states.
         My_NFA_Transitions := Build_Transitions_For_A_State(My_State.state, The_NFA.states);
         My_Input_Transitions := My_NFA_Transitions.input_transitions;
            
    
         My_NFA_Complement_Conversion := Convert_Complement(My_NFA_Transitions);
         
         -- This line may or may not add more states to the queue.
         -- It will also add to the set of DFA accepting states and increment the My_State_Number based on 
         -- how many additional states were added.
         My_DFA_Transitions := Build_DFA_Transitions
           (My_NFA_Complement_Conversion, 
            My_State_Number, 
            The_NFA.accepting, 
            My_Accepting, 
            The_States_Queue,
            My_Complement_State_Number,
            Global_Seen_States
           );            
            
         -- Once we have the DFA transition map, we can finally insert the DFA map into the DFA states table.
         if Is_Empty(My_NFA_Complement_Conversion.complement_transitions) then 
            DFA_States.Append
              (My_DFA_States, 
               ( input_transitions => My_DFA_Transitions,
                 has_complement => False,
                 complement_inputs => Inputs.Empty_Set,
                 complement_transition => 0
                )
              );
         else 
            -- If there was a complement, it was assigned a state number.
            DFA_States.Append
              (My_DFA_States, 
               ( input_transitions => My_DFA_Transitions,
                 has_complement => True,
                 complement_inputs => My_NFA_Complement_Conversion.complement_inputs,
                 complement_transition => My_Complement_State_Number
                )
              );
         end if;
         
      end loop;
      
      if The_Start_Is_Accepting then 
         P_FA_States.Union(My_Accepting, P_FA_States.To_Set(0));
      end if;
      
      return (
              states => My_DFA_States,
              accepting => My_Accepting
                );
   end Build_DFA_States;

   function DFA_States_From_NFA(The_NFA : NFA) return DFA_Conversion is 
      My_Start_State : FA_States;
      My_DFA_States : DFA_States.Vector := DFA_States.Empty_Vector ;
      My_Queue : DFA_States_Queue.List := DFA_States_Queue.Empty_List;
      My_State_Number : Natural := 0;
      Global_Seen_States : Seen_States.Map := Seen_States.Empty_Map;
      Start_Is_Accepting: Boolean;
   begin
      -- We would like to set this up as a processing of a queue of DFA states.
      -- 1. Enqueue Start States (with the state they correspond with)
      -- 2. Begin subprocess until queue is empty
      -- a. Dequeue
      -- b. Iterate over the NFA states, checking the input transitions of each for the epsilon closure
      --      , and using that to build
      --      a giant state per same input character to insert as the transition for the DFA state.
      --      In fact, we can accumulate this giant state within one of the NFA primitive types.
      --      Once done iterating over the NFA states, we will have a hash map from inputs to 
      --      Epsilon closures of the next states. We iterate over this hash map, assigning each
      --      input-next_states paid a state number. Then 
      --      that same giant state should be inserted into the queue with its state number.
      --      and the process repeats.
      -- c. We also need to check whether any of the sets we generate is in the set of 
      --      accepting states for the NFA states.
      
      
      -- The first state of the DFA corresponds to the epsilon closure of the start states of 
      -- the NFA.
      My_Start_State := Get_Epsilon_Closure( P_FA_States.To_Set(The_NFA.start), The_NFA.states);
      
      Enqueue(My_Queue, ( number => My_State_Number, state => My_Start_State));
      Assign_State_Number(Global_Seen_States, My_Start_State, My_State_Number);
      
      Start_Is_Accepting := not P_FA_States.Is_Empty( P_FA_States.Intersection(The_NFA.accepting, My_Start_State) );
      
      return Build_DFA_States (Global_Seen_States, My_Queue, The_NFA, Start_Is_Accepting);
   end DFA_States_From_NFA;
   
   function NFA_To_DFA(The_NFA : NFA) return DFA is 
      My_Conversion : DFA_Conversion;
   begin 
      
      
      -- A DFA is different. Each state of the DFA represents
      -- a set of states from the NFA. A single transition to another state
      -- simply represents a transition to another states from the NFA.
      -- So, to build a DFA, we need the following:
      -- 1. Start State, representing start states of the NFA
      -- 2. Accepting states, representing the subsets of all the NFA states
      --    that contain an accepting state
      -- 3. State transitions, which take us from a set of NFA states to 
      --    the next set based on the input PLUS epsilon transitions.
      
      -- This is the start of converting, so we need to reset globals.
      
      My_Conversion := DFA_States_From_NFA(The_NFA);
      return (
              start => 0,
              states => My_Conversion.states,
              accepting => My_Conversion.accepting
              );
   end NFA_To_DFA;

end Code_Gen_DFAs;
