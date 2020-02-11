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
   use NFA_Input_Transitions;
   use NFA_States;
   
   type DFA_Conversion is record 
      states : DFA_States.Vector;
      accepting: NFA_States.Set;
   end record;
   
   function Build_Input_Transitions
     ( The_Current_Transitions: NFA_Input_Transitions.Map;
       The_NFA_State_Table : State_Transitions.Vector
      ) return NFA_Input_Transitions.Map is
      
      My_Input_Transitions: NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      procedure Add_By_Input(The_Position: NFA_Input_Transitions.Cursor) is 
         My_Input : Character;
         My_Closure : NFA_States.Set;
         My_Old_Closure: NFA_States.Set;
      begin 
         My_Input := Key(The_Position);
         My_Closure := Get_Epsilon_Closure( Element(The_Position), The_NFA_State_Table );
               
         if NFA_Input_Transitions.Find(My_Input_Transitions, My_Input) = NFA_Input_Transitions.No_Element then 
            NFA_Input_Transitions.Insert(My_Input_Transitions, My_Input, My_Closure);
         else
            -- We replace with the union
            My_Old_Closure := Element(My_Input_Transitions, My_Input);
            NFA_Input_Transitions.Replace(My_Input_Transitions, My_Input, Union(My_Closure, My_Old_Closure)); 
         end if;
               
         -- once we're done with this procedure, we've augmented the known set that the DFA can reach from the current state
         -- using the given input pointed at by The_Position.
      end Add_By_Input;
   begin 
      Iterate(The_Current_Transitions, Add_By_Input'Access);
      
      return My_Input_Transitions;
   
   end Build_Input_Transitions;
   
   function Merge( The_Left : NFA_Input_Transitions.Map; The_Right : NFA_Input_Transitions.Map) return NFA_Input_Transitions.Map is 
      My_New_Map : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
         
      procedure Add_Entry(The_Position : NFA_Input_Transitions.Cursor) is 
         My_Input : Character;
         My_New_Entry : NFA_States.Set;
      begin
         My_Input := NFA_Input_Transitions.Key(The_Position);
         
         
         if NFA_Input_Transitions.Find(My_New_Map, My_Input) = NFA_Input_Transitions.No_Element then
            NFA_Input_Transitions.Insert(My_New_Map, My_Input, NFA_Input_Transitions.Element(The_Position));
         else
            My_New_Entry := NFA_States.Union
              ( NFA_Input_Transitions.Element(The_Left, My_Input),
                NFA_Input_Transitions.Element(The_Position));
                                                         
            NFA_Input_Transitions.Replace(My_New_Map, My_Input, My_New_Entry); 
         end if;
            
      end Add_Entry;
   begin
      My_New_Map := NFA_Input_Transitions.Copy(The_Left);
         
      Iterate(The_Right, Add_Entry'Access);
         
      return My_New_Map;
   end Merge;
   
   function Convert_Range_Transitions
     ( The_Inputs : Inputs.Set; 
       The_States: NFA_States.Set) return NFA_Input_Transitions.Map is 
      
      My_Input_Transitions : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      procedure Convert(The_Position: Inputs.Cursor) is
         My_Input : Character;
      begin 
         My_Input := Inputs.Element(The_Position);
         NFA_Input_Transitions.Insert(My_Input_Transitions, My_Input, The_States);
      end Convert;
   begin 
      Inputs.Iterate( The_Inputs, Convert'Access);
      
      return My_Input_Transitions;
   end Convert_Range_Transitions;
   
   type NFA_Range_Complement is record 
      complement: Inputs.Set;
      destinations: NFA_States.Set;
   end record;
   
   package NFA_Range_Complements is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => NFA_Range_Complement
         );
   
   type Transitions_For_State is record 
      input_transitions : NFA_Input_Transitions.Map;
      range_complements: NFA_Range_Complements.Vector;
   end record;
   
   
   function Build_Transitions_For_A_State
     (DFA_State : NFA_States.Set; 
      The_NFA_State_Table : State_Transitions.Vector) 
      return Transitions_For_State is
      
      My_Input_Transitions: NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      My_Range_Complements: NFA_Range_Complements.Vector := NFA_Range_Complements.Empty_Vector;
      
      procedure Build_Transition(The_Position : NFA_States.Cursor) is
         My_NFA_State : Natural;
         My_Transitions : Transitions;
         My_Range_Transitions: NFA_Input_Transitions.Map;
      begin 
         My_NFA_State := Element(The_Position);
         My_Transitions := Element(The_NFA_State_Table, My_NFA_State);
         
         case My_Transitions.kind is 
            when By_Char =>
               My_Input_Transitions := Merge(My_Input_Transitions, Build_Input_Transitions(My_Transitions.input_Transitions, The_NFA_State_Table));
            when By_Range =>
               My_Range_Transitions := Convert_Range_Transitions(My_Transitions.range_inputs, My_Transitions.range_states);
               My_Input_Transitions := 
                 Merge(My_Input_Transitions, 
                       Build_Input_Transitions(My_Range_Transitions, The_NFA_State_Table));
            when By_Range_Complement =>
               NFA_Range_Complements.Append
                 ( My_Range_Complements, 
                   ( complement => My_Transitions.range_inputs, 
                     destinations => Get_Epsilon_Closure(My_Transitions.range_states, The_NFA_State_Table)
                    )
                  );
         end case;
           
            
         -- once we're done with this procedure, we've taken the current NFA state and, for all its 
         -- valid input transitions, we've tossed all the epsilon closures into our tracking map.
      end Build_Transition;
   begin
      Iterate(DFA_State, Build_Transition'Access);
      
      return ( input_transitions => My_Input_Transitions,
               range_complements => My_Range_Complements
              );
   end Build_Transitions_For_A_State;
   
   type Complement_Conversion is record
      input_transitions : NFA_Input_Transitions.Map;
      complement_inputs : Inputs.Set;
      complement_transitions: NFA_States.Set;
   end record;
   
   function Merge_Inputs(The_Complements : NFA_Range_Complements.Vector) return Inputs.Set is 
      My_Set : Inputs.Set := Inputs.Empty_Set;
         
      procedure Build_Set(The_Position : NFA_Range_Complements.Cursor) is 
         My_Complement : NFA_Range_Complement;
      begin 
         My_Complement := NFA_Range_Complements.Element(The_Position);
         Inputs.Union(My_Set, My_Complement.complement);
      end Build_Set;
   begin 
      NFA_Range_Complements.Iterate(The_Complements, Build_Set'Access);
      return My_Set;
   end Merge_Inputs;
   
   -- given a set of complements, this function maps each element of each complement to the states
   -- it should go to afterward
   function Generate_Complement_Map(The_Complements: NFA_Range_Complements.Vector) return NFA_Input_Transitions.Map is
      My_Total_Inputs : Inputs.Set := Inputs.Empty_Set;
      My_Complement_Map : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      
      function Gen_Map(The_Inputs : Inputs.Set; The_States : NFA_States.Set) return NFA_Input_Transitions.Map is 
         My_Map : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
         
         procedure Add_Input(The_Position : Inputs.Cursor) is 
            My_Input : Character;
         begin
            My_Input := Inputs.Element(The_Position);
            NFA_Input_Transitions.Insert(My_Map, My_Input, The_States);
         end Add_Input;
      begin 
         Inputs.Iterate(The_Inputs, Add_Input'Access);
         
         return My_Map;
      end Gen_Map;
      
      procedure Build_Map(The_Position: NFA_Range_Complements.Cursor) is 
         My_Complement : NFA_Range_Complement;
         My_Excluded_Inputs : Inputs.Set := Inputs.Empty_Set;
      begin 
         My_Complement := NFA_Range_Complements.Element(The_Position);
         
         -- We check which inputs are not in the current complement.
         -- For these inputs, we should go to the destination of the current complement. Makes sense, right?
         My_Excluded_Inputs := Inputs.Difference(My_Total_Inputs, My_Complement.complement);
         My_Complement_Map := Merge(My_Complement_Map, Gen_Map(My_Excluded_Inputs, My_Complement.destinations));
         
      end Build_Map;
   begin
      My_Total_Inputs := Merge_Inputs(The_Complements);
      
      NFA_Range_Complements.Iterate(The_Complements, Build_Map'Access);
      
      -- We return a map showing how all the known inputs where one of the inputs is part of one of 
      -- the complement descriptions, goes to a new state.
      return My_Complement_Map;
   end Generate_Complement_Map;
   
   -- Returns a set of NFA states: the union of all the complement destinations.
   function Generate_Complement_Transitions(The_Complements : NFA_Range_Complements.Vector) return NFA_States.Set is 
      My_Transitions : NFA_States.Set := NFA_States.Empty_Set;
      procedure Build_Transition(The_Position: NFA_Range_Complements.Cursor) is 
         My_Complement : NFA_Range_Complement;
      begin 
         My_Complement := NFA_Range_Complements.Element(The_Position);
         NFA_States.Union(My_Transitions, My_Complement.destinations);
      end Build_Transition;
   begin 
      NFA_Range_Complements.Iterate(The_Complements, Build_Transition'Access);
      return My_Transitions;
   end Generate_Complement_Transitions;
   
   -- For each input transition, if the input is missing from a complement,
   -- the complement's destinations are added to the input transition.
   function Add_Complement_Destinations
     (The_Input_Transitions : NFA_Input_Transitions.Map;
      The_Complements : NFA_Range_Complements.Vector) return NFA_Input_Transitions.Map is
      
      My_Map : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      procedure Check_Complements(The_Position: NFA_Input_Transitions.Cursor) is 
         use NFA_Input_Transitions;
         My_Input : Character;
         procedure Add_To_Map(The_Position: NFA_Range_Complements.Cursor) is 
            My_Complement: NFA_Range_Complement;
            My_Replacement : NFA_States.Set;
            My_Position : NFA_Input_Transitions.Cursor;
         begin 
            My_Complement := NFA_Range_Complements.Element(The_Position);
            
            if not Inputs.Contains(My_Complement.complement, My_Input) then 
               My_Position := Find(My_Map, My_Input);
               if My_Position = NFA_Input_Transitions.No_Element then 
                  Insert(My_Map, My_Input, My_Complement.destinations);
               else 
                  My_Replacement := NFA_States.Union(My_Complement.destinations, Element(My_Position));
                  NFA_Input_Transitions.Replace_Element(My_Map, My_Position, My_Replacement);
               end if;
            end if;
         end Add_To_Map;
      begin
         My_Input := NFA_Input_Transitions.Key(The_Position);
         
         NFA_Range_Complements.Iterate(The_Complements, Add_To_Map'Access);
      end Check_Complements;
   begin
      NFA_Input_Transitions.Iterate(The_Input_Transitions, Check_Complements'Access);
      return Merge(The_Input_Transitions, My_Map);
   end Add_Complement_Destinations;
   
   function Convert_Complement(The_Transitions : Transitions_For_State) return Complement_Conversion is 
      My_Input_Transitions : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map;
      My_Complement_Transitions : NFA_States.Set := NFA_States.Empty_Set;
      My_Complement_Input_Map : NFA_Input_Transitions.Map;
   begin 
      
      -- 
      -- For each complement, we examine each complement state in turn. For each of the other complements, if this input
      -- isn't in the other complement, the other complement's epsilon closure gets merged into the input map.
      -- This continues until all the states in the complement have been processed. Once this is done, we know
      -- exactly where each of the inputs that are part of the complements will lead. Then, any input that iS NOT
      -- recognized by the input map must go to the union of all the complements' epsilon closures, which is its 
      -- own separate state.
      My_Complement_Input_Map := Generate_Complement_Map(The_Transitions.range_complements);
      
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
      My_Complement_Transitions := Generate_Complement_Transitions(The_Transitions.range_complements);
                
      return ( input_transitions => My_Input_Transitions,
               complement_inputs => Merge_Inputs(The_Transitions.range_complements),
               complement_transitions => My_Complement_Transitions
              );
   end Convert_Complement;
   
   function Build_DFA_Transitions
     (The_NFA_Transitions : Complement_Conversion; 
      The_Last_Used_State_Number: in out Natural;
      The_NFA_Accepting : NFA_States.Set;
      The_DFA_Accepting : in out NFA_States.Set;
      The_Queue : in out DFA_States_Queue.List
     ) 
      return DFA_Input_Transitions.Map is 
      
      My_DFA_Transitions: DFA_Input_Transitions.Map;
      My_State_Number : Natural;
      procedure Build_Transitions(The_Position : NFA_Input_Transitions.Cursor) is 
         My_Input : Character;
         My_State : NFA_States.Set;
      begin 
         My_State_Number := My_State_Number + 1;
         My_Input := Key(The_Position);
         My_State := NFA_Input_Transitions.Element(The_Position);
            
         -- we've queued up the state that this input goes to.
         Enqueue(The_Queue, (number => My_State_Number, state => My_State));
         DFA_Input_Transitions.Insert(My_DFA_Transitions, My_Input, My_State_Number);
         
         -- We can also check whether the current set of NFA states has any intersection with 
         -- the set of NFA accepting states. If so, the current state is an accepting state.
         if not NFA_States.Is_Empty( NFA_States.Intersection(My_State, The_NFA_Accepting) ) then
            NFA_States.Insert(The_DFA_Accepting, My_State_Number);
         end if;
 
      end Build_Transitions;
   begin 
      My_State_Number := The_Last_Used_State_Number;
      
      -- For each input transition, assign a state number, queue it up, and update the accepting states.
      Iterate(The_NFA_Transitions.input_transitions, Build_Transitions'Access);
      
      -- Once we're done processing each input transition, we process the complement transitions, if any.
      if not Is_Empty(The_NFA_Transitions.complement_transitions) then 
         My_State_Number := My_State_Number + 1;
         Enqueue(The_Queue, (number => My_State_Number, state => The_NFA_Transitions.complement_transitions));
         
         if not NFA_States.Is_Empty
           ( NFA_States.Intersection
               (The_NFA_Transitions.complement_transitions, The_NFA_Accepting) ) then
            NFA_States.Insert(The_DFA_Accepting, My_State_Number);
         end if;
      end if;
      
      The_Last_Used_State_Number := My_State_Number;
      return My_DFA_Transitions; 
   end Build_DFA_Transitions;
   
   function Build_DFA_States
     (The_States_Queue: in out DFA_States_Queue.List;
      The_NFA : NFA
     ) return DFA_Conversion is 
      
      My_DFA_States : DFA_States.Vector := DFA_States.Empty_Vector;
      My_State : DFA_State;
      My_Input_Transitions : NFA_Input_Transitions.Map := NFA_Input_Transitions.Empty_Map; -- for this state, we will build of the input transitions
      My_DFA_Transitions : DFA_Input_Transitions.Map := DFA_Input_Transitions.Empty_Map;
      My_State_Number : Natural;
      My_Accepting : NFA_States.Set := NFA_States.Empty_Set;
      My_NFA_Transitions : Transitions_For_State;
      My_NFA_Complement_Conversion : Complement_Conversion;
   begin 
      My_State_Number := 0; -- initially the last known state number is 0, since the queue starts with one thing in it.
      while Dequeue(The_States_Queue, My_State) loop
         Put_Line("dequeued: " & As_String(My_State.state) & ", which is state " & My_State.number'Image);
         My_NFA_Transitions := Build_Transitions_For_A_State(My_State.state, The_NFA.states);
         My_Input_Transitions := My_NFA_Transitions.input_transitions;
         Put_Line("transitions : " & As_String(My_Input_Transitions));
            
         -- At this point, for the given DFA state we just dequeued, we have 
         -- built a giant map from each input to the set of all NFA states that we could reach
         -- using this DFA state. 
         -- 
         -- However, this does not handle ranges or range complements, and doesn't fit with how
         -- the recognize function works. We can handle the range easily -- we'll simply toss
         -- each character in the range as an input that goes to the range's next NFA state.
         -- 
         -- But this does not handle range complements. A simple way to illustrate the problem is with the following union:
         --     ab|[^c]d|[^d]g
         -- But we don't want to waste precious memory space by tossing the entire unicode character
         -- set into an input transitions table (although we could, the unicode character space is at present 137,000
         -- characters, so at least 100 KB per range in the regex, which seems a terrible waste). But remember that we 
         -- only process one DFA state at a time. Theoretically, if one or more complement state machine were in the set 
         -- of NFA states, we could store it and the states it went to -- but separately from the regular input
         -- transitions. Then the question would be how to combine the range transitions and the input transitions.
         -- Take the example above. The initial DFA state consists of the states at the start of ab, [^c]d, [^d]i, [^e]g.
         --   On input a, we we would like to go to a set of states at the start of b, d, i, and g
         --   On input c, we could like to go to a set of states at the start of i and g
         --   On input d, we would like to go to a set of states at the start of d and g
         --   On input e, we would like to go to a set of states at the start of d and i
         --   On input z, we would like to to to a set of states at the start of d, i, and g.
         -- Presumably, after processing from the initial state, we would have the following:
         --   A map with one entry, from "a" to the epsilon closure of consuming the "a" input.
         --   A vector with two entries, 
         --       one containing c and the epsilon closure of consuming anything but c;
         --       one containing d and the epsilon closure of consuming anything but d;
         -- For every input in the map, we check whether the input is in any of the vector entries. If NOT,
         -- we merge the vector entry's epsilon closure into the input map (This means that if the input
         -- is in the input map, we don't need to consider whether the input is missing from the complements). 
         --
         -- Then we take the union of all the complements' epsilon closures. We'll use this later.
         -- 
         -- For each complement, we examine each input in turn. For each of the other complements, if this input
         -- isn't in the other complement, the other complement's epsilon closure gets merged into the input map.
         -- This continues until all the inputs in the complement have been processed. Once this is done, we know
         -- exactly where each of the inputs that are part of the complements will lead. Then, any input that iS NOT
         -- recognized by the input map must go to the union of all the complements' epsilon closures, which is its 
         -- own separate state.
         --
         --
         -- We now need to iterate over this map, and insert element each as a 
         -- new state in the queue, and convert the map into a DFA transition map
            
         
         My_NFA_Complement_Conversion := Convert_Complement(My_NFA_Transitions);
         
         -- This line may or may not add more states to the queue.
         -- It will also add to the set of DFA accepting states and increment the My_State_Number based on 
         -- how many additional states were added.
         My_DFA_Transitions := Build_DFA_Transitions
           (My_NFA_Complement_Conversion, 
            My_State_Number, 
            The_NFA.accepting, 
            My_Accepting, 
            The_States_Queue
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
            DFA_States.Append
              (My_DFA_States, 
               ( input_transitions => My_DFA_Transitions,
                 has_complement => True,
                 complement_inputs => My_NFA_Complement_Conversion.complement_inputs,
                 complement_transition => My_State_Number
                )
              );
         end if;
                  
         -- During DFA generation from an NFA, how do make sure that we see each state once,
         -- and only once. For example, a wildcard involves looping back on itself, and can always
         -- loop back on itself. on an input. Are we detecting that? No we are not.
         
      end loop;
      
      return (
              states => My_DFA_States,
              accepting => My_Accepting
                );
   end Build_DFA_States;

   function DFA_States_From_NFA(The_NFA : NFA) return DFA_Conversion is 
      My_Start_State : NFA_States.Set;
      My_DFA_States : DFA_States.Vector := DFA_States.Empty_Vector ;
      My_Queue : DFA_States_Queue.List := DFA_States_Queue.Empty_List;
      My_State_Number : Natural := 0;
   
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
      My_Start_State := Get_Epsilon_Closure( NFA_States.To_Set(The_NFA.start), The_NFA.states);
      Put_Line("start state: " & As_String(My_Start_State));
      
      Enqueue(My_Queue, ( number => My_State_Number, state => My_Start_State));
      
      return Build_DFA_States (My_Queue, The_NFA);
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
      
      My_Conversion := DFA_States_From_NFA(The_NFA);
      Put_Line("accepting states are " & As_String(My_Conversion.accepting));
      return (
              start => 0,
              states => My_Conversion.states,
              accepting => My_Conversion.accepting
              );
   end NFA_To_DFA;

end Code_Gen_DFAs;
