with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Parser;
with Ada.Text_IO; use Ada.Text_IO;


package body Code_Gen is
   function Gen_NFA(p_cursor: Regex_AST.Cursor) return NFA;
   use State_To_Input_Map;
   use Input_To_State;
   use State_Set;
   
   function Join(first : State_To_Input_Map.Vector; second: State_To_Input_Map.Vector) return State_To_Input_Map.Vector is 
      v_vector: State_To_Input_Map.Vector;
   begin 
      v_vector := Empty_Vector;
      Append(v_vector, first);
      Append(v_vector, second);
      return v_vector;
   end Join;
   
   function As_String(states: Set) return String is 
      str : Unbounded_String := To_Unbounded_String("");
      procedure Accumulate(Position : State_Set.Cursor) is 
      begin
         str := str & Element(Position)'Image;
      end Accumulate;
   begin
      Iterate(states, Accumulate'Access);
      return To_String(str);
   end As_String;
   
   function Empty_Transitions return Transitions is 
      
   begin
      return ( input_transitions => Empty_Map,
               epsilon_transitions => Empty_Set
              );
   end Empty_Transitions; 
     
   
   function Equal_Transitions(Left, Right : Transitions) return Boolean is 
   begin 
      return Left.input_transitions = Right.input_transitions and then Left.epsilon_transitions = Right.epsilon_transitions;
   end Equal_Transitions;
   
   function Has_Transitions_For_Input(trans: Transitions; c: Character) return Boolean is 
   begin 
      return (not Is_Empty(trans.epsilon_transitions)) or else Find(trans.input_transitions, c) /= Input_To_State.No_Element;
   end Has_Transitions_For_Input;
   
   
   function Get_Epsilon_Closure(current_states: Set; state_table: Vector) return Set is 
      v_states : Set := Empty_Set;
      v_new_states: Set := Empty_Set;
      procedure Get_Epsilon_States(Position: State_Set.Cursor) is 
         v_state : Natural;
         v_further_states : Set;
      begin
         v_state := Element(Position);
         
         -- Look at the epsilon states, but just the new ones that aren't in the existing states
         -- We only dig deeper if we find new states.
         v_new_states := Difference(Element(state_table, v_state).epsilon_transitions, v_states);
         if not Is_Empty(v_new_states) then
            -- add the old states, and then explore deeper
            Union(v_new_states, v_states);
            v_further_states := Get_Epsilon_Closure(v_new_states, state_table);
            
            Union(v_new_states, v_further_states);
         else 
            Union(v_new_states, v_states);
         end if;       
      end Get_Epsilon_States;
   begin
      v_states := Copy(current_states);
      Iterate(v_states, Get_Epsilon_States'Access);
      return Copy(v_new_states);
   end Get_Epsilon_Closure;
   
   -- walks through all the one-character transitions, as well as the entire epsilon-closure
   function Get_New_States_On_Input(trans: Transitions; states: Vector; c: Character) return Set is 
      v_states : Set := Empty_Set;
   begin 
      -- First get direct input transitions
      if Find(trans.input_transitions, c) /= Input_To_State.No_Element then 
         Union(v_states, Element(trans.input_transitions, c));
      end if;
      
      -- Then get epsilon closure of these new states.
      v_states := Get_Epsilon_Closure(v_states, states);
      
      return v_states;
   end Get_New_States_On_Input;

   
   function Charac_Hash(Key: Character) return Ada.Containers.Hash_Type is 
   begin 
      return Ada.Strings.Hash((1 => Key));
   end Charac_Hash;
   
   function Equiv_Keys (Left, Right: Character) return Boolean is 
   begin
      return Left = Right;
   end Equiv_Keys;
   
   function Natural_Hash(El: Natural) return Ada.Containers.Hash_Type is 
      
   begin 
      return Ada.Containers.Hash_Type'Mod(El);
   end Natural_Hash;
   
   function Get_New_States(states : Set; p_transitions: Vector; input: Character) return Set is 
      v_new_states : Set := Empty_Set;
      
      procedure Add_New_States(Position: State_Set.Cursor) is 
         v_current_state : Natural;
         v_current_transitions: Transitions;
      begin
         v_current_state := Element(Position);
         v_current_transitions := Element(p_transitions, v_current_state);
         Union(v_new_states, Get_New_States_On_Input(v_current_transitions, p_transitions, input));
      end Add_New_States;
   begin
      Iterate(states, Add_New_States'Access);
      return v_new_states;
      
   end Get_New_States;
   
   function Recognize(machine: NFA; input: Unbounded_String) return Boolean is 
      c : Character;
      transition : Transitions; -- need a vector of transitions, one for each state we're in.
      current_states : Set;
      new_states: Set;
      initial_states: Set;
   begin 
      -- for every character in the input,
      -- check the current state to get the transition functions, and 
      -- check what the next state is.
      initial_states := To_Set(machine.start);
      current_states := Get_Epsilon_Closure(initial_states, machine.states);
      for I in 1..Length(input) loop
         c := Element(input, I);
         Put_Line("read character " & c);
         
         -- for each state in the current states, get the new states and append to the new states.
         Put_Line("current states are " & As_String(current_states));
         new_states := Empty_Set;
         new_states := Get_New_States(current_states, machine.states, c);
         Put_Line("new states are " & As_String(new_states));
         
         -- if the new_states are empty, we could not transition on the input at all
         if Is_Empty(new_states) then 
            Put_Line("Failed because could not find more new states with current state set as " & As_String(current_states));
            return False;
         else 
            current_states := Copy(new_states);
         end if;
         
      end loop;
      
      -- We consumed all the input, so the successful 
      -- run of the machine determines success.
      return not Is_Empty(Intersection(machine.accepting, current_states));
   end Recognize;
   
   function Increment_By(The_Set : Set; The_Increment: Positive) return Set is 
      A_Set : Set := Empty_Set;
      procedure Do_Increment(Position: State_Set.Cursor) is 
         A_State : Natural;
      begin
         A_State := Element(Position);
         Insert(A_Set, A_State + Natural(The_Increment)); 
      end Do_Increment;
   begin
      Iterate(The_Set, Do_Increment'Access);
      return A_Set;
   end Increment_By;
   
   function Increment_All_Input_Transitions_By(transitions: Input_To_State.Map; increment: Positive) return Input_To_State.Map is 
      new_map: Map := Empty_Map;
      procedure Do_Increment(Position: Input_To_State.Cursor) is 
         v_states : Set;
      begin
         v_states := Increment_By(Element(Position), increment);
         Insert(new_map, Key(Position), v_states);
      end Do_Increment;
   begin 
      Iterate(transitions, Do_Increment'Access);
      return new_map;   
   end Increment_All_Input_Transitions_By;
   
   function Increment_All_Epsilon_Transitions_By(transitions: State_Set.Set; increment: Positive) return State_Set.Set is 
      new_states : Set := Empty_Set;
   begin
      new_states := Increment_By(transitions, increment);
      return new_states;
   end Increment_All_Epsilon_Transitions_By;
   
   function Increment_All_Transitions_By(state_table: Vector; increment: Positive) return Vector is 
      new_table: Vector := Empty_Vector;
      procedure Do_Increment(Position: State_To_Input_Map.Cursor) is 
         v_transition: Transitions;
         v_new_transition: Transitions;
      begin
         v_transition := Element(Position);
         v_new_transition := ( input_transitions => Increment_All_Input_Transitions_By(v_transition.input_transitions, increment),
                               epsilon_transitions => Increment_All_Epsilon_Transitions_By(v_transition.epsilon_transitions, increment)
                              );
         Append(new_table, v_new_transition);
      end Do_Increment;
   begin 
      Iterate(state_table, Do_Increment'Access);
      return new_table;
   end Increment_All_Transitions_By;
   
   function Update_States_To_Epsilon_Transition_To(state_table: Vector; states: Set; new_state: Natural) return Vector is 
      v_new_state_table : Vector := Empty_Vector;
      procedure Update_Epsilon_Transition(Position: State_Set.Cursor) is 
         v_state : Natural;
         v_transitions : Transitions;
         v_set : Set := Empty_Set;
      begin 
         v_state := Element(Position);
         v_set := Copy(Element(v_new_state_table, v_state).epsilon_transitions);
         Insert(v_set, new_state);
         v_transitions := ( input_transitions => Copy(Element(v_new_state_table, v_state).input_transitions),
                            epsilon_transitions => v_set
                           );
         Replace_Element(v_new_state_table, v_state, v_transitions);
      end Update_Epsilon_Transition;
   begin 
      v_new_state_table := Copy(state_table);
      Iterate(states, Update_Epsilon_Transition'Access);
      return v_new_state_table;
   end Update_States_To_Epsilon_Transition_To;

   function Gen_Character(tok : Abstract_Syntax_Token; p_cursor: Regex_AST.Cursor) return NFA is 
      v_map : Map;
      v_transitions: Transitions;
   begin
      v_map := Empty_Map;
      -- for this nfa, a single character is enough.
      Insert(v_map, Element(tok.f_lexeme, Length(tok.f_lexeme)), To_Set(1));
      v_transitions := ( input_transitions => v_map,
                         epsilon_transitions => Empty_Set                        
                        );
      return (
              start => 0,
              states => Empty_Vector &
                v_transitions & -- map from start state to end state
                Empty_Transitions, -- no transitions from the accepting state.
              accepting => To_Set(1) -- says that accepting states includes State 1       
             );
   end Gen_Character;
   
   function Gen_Concat(tok : Abstract_Syntax_Token; p_cursor: Regex_AST.Cursor) return NFA is 
      v_map : Map;
      v_first_child : Regex_AST.Cursor;
      v_second_child : Regex_AST.Cursor;
      v_first_nfa: NFA;
      v_second_nfa: NFA;
      v_second_nfa_new_start : Natural;
      v_updated_first_nfa_states : Vector;
   begin
      -- Need to grab NFAs made from first two children and then combine them into one NFA.
      v_first_child := First_Child(p_cursor);
      v_second_child := Next_Sibling(v_first_child);
      
      if v_first_child /= Regex_AST.No_Element and then v_second_child /= Regex_AST.No_Element then 
         v_first_nfa := Gen_NFA(v_first_child);
         v_second_nfa := Gen_NFA(v_second_child);
         
         -- at this point, the first nfa's accepting state transitions need to be modified to 
         -- have an epsilon transition to the starting state of the second nfa.
         v_second_nfa_new_start := Natural(Length(v_first_nfa.states));
         v_updated_first_nfa_states := Update_States_To_Epsilon_Transition_To
           (v_first_nfa.states, v_first_nfa.accepting, v_second_nfa_new_start);
         
         return (
                 start => 0, -- we start at start of first nfa
                 states => Join(v_updated_first_nfa_states, Increment_All_Transitions_By(v_second_nfa.states, v_second_nfa_new_start)),
                 accepting => Increment_By(v_second_nfa.accepting, v_second_nfa_new_start)
                 );
      else
         raise Invalid_Subtree with "Concat subtree did not have two subtrees";
      end if;
   end Gen_Concat;
   
   function Gen_Union(tok : Abstract_Syntax_Token; p_cursor: Regex_AST.Cursor) return NFA is 
      v_map : Map;
      v_first_child : Regex_AST.Cursor;
      v_second_child : Regex_AST.Cursor;
      v_first_nfa: NFA;
      v_second_nfa: NFA;
      v_first_nfa_new_start: Natural;
      v_second_nfa_new_start : Natural;
      v_new_start_transitions : Transitions;
      v_new_start_epsilon_transitions : Set := Empty_Set;
   begin
      -- Need to grab NFAs made from first two children and then combine them into one NFA.
      v_first_child := First_Child(p_cursor);
      v_second_child := Next_Sibling(v_first_child);
      
      if v_first_child /= Regex_AST.No_Element and then v_second_child /= Regex_AST.No_Element then 
         v_first_nfa := Gen_NFA(v_first_child);
         v_second_nfa := Gen_NFA(v_second_child);
         
         -- at this point, we add a new start state 0 that has epsilon transitions to 
         -- the start states of the two child NFAs.
         v_first_nfa_new_start := 1;
         v_second_nfa_new_start := 1 + Natural(Length(v_first_nfa.states));
         Insert(v_new_start_epsilon_transitions, v_first_nfa_new_start);
         Insert(v_new_start_epsilon_transitions, v_second_nfa_new_start);
         v_new_start_transitions :=
         ( input_transitions => Empty_Map, -- there are no input transitions
           epsilon_transitions => v_new_start_epsilon_transitions
          );
         
         return (
                 start => 0, -- we start at start of first nfa
                 states => Join(To_Vector(v_new_start_transitions, 1), 
                   Join(
                     Increment_All_Transitions_By(v_first_nfa.states, v_first_nfa_new_start), 
                     Increment_All_Transitions_By(v_second_nfa.states, v_second_nfa_new_start)
                    )),
                 accepting => Union(
                   Increment_By(v_first_nfa.accepting, v_first_nfa_new_start),
                   Increment_By(v_second_nfa.accepting, v_second_nfa_new_start)
                    )
                 );
      else
         raise Invalid_Subtree with "Union subtree did not have two subtrees";
      end if;
   end Gen_Union;

   function Gen_NFA(p_cursor: Regex_AST.Cursor) return NFA is 
      v_token : Abstract_Syntax_Token;
      --v_map : Map;
   begin 
      v_token := Element(p_cursor);
      case v_token.f_class is 
         when Parse_Types.Character =>
            return Gen_Character(v_token, p_cursor);
         when Parse_Types.Concat =>
            return Gen_Concat(v_token, p_cursor);
         when Parse_Types.Union =>
            return Gen_Union(v_token, p_cursor);
         when others => 
            raise Invalid_Subtree with "Unknown token";
      end case;
   end Gen_NFA;
   
   function Gen_NFA(AST: Tree) return NFA is 
      v_cursor : Regex_AST.Cursor;
   begin
      if Parser.Count(AST) > 0 then 
         v_cursor := First_Child(Root(AST));
         
         return Gen_NFA(v_cursor);
         
      else 
         return (
                 start => 0,
                 states => Empty_Vector,
                 accepting => Empty_Set
              );
      end if;
   end;
   
   function Count_State(machine: NFA) return Natural is 
   begin 
      return Natural(Length(machine.states));
   end Count_State;
   
   function Count_Epsilon_Transitions(machine: NFA) return Natural is 
      v_count : Natural := 0;
      procedure Accumulate ( Position: State_To_Input_Map.Cursor) is 
      begin
         v_count := v_count + Natural(Length(Element(Position).epsilon_transitions));
      end Accumulate;
   begin
      Iterate(machine.states, Accumulate'Access);
      return v_count;
   end Count_Epsilon_Transitions;


   

end Code_Gen;
