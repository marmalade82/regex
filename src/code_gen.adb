with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;


package body Code_Gen is
   function Gen_NFA(The_Node: Regex_AST.Cursor) return NFA;
   use State_Transitions;
   use NFA_Input_Transitions;
   use NFA_States;
   
   function Join(The_First : State_Transitions.Vector; The_Second: State_Transitions.Vector) return State_Transitions.Vector is 
      My_Vector: State_Transitions.Vector;
   begin 
      My_Vector := Empty_Vector;
      Append(My_Vector, The_First);
      Append(My_Vector, The_Second);
      return My_Vector;
   end Join;
   
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
     
   
   function Equal_Transitions(The_Left, The_Right : Transitions) return Boolean is 
   begin 
      return The_Left.input_transitions = The_Right.input_transitions and then The_Left.epsilon_transitions = The_Right.epsilon_transitions;
   end Equal_Transitions;
   
   function Has_Transitions_For_Input(The_Transitions: Transitions; c: Character) return Boolean is 
   begin 
      return (not Is_Empty(The_Transitions.epsilon_transitions)) or else Find(The_Transitions.input_transitions, c) /= NFA_Input_Transitions.No_Element;
   end Has_Transitions_For_Input;
   
   
   function Get_Epsilon_Closure(The_Current_States: Set; The_State_Table: Vector) return Set is 
      My_States : Set := Empty_Set;
      My_New_States: Set := Empty_Set;
      procedure Get_Epsilon_States(The_Position: NFA_States.Cursor) is 
         My_State : Natural;
         My_Further_States : Set;
         My_Difference : Set;
      begin
         My_State := Element(The_Position);
         
         -- Look at the epsilon states, but just the new ones that aren't in the existing states
         -- We only dig deeper if we find new states.
         -- THIS IS WRONG.
         My_Difference := Difference(Element(The_State_Table, My_State).epsilon_transitions, My_States);
         Union(My_New_States, My_States);
         Union(My_New_States, My_Difference);
         if not Is_Empty(My_Difference) then
            -- add the new states, and then explore deeper
            Union(My_New_States, Copy(My_States));
            My_Further_States := Get_Epsilon_Closure(My_New_States, The_State_Table);
            
            Union(My_New_States, Copy(My_Further_States));
         end if;       
      end Get_Epsilon_States;
   begin
      My_States := Copy(The_Current_States);
      Iterate(My_States, Get_Epsilon_States'Access);
      return Copy(My_New_States);
   end Get_Epsilon_Closure;
   
   -- walks through all the one-character transitions, as well as the entire epsilon-closure
   function Get_New_States_On_Input(The_Transitions: Transitions; The_States: Vector; The_Input: Character) return Set is 
      My_States : Set := Empty_Set;
   begin 
      -- First get direct input transitions where possible
      case The_Transitions.kind is 
         when By_Char =>
            if Find(The_Transitions.input_transitions, The_Input) /= NFA_Input_Transitions.No_Element then 
               Union(My_States, Element(The_Transitions.input_transitions, The_Input));
            end if;
         when By_Range => 
            if Inputs.Contains(The_Transitions.range_inputs, The_Input) then 
               Union(My_States, The_Transitions.range_states);
            end if;
         when By_Range_Complement =>
            if not Inputs.Contains(The_Transitions.range_inputs, The_Input) then 
               Union(My_States, The_Transitions.range_states);
            end if;
      end case;
      
      -- Then get epsilon closure of these new states.
      My_States := Get_Epsilon_Closure(My_States, The_States);
      
      return My_States;
   end Get_New_States_On_Input;

   
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
   
   function Get_New_States(The_States : Set; The_Transitions: Vector; The_Input: Character) return Set is 
      My_New_States : Set := Empty_Set;
      
      procedure Add_New_States(The_Position: NFA_States.Cursor) is 
         My_Current_State : Natural;
         My_Current_Transitions: Transitions;
      begin
         My_Current_State := Element(The_Position);
         My_Current_Transitions := Element(The_Transitions, My_Current_State);
         Union(My_New_States, Get_New_States_On_Input(My_Current_Transitions, The_Transitions, The_Input));
      end Add_New_States;
   begin
      Iterate(The_States, Add_New_States'Access);
      return My_New_States;
      
   end Get_New_States;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean is 
      My_Input : Character;
      The_Current_States : Set;
      My_New_States: Set;
      My_Initial_States: Set;
   begin 
      -- for every character in the input,
      -- check the current state to get the transition functions, and 
      -- check what the next state is.
      My_Initial_States := To_Set(The_Machine.start);
      The_Current_States := Get_Epsilon_Closure(My_Initial_States, The_Machine.states);
      for I in 1..Length(The_Input) loop
         My_Input := Element(The_Input, I);
         
         -- for each state in the current states, get the new states and append to the new states.
         My_New_States := Empty_Set;
         My_New_States := Get_New_States(The_Current_States, The_Machine.states, My_Input);
         
         -- if the new_states are empty, we could not transition on the input at all
         if Is_Empty(My_New_States) then 
            return False;
         else 
            The_Current_States := Copy(My_New_States);
         end if;
         
      end loop;
      
      -- We consumed all the input, so the successful 
      -- run of the The_Machine determines success.
      return not Is_Empty(Intersection(The_Machine.accepting, The_Current_States));
   end Recognize;
   
   function Increment_By(The_Set : Set; The_Increment: Positive) return Set is 
      A_Set : Set := Empty_Set;
      procedure Do_Increment(The_Position: NFA_States.Cursor) is 
         A_State : Natural;
      begin
         A_State := Element(The_Position);
         Insert(A_Set, A_State + Natural(The_Increment)); 
      end Do_Increment;
   begin
      Iterate(The_Set, Do_Increment'Access);
      return A_Set;
   end Increment_By;
   
   function Increment_All_Input_Transitions_By(The_Transitions: NFA_Input_Transitions.Map; The_Increment: Positive) return NFA_Input_Transitions.Map is 
      My_New_Map: Map := Empty_Map;
      procedure Do_Increment(The_Position: NFA_Input_Transitions.Cursor) is 
         My_States : Set;
      begin
         My_States := Increment_By(Element(The_Position), The_Increment);
         Insert(My_New_Map, Key(The_Position), My_States);
      end Do_Increment;
   begin 
      Iterate(The_Transitions, Do_Increment'Access);
      return My_New_Map;   
   end Increment_All_Input_Transitions_By;
   
   function Increment_All_Epsilon_Transitions_By(The_Transitions: NFA_States.Set; The_Increment: Positive) return NFA_States.Set is 
      My_New_States : Set := Empty_Set;
   begin
      My_New_States := Increment_By(The_Transitions, The_Increment);
      return My_New_States;
   end Increment_All_Epsilon_Transitions_By;
   
   function Increment_All_Transitions_By(The_State_Table: Vector; The_Increment: Positive) return Vector is 
      My_New_Table: Vector := Empty_Vector;
      procedure Do_Increment(The_Position: State_Transitions.Cursor) is 
         My_Transition: Transitions;
         My_New_Transition: Transitions;
      begin
         My_Transition := Element(The_Position);
         My_New_Transition := ( input_transitions => Increment_All_Input_Transitions_By(My_Transition.input_transitions, The_Increment),
                               epsilon_transitions => Increment_All_Epsilon_Transitions_By(My_Transition.epsilon_transitions, The_Increment),
                               kind => My_Transition.kind,
                               range_inputs => Inputs.Copy(My_Transition.range_inputs),
                               range_states => Increment_By(My_Transition.range_states, The_Increment)
                              );
         Append(My_New_Table, My_New_Transition);
      end Do_Increment;
   begin 
      Iterate(The_State_Table, Do_Increment'Access);
      return My_New_Table;
   end Increment_All_Transitions_By;
   
   function Update_States_To_Epsilon_Transition_To(The_State_Table: Vector; The_States: Set; The_New_State: Natural) return Vector is 
      My_New_State_Table : Vector := Empty_Vector;
      procedure Update_Epsilon_Transition(The_Position: NFA_States.Cursor) is 
         My_State : Natural;
         My_Transitions : Transitions;
         My_Set : Set := Empty_Set;
         My_Old_Transitions : Transitions;
      begin 
         My_State := Element(The_Position);
         My_Old_Transitions := Element(My_New_State_Table, My_State);
         My_Set := Copy(My_Old_Transitions.epsilon_transitions);
         Insert(My_Set, The_New_State);
         My_Transitions := ( input_transitions => Copy(My_Old_Transitions.input_transitions),
                            epsilon_transitions => My_Set,
                            kind => My_Old_Transitions.kind,
                            range_inputs => Inputs.Copy(My_Old_Transitions.range_inputs),
                            range_states => NFA_States.Copy(My_Old_Transitions.range_states)
                           );
         Replace_Element(My_New_State_Table, My_State, My_Transitions);
      end Update_Epsilon_Transition;
   begin 
      My_New_State_Table := Copy(The_State_Table);
      Iterate(The_States, Update_Epsilon_Transition'Access);
      return My_New_State_Table;
   end Update_States_To_Epsilon_Transition_To;
   
   function Make_Character_NFA(The_Character : Character) return NFA is 
      My_Map : Map;
      My_Transitions: Transitions;
   begin
      My_Map := Empty_Map;
      -- for this nfa, a single character is enough.
      Insert(My_Map, The_Character, To_Set(1));
      My_Transitions := ( input_transitions => My_Map,
                         epsilon_transitions => Empty_Set,
                         kind => By_Char,
                         range_inputs => Inputs.Empty_Set,
                         range_states => NFA_States.Empty_Set
                        );
      return (
              start => 0,
              states => Empty_Vector &
                My_Transitions & -- map from start state to end state
                Empty_Transitions, -- no transitions from the accepting state.
              accepting => To_Set(1) -- says that accepting states includes State 1       
             );
   end Make_Character_NFA;

   function Gen_Character(The_Token : Abstract_Syntax_Token; The_Node: Regex_AST.Cursor) return NFA is 
   begin
      return Make_Character_NFA( Element(The_Token.f_lexeme, Length(The_Token.f_lexeme)) );
   end Gen_Character;
   
   function Gen_Escape_Character(The_Class : Escape_Characters; The_Node: Regex_AST.Cursor) return NFA is 
      My_NFA : NFA;
   begin
      case The_Class is 
         when Newline =>
            My_NFA := Make_Character_NFA(LF);
         when Tab =>
            My_NFA := Make_Character_NFA(HT);
         when Carriage_Return =>
            My_NFA := Make_Character_NFA(CR);
      end case;
      
      return My_NFA;
   end Gen_Escape_Character;
   
   function Make_Concat_NFA(The_Left : NFA; The_Right : NFA) return NFA is 
      My_Right_New_Start : Natural;
      My_Left_Updated_States : Vector;
   begin 
      -- at this point, the first nfa's accepting state transitions need to be modified to 
      -- have an epsilon transition to the starting state of the second nfa.
      My_Right_New_Start := Natural(Length(The_Left.states));
      My_Left_Updated_States := Update_States_To_Epsilon_Transition_To
        (The_Left.states, The_Left.accepting, My_Right_New_Start);
      
      return (
              start => 0, -- we start at start of first nfa
              states => Join(My_Left_Updated_States, Increment_All_Transitions_By(The_Right.states, My_Right_New_Start)),
              accepting => Increment_By(The_Right.accepting, My_Right_New_Start)
             );
   end Make_Concat_NFA;
   
   function Gen_Concat(The_Token : Abstract_Syntax_Token; The_Node: Regex_AST.Cursor) return NFA is 
      My_Map : Map;
      My_First_Child : Regex_AST.Cursor;
      My_Second_Child : Regex_AST.Cursor;
      My_First_NFA: NFA;
      My_Second_NFA: NFA;
   begin
      -- Need to grab NFAs made from first two children and then combine them into one NFA.
      My_First_Child := First_Child(The_Node);
      My_Second_Child := Next_Sibling(My_First_Child);
      
      if My_First_Child /= Regex_AST.No_Element and then My_Second_Child /= Regex_AST.No_Element then 
         My_First_NFA := Gen_NFA(My_First_Child);
         My_Second_NFA := Gen_NFA(My_Second_Child);
         
         return Make_Concat_NFA(My_First_NFA, My_Second_NFA);
      else
         raise Invalid_Subtree with "Concat subtree did not have two subtrees";
      end if;
   end Gen_Concat;
   
   function Gen_Union(The_Token : Abstract_Syntax_Token; The_Node: Regex_AST.Cursor) return NFA is 
      My_Map : Map;
      My_First_Child : Regex_AST.Cursor;
      My_Second_Child : Regex_AST.Cursor;
      My_First_NFA: NFA;
      My_Second_NFA: NFA;
      My_First_NFA_new_start: Natural;
      My_Second_NFA_new_start : Natural;
      My_New_Start_Transitions : Transitions;
      My_New_Start_Epsilon_Trans : Set := Empty_Set;
   begin
      -- Need to grab NFAs made from first two children and then combine them into one NFA.
      My_First_Child := First_Child(The_Node);
      My_Second_Child := Next_Sibling(My_First_Child);
      
      if My_First_Child /= Regex_AST.No_Element and then My_Second_Child /= Regex_AST.No_Element then 
         My_First_NFA := Gen_NFA(My_First_Child);
         My_Second_NFA := Gen_NFA(My_Second_Child);
         
         -- at this point, we add a new start state 0 that has epsilon transitions to 
         -- the start states of the two child NFAs.
         My_First_NFA_new_start := 1;
         My_Second_NFA_new_start := 1 + Natural(Length(My_First_NFA.states));
         Insert(My_New_Start_Epsilon_Trans, My_First_NFA_new_start);
         Insert(My_New_Start_Epsilon_Trans, My_Second_NFA_new_start);
         My_New_Start_Transitions :=
         ( input_transitions => Empty_Map, -- there are no input transitions
           epsilon_transitions => My_New_Start_Epsilon_Trans,
           kind => By_Char,
           range_inputs => Inputs.Empty_Set,
           range_states => NFA_States.Empty_Set
          );
         
         return (
                 start => 0, -- we start at start of first nfa
                 states => Join(To_Vector(My_New_Start_Transitions, 1), 
                   Join(
                     Increment_All_Transitions_By(My_First_NFA.states, My_First_NFA_new_start), 
                     Increment_All_Transitions_By(My_Second_NFA.states, My_Second_NFA_new_start)
                    )),
                 accepting => Union(
                   Increment_By(My_First_NFA.accepting, My_First_NFA_new_start),
                   Increment_By(My_Second_NFA.accepting, My_Second_NFA_new_start)
                    )
                 );
      else
         raise Invalid_Subtree with "Union subtree did not have two subtrees";
      end if;
   end Gen_Union;
   
   function Range_From_Interval(The_Interval: Regex_AST.Cursor) return Inputs.Set is 
      My_Left : Regex_AST.Cursor;
      My_Right : Regex_AST.Cursor;
      My_Left_Token : Abstract_Syntax_Token;
      My_Right_Token : Abstract_Syntax_Token;
      My_Range : Inputs.Set := Inputs.Empty_Set;
      My_Left_Char : Character;
      My_Right_Char : Character;
      My_Valid_Pair : Boolean;
      
      function Valid(The_Left, The_Right : Abstract_Syntax_Token) return Boolean is 
         My_Left_Char : Character;
         My_Right_Char : Character;
      begin 
         -- The two operands should both have the Character class
         if The_Left.f_class /= The_Right.f_class then
            return False;
         end if;
         
         if The_Left.f_class /= Parse_Types.Character then
            return False;
         end if;
         
         -- There are only a few valid comparisons for an interval
         -- Two lowercase, two uppercase, or two digits
         My_Left_Char := Element(The_Left.f_lexeme, Length(The_Left.f_lexeme));
         My_Right_Char := Element(The_Right.f_lexeme, Length(The_Right.f_lexeme));
         My_Valid_Pair := 
           (Is_Lower(My_Left_Char) and then Is_Lower(My_Right_Char)) or else
           (Is_Upper(My_Left_Char) and then Is_Upper(My_Right_Char)) or else
           (Is_Digit(My_Left_Char) and then Is_Digit(My_Right_Char));
           
         if not My_Valid_Pair then
            return False;
         end if;
         
         -- The left should be less than or equal to the right.
         if (My_Left_Char > My_Right_Char) then
            return False;
         end if;
         
         return True;
      end Valid;
   begin
      My_Left := First_Child(The_Interval);
      My_Right := Next_Sibling(My_Left);
      
      if My_Left /= Regex_AST.No_Element and then My_Right /= Regex_AST.No_Element then 
         My_Left_Token := Element(My_Left);
         My_Right_Token := Element(My_Right);
         My_Left_Char := Element(My_Left_Token.f_lexeme, Length(My_Left_Token.f_lexeme));
         My_Right_Char := Element(My_Right_Token.f_lexeme, Length(My_Right_Token.f_lexeme));
         if Valid(My_Left_Token, My_Right_Token) then 
            
            for C in My_Left_Char .. My_Right_Char loop
               Inputs.Insert(My_Range, C);
            end loop;
            
            return My_Range;
         else 
            raise Invalid_Subtree with ("Interval's children were not valid: " & My_Left_Char & My_Right_Char);
         end if;   
      else 
         raise Invalid_Subtree with "Interval did not have two children";
      end if;
   end Range_From_Interval;
   
   function Escape_Inputs( The_Class : Escape_Characters ) return Inputs.Set is 
      My_Set : Inputs.Set := Inputs.Empty_Set;
   begin
      case The_Class is 
         when Newline =>
            My_Set := Inputs.To_Set(LF);
         when Tab =>
            My_Set := Inputs.To_Set(HT);
         when Carriage_Return =>
            My_Set := Inputs.To_Set(CR);
      end case;
      
      return My_Set;
   end Escape_Inputs;
   
   function Build_Range_Helper(The_Position : Regex_AST.Cursor; The_Range_Inputs : Inputs.Set) return Inputs.Set is 
      An_Input : Character;
      A_Token : Abstract_Syntax_Token;
      My_New_Set : Inputs.Set := Inputs.Empty_Set;
   begin
      A_Token := Element(The_Position);
      case A_Token.f_class is 
         when Parse_Types.Character =>
            An_Input := Element(A_Token.f_lexeme, Length(A_Token.f_lexeme));
            Inputs.Insert(My_New_Set, An_Input);
         when Escape_Characters =>
            Inputs.Union(My_New_Set, Escape_Inputs(A_Token.f_class));
         when Parse_Types.Range_Interval => 
            Inputs.Union(My_New_Set, Range_From_Interval(The_Position));
         when others => 
            raise Unknown_AST_Token with "Token that a range cannot handle";
      end case;
      
      return Inputs.Union(The_Range_Inputs, My_New_Set);
   end Build_Range_Helper;
   
   -- This expects that the group subtree has been flattened as much as possible
   function Gen_Range_Group(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
      A_Transition : Transitions;
      Some_Range_Inputs : Inputs.Set;
      procedure Build_Range(The_Position : Regex_AST.Cursor) is 
      begin
         Some_Range_Inputs := Build_Range_Helper(The_Position, Some_Range_Inputs);
         
      end Build_Range;
   begin 
      Iterate_Children(The_Parent, Build_Range'Access);
      
      A_Transition := ( input_transitions => Empty_Map,
                        epsilon_transitions => Empty_Set,
                        range_inputs => Some_Range_Inputs,
                        range_states => To_Set(1),
                        kind => By_Range
                       );
      
      return ( start => 0,
               states => Empty_Vector &
                 A_Transition & 
                 Empty_Transitions,
               accepting => To_Set(1)
              );
   end Gen_Range_Group;
   
   -- This expects that the group subtree has been flattened as much as possible
   function Gen_Range_Complement(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
      A_Transition : Transitions;
      Some_Range_Inputs : Inputs.Set;
      procedure Build_Range(The_Position : Regex_AST.Cursor) is 
      begin
         Some_Range_Inputs := Build_Range_Helper(The_Position, Some_Range_Inputs);
      end Build_Range;
   begin 
      Iterate_Children(The_Parent, Build_Range'Access);
      
      A_Transition := ( input_transitions => Empty_Map,
                        epsilon_transitions => Empty_Set,
                        range_inputs => Some_Range_Inputs,
                        range_states => To_Set(1),
                        kind => By_Range_Complement
                       );
      
      return ( start => 0,
               states => Empty_Vector &
                 A_Transition & 
                 Empty_Transitions,
               accepting => To_Set(1)
              );
   end Gen_Range_Complement;
   
   
   function Make_Wildcard_NFA(The_NFA : NFA) return NFA is 
      My_Inner_Start: Natural;
      My_Inner_State_Table : Vector;
      My_Start_Transition : Transitions;
      
   begin
      -- to augment this NFA, we need to add a new start and accepting state,
      -- which we will put at the start of the state table.
      My_Inner_Start := 1;
      My_Start_Transition := ( input_transitions => Empty_Map,
                               kind => By_Char,
                               range_inputs => Inputs.Empty_Set,
                               range_states => NFA_States.Empty_Set,
                               epsilon_transitions => 
                                 NFA_States.Union( 
                                   -- transitions to accepting state and start of inner nfa.
                                   NFA_States.To_Set(My_Inner_Start + Natural(Length(The_NFA.states))),
                                   NFA_States.To_Set(My_Inner_Start)  
                                  )
                              );
        
      My_Inner_State_Table := 
        Increment_All_Transitions_By -- Bump all the old states to their new positions
          (
           Update_States_To_Epsilon_Transition_To -- update old accepting states to transition to new accepting state.
             ( The_NFA.states, The_NFA.accepting, Natural(Length(The_NFA.states))
             ), 
           My_Inner_Start
          );
           
      return ( start => 0,
               states => Empty_Vector &
                 My_Start_Transition & 
                 My_Inner_State_Table &
               ( input_transitions => Empty_Map,
                 kind => By_Char,
                 range_inputs => Inputs.Empty_Set,
                 range_states => NFA_States.Empty_Set,
                 epsilon_transitions => NFA_States.To_Set(0) -- accepting state can transition back to to start.
                ),
               accepting => To_Set(My_Inner_Start + Natural(Length(The_NFA.states)))
              );
   end Make_Wildcard_NFA;
   
   function Gen_Zero_Or_More(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
      My_Map : Map;
      My_First_Child : Regex_AST.Cursor;
      My_First_NFA: NFA;
   begin
      -- Need to grab NFAs made from first child and then augment it into a wildcard.
      My_First_Child := First_Child(The_Parent);
      
      if My_First_Child /= Regex_AST.No_Element then 
         My_First_NFA := Gen_NFA(My_First_Child);
         
         return Make_Wildcard_NFA(My_First_NFA);
      else 
         raise Invalid_Subtree with "Wildcard subtree had zero subtrees";
      end if;
      
   end Gen_Zero_Or_More;
   
   function Gen_One_Or_More(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
      My_Map : Map;
      My_First_Child : Regex_AST.Cursor;
      My_First_NFA: NFA;
   begin
      -- Need to grab NFAs made from first child and then augment it into a plus.
      My_First_Child := First_Child(The_Parent);
      
      if My_First_Child /= Regex_AST.No_Element then 
         My_First_NFA := Gen_NFA(My_First_Child);
         
         return Make_Concat_NFA(My_First_NFA, Make_Wildcard_NFA(My_First_NFA));
      else 
         raise Invalid_Subtree with "Plus subtree had zero subtrees";
      end if;
   end Gen_One_Or_More;
   
   function Gen_Optional(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
      My_Map : Map;
      My_First_Child : Regex_AST.Cursor;
      My_Optional_NFA : NFA;
   begin
      -- Need to grab NFAs made from first child and then augment it into an optional.
      -- The simplest way to do this is to make the starting state an accepting state.
      -- Ideally, in terms of primitives, we would like to union this NFA with the next NFA in the sequence,
      -- but based on our loop, implementing this would be more trouble than it's worth.
      -- So in our case, we'll regard the optional as an additional primitive.
      My_First_Child := First_Child(The_Parent);
      
      if My_First_Child /= Regex_AST.No_Element then          
         My_Optional_NFA := Gen_NFA(My_First_Child);
         
         NFA_States.Insert(My_Optional_NFA.accepting, My_Optional_NFA.start);
         
         return My_Optional_NFA; 
      else 
         raise Invalid_Subtree with "Optional subtree had zero subtrees";
      end if;
   end Gen_Optional;

   function Gen_NFA(The_Node: Regex_AST.Cursor) return NFA is 
      My_Token : Abstract_Syntax_Token;
   begin 
      if The_Node /= Regex_AST.No_Element then 
         My_Token := Element(The_Node);
         case My_Token.f_class is 
         when Parse_Types.Character =>
            return Gen_Character(My_Token, The_Node);
         when Parse_Types.Concat =>
            return Gen_Concat(My_Token, The_Node);
         when Parse_Types.Union =>
            return Gen_Union(My_Token, The_Node);
         when Parse_Types.Range_Group => 
            return Gen_Range_Group(My_Token, The_Node);
         when Parse_Types.Range_Complement =>
            return Gen_Range_Complement(My_Token, The_Node);
         when Parse_Types.Zero_Or_More =>
            return Gen_Zero_Or_More(My_Token, The_Node);
         when Parse_Types.One_Or_More => 
            return Gen_One_Or_More(My_Token, The_Node);
         when Parse_Types.Optional =>
            return Gen_Optional(My_Token, The_Node);
         when Parse_Types.Match_Start | Parse_Types.Match_End =>
            -- ignore what are essentially operational directives
            return Gen_NFA(First_Child(The_Node));
         when Escape_Characters =>
            return Gen_Escape_Character(My_Token.f_class, The_Node);
         when Non_NFA_Classes => 
            raise Unknown_AST_Token with "Cannot use this token while generating NFA";
         end case;
      else 
         return ( start => 0,
                  states => Empty_Vector,
                  accepting => Empty_Set
                 );
      end if;
   end Gen_NFA;
   
   function Gen_NFA(The_AST: Tree) return NFA is 
      My_Node : Regex_AST.Cursor;
   begin
      if Parser.Count(The_AST) > 0 then 
         My_Node := First_Child(Root(The_AST));
         
         return Gen_NFA(My_Node);
         
      else 
         return (
                 start => 0,
                 states => Empty_Vector,
                 accepting => Empty_Set
              );
      end if;
   end;
   
   function Count_State(The_Machine: NFA) return Natural is 
   begin 
      return Natural(Length(The_Machine.states));
   end Count_State;
   
   function Count_Epsilon_Transitions(The_Machine: NFA) return Natural is 
      My_Count : Natural := 0;
      procedure Accumulate ( The_Position: State_Transitions.Cursor) is 
      begin
         My_Count := My_Count + Natural(Length(Element(The_Position).epsilon_transitions));
      end Accumulate;
   begin
      Iterate(The_Machine.states, Accumulate'Access);
      return My_Count;
   end Count_Epsilon_Transitions;


   

end Code_Gen;
