with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Code_Gen_NFAs; use Code_Gen_NFAs;
with Code_Gen_DFAs; use Code_Gen_DFAs;


package body Code_Gen is
   function Gen_NFA(The_Node: Regex_AST.Cursor) return NFA;
   use State_Transitions;
   use NFA_Input_Transitions;
   use NFA_States;
   
   function Recognize(The_Machine: NFA; The_Input: Unbounded_String) return Boolean is 
   begin
      return Code_Gen_NFAs.Recognize(The_Machine, The_Input);
   end Recognize;
   
   function Has_Transitions_For_Input(The_Transitions: Transitions; c: Character) return Boolean is 
   begin 
      return (not Is_Empty(The_Transitions.epsilon_transitions)) or else Find(The_Transitions.input_transitions, c) /= NFA_Input_Transitions.No_Element;
   end Has_Transitions_For_Input;

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
   begin
      -- Need to grab NFAs made from first two children and then combine them into one NFA.
      My_First_Child := First_Child(The_Node);
      My_Second_Child := Next_Sibling(My_First_Child);
      
      if My_First_Child /= Regex_AST.No_Element and then My_Second_Child /= Regex_AST.No_Element then 
         My_First_NFA := Gen_NFA(My_First_Child);
         My_Second_NFA := Gen_NFA(My_Second_Child);
         
         return Make_Union_NFA(My_First_NFA, My_Second_NFA);
      else
         raise Invalid_Subtree with "Union subtree did not have two subtrees";
      end if;
   end Gen_Union;
   
   
   
   -- This expects that the group subtree has been flattened as much as possible
   function Gen_Range_Group(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
   begin 
      return Make_Range_NFA(The_Parent);
   end Gen_Range_Group;
   
   -- This expects that the group subtree has been flattened as much as possible
   function Gen_Range_Complement(The_Token : Abstract_Syntax_Token; The_Parent: Regex_AST.Cursor) return NFA is 
   begin
      return Make_Complement_NFA(The_Parent);
   end Gen_Range_Complement;
   
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
   
   
   function Gen_DFA(The_AST: Tree) return DFA is 
      My_NFA : NFA;
   begin
      My_NFA := Gen_NFA(The_AST);
      
      return NFA_To_DFA(My_NFA);
      
   end Gen_DFA;

   function Recognize(The_Machine: DFA; The_Input: Unbounded_String) return Boolean is 
      use DFA_Input_Transitions;
      My_Current_State : Natural;
      My_Current_Transitions : DFA_Transitions;
      My_Input : Character;
   begin 
      -- Operating a DFA is simple. We loop over the input and try to match as long as we can
      My_Current_State := The_Machine.start;
      
      for I in 1..Length(The_Input) loop
         My_Input := Ada.Strings.Unbounded.Element(The_Input, I);
         My_Current_Transitions := DFA_States.Element(The_Machine.states, My_Current_State);
         
         if DFA_Input_Transitions.Find(My_Current_Transitions.input_transitions, My_Input) /= DFA_Input_Transitions.No_Element then 
            My_Current_State := DFA_Input_Transitions.Element(My_Current_Transitions.input_transitions, My_Input);
         else 
            -- IF we couldn't find the transition, we failed to process all the input.
            Put_Line("Couldn't find next transition, failed at state " & My_Current_State'Image & " with input " & My_Input);
            return False;
         end if;
      end loop;
      
      -- If we processed all the input, we need to make sure that our final state is an accepting state.
      
      return Contains(The_Machine.accepting, My_Current_State);
   end Recognize;
   
   
   function Count_State(The_Machine: DFA) return Natural is
      
   begin 
      return Natural( DFA_States.Length(The_Machine.states) );
   end Count_State;
   
   function Count_Epsilon_Transitions(The_Machine: DFA) return Natural is 
      
   begin 
      return 0;
   end Count_Epsilon_Transitions;

   
   

end Code_Gen;
