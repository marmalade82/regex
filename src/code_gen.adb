with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Parser;

package body Code_Gen is
   use State_To_Input_Map;
   use Input_To_State;
   use Accepting_Set;
   
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
   
   function Recognize(machine: NFA; input: Unbounded_String) return Boolean is 
      c : Character;
      transition : Map;
      next_state : Natural;
      current_state : Natural;
   begin 
      -- for every character in the input,
      -- check the current state to get the transition functions, and 
      -- check what the next state is.
      current_state := machine.start;
      for I in 1..Length(input) loop
         c := Element(input, I);
         transition := Element(machine.states, current_state);
         
         if Find(transition, c) /= Input_To_State.No_Element then 
            next_state := Element(transition, c);
            current_state := next_state;
         else 
            -- if we could not do anything at all without consuming all input, then we failed.
            return False;
         end if;
      end loop;
      
      -- We consumed all the input.
      return Contains(machine.accepting, current_state);
   end Recognize;



   function Gen_NFA(p_cursor: Regex_AST.Cursor) return NFA is 
      v_token : Abstract_Syntax_Token;
      v_map : Map;
   begin 
      v_token := Element(p_cursor);
      case v_token.f_class is 
         when Parse_Types.Character =>
            v_map := Empty_Map;
            -- for this nfa, a single character is enough.
            Insert(v_map, Element(v_token.f_lexeme, Length(v_token.f_lexeme)), 1);
            return (
                    start => 0,
                    states => Empty_Vector &
                      v_map & -- map from start state to end state
                      Empty_Map, -- empty map
                    accepting => To_Set(1) -- says that accepting states includes State 1
                      
                   );
         when others => 
            return (
                    start => 1,
                    states => Empty_Vector, 
                    accepting => Empty_Set
                   );
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
                 start => 1,
                 states => Empty_Vector,
                 accepting => Empty_Set
              );
      end if;
   end;
   
   function Count_State(machine: NFA) return Natural is 
      
   begin 
      return Natural(Length(machine.states));
   end Count_State;


   

end Code_Gen;
