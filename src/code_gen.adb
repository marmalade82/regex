with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Parser;

package body Code_Gen is
   
   function Charac_Hash(Key: Character) return Ada.Containers.Hash_Type is 
   begin 
      return Ada.Strings.Hash((1 => Key));
   end Charac_Hash;
   
   function Equiv_Keys (Left, Right: Character) return Boolean is 
   begin
      return Left = Right;
   end Equiv_Keys;
   
   use State_To_Input_Map;
   use Input_To_State;
   


   function Gen_NFA(p_cursor: Regex_AST.Cursor) return NFA is 
      v_token : Abstract_Syntax_Token;
      v_map : Map;
   begin 
      v_token := Element(p_cursor);
      case v_token.f_class is 
         when Parse_Types.Character =>
            v_map := Empty_Map;
            Insert(v_map, 
            return (
                    start => 0,
                    states => Empty_Vector 
                      
                   );
         when others => 
            return (
                    start => 1,
                    states => Empty_Vector 
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
                 states => Empty_Vector
              );
      end if;
   end;
   
   function Count_State(machine: NFA) return Natural is 
      
   begin 
      return 2;
   end Count_State;


   

end Code_Gen;
