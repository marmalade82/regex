with Parse_Types; use Parse_Types;
use Parse_Types.Regex_AST;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Code_Gen is
   
   function Charac_Hash(Key: Character) return Ada.Containers.Hash_Type;
   
   function Equiv_Keys (Left, Right: Character) return Boolean;
   
   package Input_To_State is new Ada.Containers.Hashed_Maps
     ( Key_Type => Character,
       Element_Type => Natural,
       Hash => Charac_Hash,
       Equivalent_Keys => Equiv_Keys
      );
   
   package State_To_Input_Map is new Ada.Containers.Vectors 
     ( Index_Type => Natural,
       Element_Type => Input_To_State.Map,
       "=" => Input_To_State."="
      );
   
   function Natural_Hash(El: Natural) return Ada.Containers.Hash_Type;
   
   package Accepting_Set is new Ada.Containers.Hashed_Sets
     ( Element_Type => Natural,
       Hash => Natural_Hash,
       Equivalent_Elements => Standard."="
      );
   
   type NFA is record 
      start: Natural;
      states: State_To_Input_Map.Vector;
      accepting: Accepting_Set.Set; 
   end record;
   
   function Gen_NFA(AST: Tree) return NFA;
   
   function Count_State(machine: NFA) return Natural;
   
   function Recognize(machine: NFA; input: Unbounded_String) return Boolean;
   
   

end Code_Gen;
