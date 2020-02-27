with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Code_Gen_DFAs; use Code_Gen_DFAs;

package Regex is
   Cannot_Convert : exception;
   
   type Regex is record
      M_Automaton: D_Finite_Automaton;
   end record;

   function Compile(The_Machine: out Regex; S: String; The_Error: out Unbounded_String) return Boolean;
   
   function Compile(The_Machine: out Regex; S: String) return Boolean;
   
   function Test(The_Machine: in out Regex; S: String) return Boolean;

end Regex;
