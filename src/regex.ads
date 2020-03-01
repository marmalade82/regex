with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Code_Gen_DFAs; use Code_Gen_DFAs;
with Ada.Containers.Vectors;

package Regex is
   Cannot_Convert : exception;
   
   type Result is record 
      M_String: Unbounded_String;
      Index: Positive;
   end record;
   
   
   package String_Vector is new Ada.Containers.Vectors 
     (Index_Type   => Positive,
      Element_Type => Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");
   
   subtype Results is String_Vector.Vector;
   
   function Empty_Results return Results;
   
   function As_String(The_Results: Results) return String;
   
   type Regex is record
      M_Automaton: D_Finite_Automaton;
   end record;

   function Compile(The_Machine: out Regex; S: String; The_Error: out Unbounded_String) return Boolean;
   
   function Compile(The_Machine: out Regex; S: String) return Boolean;
   
   -- Tests whether there is any regex match within @S
   function Test(The_Machine: in out Regex; S: String) return Boolean;
   
   -- Tests for nonzero length matches within @S
   function Match(The_Machine: in out Regex; S: String) return Results;
     
   
   

end Regex;
