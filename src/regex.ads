with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Regex is
   Cannot_Convert : exception;
   
   type Regex is record
      c: Unbounded_String;
   end record;

   function Compile(The_Machine: in out Regex; S: String; The_Error: out Unbounded_String) return Boolean;
   
   function Compile(The_Machine: in out Regex; S: String) return Boolean;

end Regex;
