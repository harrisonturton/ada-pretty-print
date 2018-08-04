
generic
  No_Of_Tasks : Positive;
package Pretty_Print is
  
  type Color is
    (Black,
     Red,
     Green,
     Yellow,
     Blue,
     Magenta,
     Cyan,
     White);

  type Effect is
    (Bold,
     Faint,
     Italic,
     Underline,
     Slow_Blink,
     Fast_Blink,
     Strike);

  type Effect_List is array (Positive range <>) of Effect;

  -- Style a string by a colour & any number of effects.
  function With_Style (C : Color; E : Effect; S : String) return String;
  function With_Style (C : Color; E : Effect_List; S : String) return String;

  -- Colour a string
  function With_Color  (C : Color;  S : String) return String;
  function Color_Task  (Index : Positive; S : String) return String;
  function Black   (S : String) return String;
  function Red     (S : String) return String;
  function Green   (S : String) return String;
  function Yellow  (S : String) return String;
  function Blue    (S : String) return String;
  function Magenta (S : String) return String;
  function Cyan    (S : String) return String;
  function White   (S : String) return String;

  -- Put an effect on a string
  function With_Effect (E : Effect; S : String) return String;
  function With_Effect (E : Effect_List;  S : String) return String;
  function Bold       (S : String) return String;
  function Faint      (S : String) return String;
  function Italic     (S : String) return String;
  function Underline  (S : String) return String;
  function Slow_Blink (S : String) return String;
  function Fast_Blink (S : String) return String;
  function Strike     (S : String) return String;

  procedure Put_Color  (C : in Color;  S : in String);
  procedure Put_Effect (E : in Effect; S : in String);
  procedure Put_Effect (E : in Effect_List; S : in String);
  procedure Put_Style  (C : in Color;  E : in Effect; S : in String);
  procedure Put_Style  (C : in Color;  E : in Effect_List; S : in String);
  procedure Put_Task   (Index : in Positive; S : in String);

end;
