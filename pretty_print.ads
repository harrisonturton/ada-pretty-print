
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

  -- Used to have multiple effects at the same time, i.e. (Bold, Underline)
  type Effect_List is array (Positive range <>) of Effect;

  -- Style a String with colors & effects.
  function With_Style (C : Color; E : Effect; S : String) return String;
  function With_Style (C : Color; E : Effect_List; S : String) return String;

  -- Colour a String. Helper function for each color.
  function With_Color (C : Color;  S : String) return String;
  function Black   (S : String) return String;
  function Red     (S : String) return String;
  function Green   (S : String) return String;
  function Yellow  (S : String) return String;
  function Blue    (S : String) return String;
  function Magenta (S : String) return String;
  function Cyan    (S : String) return String;
  function White   (S : String) return String;

  -- Put an effect on a String - Bold, Underlined etc.
  -- Can easily combine multiple effects.
  function With_Effect (E : Effect; S : String) return String;
  function With_Effect (E : Effect_List;  S : String) return String;
  function Bold        (S : String) return String;
  function Faint       (S : String) return String;
  function Italic      (S : String) return String;
  function Underline   (S : String) return String;
  function Slow_Blink  (S : String) return String;
  function Fast_Blink  (S : String) return String;
  function Strike      (S : String) return String;

  -- Applies a different color or effect depending on the
  -- ID of the task.
  function Color_Task  (Index : Positive; S : String) return String;
  function Effect_Task (Index : Positive; S : String) return String;

  -- Print a String in a specifc Color.
  procedure Put_Color  (C : in Color;  S : in String);

  -- Print a string with one or more Effects.
  procedure Put_Effect (E : in Effect; S : in String);
  procedure Put_Effect (E : in Effect_List; S : in String);

  -- Print a string with both a Color & one or more Effects.
  procedure Put_Style  (C : in Color;  E : in Effect; S : in String);
  procedure Put_Style  (C : in Color;  E : in Effect_List; S : in String);

  -- Print a String styled depending on the index.
  -- Intended to make debugging concurrent output simpler.
  procedure Put_Task   (Index : in Positive; S : in String);

end;
