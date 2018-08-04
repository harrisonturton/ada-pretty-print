with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package body Pretty_Print is

  subtype ANSI_Code is String;
  type ANSI_Code_Handle is access constant ANSI_Code;

  -- Colors
  Black_Code   : aliased constant ANSI_Code := "30";
  Red_Code     : aliased constant ANSI_Code := "31";
  Green_Code   : aliased constant ANSI_Code := "32";
  Yellow_Code  : aliased constant ANSI_Code := "33";
  Blue_Code    : aliased constant ANSI_Code := "34";
  Magenta_Code : aliased constant ANSI_Code := "35";
  Cyan_Code    : aliased constant ANSI_Code := "36";
  White_Code   : aliased constant ANSI_Code := "37";

  -- Effects
  Bold_Code       : aliased constant ANSI_Code := "1";
  Faint_Code      : aliased constant ANSI_Code := "2";
  Italic_Code     : aliased constant ANSI_Code := "3";
  Underline_Code  : aliased constant ANSI_Code := "4";
  Slow_Blink_Code : aliased constant ANSI_Code := "5";
  Fast_Blink_Code : aliased constant ANSI_Code := "6";
  Strike_Code     : aliased constant ANSI_Code := "9";

  -- Must escape & prepend this code, otherwise the
  -- styling will spill into other terminal output.
  Reset_Code   : String := "[0m";

  -- Table of ANSI color codes
  -- Use pointers because each string has different
  -- length -- creates ragged array.
  -- Consider using Unbounded strings everywhere.
  Color_Table : array (Color) of ANSI_Code_Handle :=
    (Black   => Black_Code'Access,
     Red     => Red_Code'Access,
     Green   => Green_Code'Access,
     Yellow  => Yellow_Code'Access,
     Blue    => Blue_Code'Access,
     Magenta => Magenta_Code'Access,
     Cyan    => Cyan_Code'Access,
     White   => White_Code'Access);

  Effect_Table : array (Effect) of ANSI_Code_Handle :=
    (Bold       => Bold_Code'Access,
     Faint      => Faint_Code'Access,
     Italic     => Italic_Code'Access,
     Underline  => Underline_Code'Access,
     Slow_Blink => Slow_Blink_Code'Access,
     Fast_Blink => Fast_Blink_Code'Access,
     Strike     => Strike_Code'Access);

  function With_Color  (C : Color;  S : String) return String is
    (ESC & "[" & Color_Table(C).all & "m" & S & ESC & Reset_Code);

  function With_Effect (E : Effect; S : String) return String is
    (ESC & "[" & Effect_Table(E).all & "m" & S & ESC & Reset_Code);

  function Color_Task (Index : Positive; S : String) return String is
  begin
    return With_Color(Color'Val((Index mod No_Of_Tasks) + 1), S);
  end;

  function With_Effect (E : Effect_List; S : String) return String is
    Result    : Unbounded_String := To_Unbounded_String(ESC & "[");
    Str       : Unbounded_String := To_Unbounded_String(S);
    Seperator : Unbounded_String := To_Unbounded_String(";");
    Suffix    : Unbounded_String := To_Unbounded_String("m");
  begin
    for Index in E'Range loop
      Append(Result, To_Unbounded_String(Effect_Table(E(Index)).all));
      if E'Length /= Index then
        Append(Result, Seperator);
      end if;
    end loop;
    Append(Result, Suffix);
    Append(Result, Str);
    return To_String(Result) & ESC & Reset_Code;
  end;

  function Style (C : Color; E : Effect; S : String) return String is
    (ESC & "[" & Color_Table(C).all & ";" & Effect_Table(E).all & "m" & S & ESC & Reset_Code);

  function Style (C : Color; E : Effect_List; S : String) return String is
    Result    : Unbounded_String := To_Unbounded_String(ESC & "[" & Color_Table(C).all & ";");
    Str       : Unbounded_String := To_Unbounded_String(S);
    Seperator : Unbounded_String := To_Unbounded_String(";");
    Suffix    : Unbounded_String := To_Unbounded_String("m");
  begin
    for Index in E'Range loop
      Append(Result, To_Unbounded_String(Effect_Table(E(Index)).all));
      if E'Length /= Index then
        Append(Result, Seperator);
      end if;
    end loop;
    Append(Result, Suffix);
    Append(Result, Str);
    return To_String(Result) & ESC & Reset_Code;
  end;

  function Black   (S : String) return String is (With_Color(Black,  S));
  function Red     (S : String) return String is (With_Color(Red,    S));
  function Green   (S : String) return String is (With_Color(Green,  S));
  function Yellow  (S : String) return String is (With_Color(Yellow, S));
  function Blue    (S : String) return String is (With_Color(Yellow, S));
  function Magenta (S : String) return String is (With_Color(Yellow, S));
  function Cyan    (S : String) return String is (With_Color(Yellow, S));
  function White   (S : String) return String is (With_Color(Yellow, S));

  function Bold       (S : String) return String is (With_Effect(Bold,       S));
  function Faint      (S : String) return String is (With_Effect(Faint,      S));
  function Italic     (S : String) return String is (With_Effect(Italic,     S));
  function Underline  (S : String) return String is (With_Effect(Underline,  S));
  function Slow_Blink (S : String) return String is (With_Effect(Slow_Blink, S));
  function Fast_Blink (S : String) return String is (With_Effect(Fast_Blink, S));
  function Strike     (S : String) return String is (With_Effect(Strike,     S));

  procedure Put_Color (C : in Color; S : in String) is
  begin
    Put_Line(With_Color(C, S));
  end;

  procedure Put_Effect (E : in Effect; S : in String) is
  begin
    Put_Line(With_Effect(E, S));
  end;

  procedure Put_Effect (E : in Effect_List; S : in String) is
  begin
    Put_Line(With_Effect(E, S));
  end;

  procedure Put_Style (C : in Color; E : in Effect; S : in String) is
  begin
    Put_Line(Style(C, E, S));
  end;

  procedure Put_Style (C : in Color; E : in Effect_List; S : in String) is
  begin
    Put_Line(Style(C, E, S));
  end;

  procedure Put_Task  (Index : in Positive; S : in String) is
  begin
    Put_Line(Color_Task(Index, "Task " & Positive'Image(Index) & ": " & S));
  end;

end;
