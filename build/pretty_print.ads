
package Pretty_Print is
  type Color is (
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White
  );

  procedure Put_Black(Line : String);
  procedure Put_Red(Line : String);
  procedure Put_Green(Line : String);
  procedure Put_Yellow(Line : String);
  procedure Put_Blue(Line : String);
  procedure Put_Magenta(Line : String);
  procedure Put_Cyan(Line : String);
  procedure Put_White(Line : String);

  procedure Put_Color(C : Color; Line : String);
end;
