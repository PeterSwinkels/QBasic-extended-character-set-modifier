DEFINT A-Z

TYPE RegTypeX
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE

DECLARE SUB CGADemo ()
DECLARE SUB EGAVGADemo ()
DECLARE SUB Main ()

CONST CGA = &H1F
CONST EGAVGA = &H43
CONST FALSE = 0
CONST SIZECGA = &H400
CONST SIZEEGAVGA = &H1000
CONST TRUE = -1

DECLARE FUNCTION GetFonts$ (IsCGA, Segment, Offset)
DECLARE FUNCTION InvertFonts$ (Fonts$)
DECLARE SUB DisplayExtendedCharacters ()
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB RestoreDefault (IsCGA, Segment, Offset)
DECLARE SUB SetFonts (IsCGA, Fonts$)

Main

SUB CGADemo
 SCREEN 1
 CLS
 COLOR , 1

 Fonts$ = GetFonts$(TRUE, Segment, Offset)
 Fonts$ = InvertFonts$(Fonts$)
 SetFonts TRUE, Fonts$
 PRINT "CGA"
 PRINT
 DisplayExtendedCharacters
 RestoreDefault TRUE, Segment, Offset
END SUB

SUB DisplayExtendedCharacters
 FOR Character = 128 TO 255
  PRINT CHR$(Character);
 NEXT Character
END SUB

SUB EGAVGADemo
 SCREEN 13
 CLS
 COLOR 7

 Fonts$ = GetFonts$(FALSE, Segment, Offset)
 Fonts$ = InvertFonts$(Fonts$)
 SetFonts FALSE, Fonts$
 PRINT "EGA/VGA"
 PRINT
 DisplayExtendedCharacters
 RestoreDefault FALSE, Segment, Offset
END SUB

FUNCTION GetFonts$ (IsCGA, Segment, Offset)
DIM Registers AS RegTypeX

 IF IsCGA THEN
  Registers.ax = &H3500 OR CGA
  Size = SIZECGA
 ELSE
  Registers.ax = &H3500 OR EGAVGA
  Size = SIZEEGAVGA
 END IF

 INTERRUPTX &H21, Registers, Registers

 Fonts$ = ""
 Offset = Registers.bx
 Segment = Registers.es

 DEF SEG = Segment
 
 FOR Position = Offset TO (Offset + Size) - &H1
  Fonts$ = Fonts$ + CHR$(PEEK(Position))
 NEXT Position

GetFonts$ = Fonts$
END FUNCTION

FUNCTION InvertFonts$ (Fonts$)
 Inverted$ = Fonts$
 FOR Position = 1 TO LEN(Fonts$)
  MID$(Inverted$, Position, 1) = CHR$(&HFF XOR ASC(MID$(Inverted$, Position, 1)))
 NEXT Position

 InvertFonts$ = Inverted$
END FUNCTION

SUB Main
 CGADemo
 DO: LOOP WHILE INKEY$ = ""
 EGAVGADemo
END SUB

SUB Quit
 SCREEN 0
 COLOR 7, 0
 CLS
 WIDTH 80, 25
 SYSTEM
END SUB

SUB RestoreDefault (IsCGA, Segment, Offset)
DIM Registers AS RegTypeX

 IF IsCGA THEN
  Registers.ax = &H2500 OR CGA
 ELSE
  Registers.ax = &H2500 OR EGAVGA
 END IF

 Registers.dx = Offset
 Registers.ds = Segment
 INTERRUPTX &H21, Registers, Registers
END SUB

SUB SetFonts (IsCGA, Fonts$)
DIM Registers AS RegTypeX

 IF IsCGA THEN
  Registers.ax = &H2500 OR CGA
 ELSE
  Registers.ax = &H2500 OR EGAVGA
 END IF

 Registers.dx = SADD(Fonts$)
 Registers.ds = VARSEG(Fonts$)
 INTERRUPTX &H21, Registers, Registers
END SUB

