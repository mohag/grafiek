{*DEFINE DEBUG} {Change to $Define for more extensive logging}
{$IFDEF FPC}
 {$INLINE ON}
 {$MODE DELPHI}
 {$H-}
 {$GOTO ON}
{$ENDIF}
{$N+}
{$E-}
{$G+}
{
    ScrSave.pas - Unit to save graphmode screen and switch between txt and
                  graphmode
    Copyright (C) 2004  Gert van den Berg

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}
Unit ScrSave;

Interface

uses grafunit;

var scrgmode : boolean;

Procedure TxtMode(fname     : string);{fname = filename sonder extention}
Procedure GraphMode(fname : string);{fname = filename sonder extention}
Procedure DeletescrFiles(fname : string);
Procedure SaveScreen(fname : string;Settings : SettingT);
{Default extension: .dmp}
Procedure LoadScreen(fname : string);
{Extension MUST be specified}
Procedure FastLoad(fname : string);
Procedure FastSave(fname : string);
{Saves to format that can only read by FastLoad}

Implementation

uses graph{$IFDEF WIN32},crt{$ENDIF};

const
{$IFDEF FPC}
    lynTSize = 32*1024;
{$ELSE}
    lynTSize = 32700;
{$ENDIF}

Type
     lynT        = Array[1..lynTSize]of byte;
     LynFT    = File of LynT;
     headerT  =     {header of data file}
        packed record
            ftype   :   STRING[6];
            xsize   :   word;
            ysize   :   word;
            scale   :   word;
            xpos    :   longint;
            ypos    :   longint;
        End; {(packed) record}
     pixelT   =     {Format to save pixel}
        packed record
            x       :   word;
            y       :   word;
            color   :   word;
        End; {(packed) record}


{----------------------------------------------------------------------------}
Procedure FastSave(fname : String);
Var
  Lyn             : LynT;
  LynF            : LynFT;
  i,z,a,b,s       : Longint;
  txt             : Text;
Begin
      s := Sizeof(lyn) div ImageSize(0,0,MaxX,0);{Hoeveelheid lyne wat in lyn pas}
      Assign(lynF,fname+'.tmp');
      Assign(txt,fname+'.txt');
      z := (((MaxY+1) div s) +1);{Hoeveelheid keer wat s lyne op skerm voorkom}
      {$IFDEF DEBUG}
      LogEvent('S = '+Int2Str(s)+'  Z = '+Int2str(Z)+' MaxY = '+Int2Str(MaxY));
      {$ENDIF}
      {$I-}Rewrite(lynF);
      IF (IOREsult <> 0) then LogEvent('Error opening '+fname+'.tmp');
      Rewrite(txt);{$I+}
      IF (IOREsult <> 0) then LogEvent('Error opening '+fname+'.txt');
      Writeln(txt,s);
      Close(Txt);

      For i := 1 to Z do
      Begin
         a := (i*s)-s;
         if (i*s < MaxY) then b := (i*s-1) else b := MaxY;
         GetImage(0,a,MaxX,b,lyn);
         Write(LynF,lyn);
      End;
      Close(LynF);
End;
{----------------------------------------------------------------------------}
Procedure FastLoad(fname : String);
Var
  Lyn  : LynT;
  LynF : LynFT;
  i    : word;
  txt  : Text;
  s    : byte;
  ts   : string;
  ec   : integer;
Begin
   Assign(txt,fname+'.txt');
   Assign(lynF,fname+'.tmp');
   Reset(LynF);
   {$I-}Reset(txt);{$I+}
   If IORESult = 0 then
   Begin
      Readln(txt,ts);
      Close(Txt);
      Val(ts,s,ec);
   End else
   Begin
     LogEvent('Error opening '+fname+'.txt');
     s := MaxY+1;

   End;

   If ec <> 0 then LogEvent ('Unexpected error');

   i := 1;
   While not(EoF(LynF)) Do
   Begin
      Read(LynF,lyn);
      PutImage(0,((i-1)*s),lyn,NormalPut);
      Inc(i);
   End;
   Close(LynF);
End;
{----------------------------------------------------------------------------}
Procedure TxtMode(fname     : string);{fname = filename sonder extention}
Begin
    LogEvent('Switching to textmode');
    If scrgmode then
        Begin
            scrgmode := false;
{$IFNDEF WIN32}
            FastSave(fname);
            RestoreCrtMode;
{$ELSE}
            Window(1,1,80,25);
            TextBackGround(Black);
            ClrScr;
{$ENDIF WIN32}
            LogEvent('Switched to textmode');
        End else
            LogEvent('Already in textmode');
End;
{----------------------------------------------------------------------------}
Procedure GraphMode(fname : string);{fname = filename sonder extention}
Begin
    LogEvent('Switching to graphmode');
{$IFNDEF WIN32}
    If (scrgmode) then
        LogEvent('Already in graphmode')
    else
        SetGraphMode(gm);
    scrgmode := true;
    FastLoad(fname);
{$ENDIF WIN32}
    LogEvent('Switched to graphmode');
End;
{----------------------------------------------------------------------------}
Procedure DeletescrFiles(fname : string);
Var
  LynF : LynFT;
  txt  : Text;
  dmp  : Integer;
Begin
   Assign(txt,fname+'.txt');
   Assign(lynF,fname+'.tmp');
   {$I-}
   Erase(txt);
   Erase(LynF);
   {$I+}
   dmp := IOREsult;
End;
{----------------------------------------------------------------------------}
Function Factor(y,x : longint) : boolean;
Begin
    if X <> 0 then
        factor := ((y mod (x)) = 0)
    else
        factor    :=  false;
End;
{----------------------------------------------------------------------------}
{
File Format:
------------
Pixel format
Offset:     |Size:  |Type           |Description:               |
------------|-------|---------------|---------------------------|
00h = 00    | 7     |String[6]      |Header = 'SCRDMP'          |
07h = 07    | 2     |word           |horizontal screen size     |
09h = 09    | 2     |word           |vertical screen size       |
0Bh = 11    | 2     |word           |scale in pixels per unit   |
0Ch = 13    | 4     |longint        |x posision of y axis       |
11h = 17    | 4     |longint        |y posision of x axis       |
15h = 21    | 6*x   |pixelT         |pixel description          |
??h = ??    | 6     |pixelT         |00h x 6 to indicate EoF    |
-----------------------------------------------------------------
Header size (excluding data):   21

PixelT format:
--------------
Offset:     |Size:  |Type           |Description:               |
------------|-------|---------------|---------------------------|
00h = 00    | 2     |word           |horizontal pixel posision  |
02h = 02    | 2     |word           |vertical pixel posision    |
04h = 04    | 2     |word           |color of pixel             |
-----------------------------------------------------------------
Total size:   6
}
Procedure SaveScreen(fname : string;Settings : SettingT);
var
    Header  :   HeaderT;
    pixel   :   PixelT;
    x,y     :   longint;
    dmp     :   file;
    dgcolor :   Word;
label FuncEnd,FileClose;
Begin
    PutPixel(maxx,maxy,DarkGray);
    dgColor := GetPixel(maxx,maxy);
    Header.Ftype    :=  'SCRDMP';
    Header.xsize    :=  maxX+1;
    Header.ysize    :=  maxY+1;
    Header.scale    :=  Settings.skaal;
    Header.xpos     :=  Settings.x;
    Header.ypos     :=  Settings.y;
    if (pos('.',fname) = 0) then
        fname := fname + '.dmp';
    Filemode := 1;{Write only}
    {$I-}Assign(dmp,fname);{$I+}
    If (IOResult <> 0) then
        Begin
            LogEvent('Error saving screen: Cannot assign '+fname);
            GOTO FileClose;
        End;
    {$I-}ReWrite(dmp,1);{$I+}
    If (IOResult <> 0) then
        Begin
            LogEvent('Error saving screen: Cannot open '+fname+' for writing');
            Goto FuncEnd;
        End;
    If (IOResult = 0) then
        {$I-}BlockWrite(dmp,Header,sizeof(header));{$I+}
    If (IOResult <> 0) then
        Begin
            LogEvent('Error saving screen: Cannot write header');
            GOTO FileClose;
        End;

    for y := 0 to GetmaxY do
        for x := 0 to GetmaxX do
            Begin
                Pixel.color := Getpixel(x,y);
                if (Pixel.color <> Black) and
                not (
                    (Pixel.color = dgcolor)
                    and (
                        (factor(x-settings.x,settings.skaal)
                        or
                        factor(y-settings.y,settings.skaal))

                        and(
                            (x-settings.x <> 0)
                            or
                            (y-settings.y <> 0)
                        )
                    )
                ) then
                    Begin
                        pixel.x := x;
                        pixel.y := y;
                        {$I-}BlockWrite(dmp,Pixel,Sizeof(pixel));{$I+}
                        If (IOResult <> 0) then
                            Begin
                                LogEvent('Error saving screen: Cannot write'+
                                   ' pixel. X='+Int2Str(x)+' Y='+Int2Str(y));
                                Goto FileClose;
                            End;
                    End;
            End;{For x;for y}
    FillChar(pixel,Sizeof(pixel),0);
    {$I-}BlockWrite(dmp,Pixel,Sizeof(pixel));{$I+}
    If (IOResult <> 0) then
        Begin
            LogEvent('Error saving screen: Cannot write EoF marker.');
            Goto FileClose;
        End;
FileClose:
    {$I-}Close(dmp);{$I+}
    If (IOResult <> 0) then
        LogEvent('Error saving screen: Cannot close file');
FuncEnd:
    Filemode := 2;{Read-Write}
End;
{----------------------------------------------------------------------------}
Procedure LoadScreen(fname : string);
var
    Header  :   HeaderT;
    pixel   :   PixelT;
    dmp     :   file;
    Sets    :   SettingT;
label FuncEnd,FileClose;
Begin
    FillChar(Sets,Sizeof(Sets),0);
    Filemode := 0;
    {$I-}Assign(dmp,fname);
    If (IOResult <> 0) then
        Begin
            LogEvent('Error loading screen: Cannot assign '+fname);
            GOTO FileClose;
        End;
    Reset(dmp,1);{$I+}
    If (IOResult = 0) then
        {$I-}BlockRead(dmp,Header,Sizeof(header)){$I+}
    else
        Begin
            LogEvent('Error loading screen: Cannot open '+fname+' for reading');
            Goto FuncEnd;
        End;
    If (IOResult <> 0) then
        Begin
            LogEvent('Error loading screen: Cannot read header');
            GOTO FileClose;
        End;
    If header.ftype <> 'SCRDMP' then
        Begin
            LogEvent('Invalid filetype');
            goto FileClose;
        End;
    sets.skaal  :=  Header.scale;
    sets.x      :=  Header.xpos;
    sets.y      :=  Header.ypos;
    If ((header.xsize-1) > maxX) or ((header.ysize-1) > maxY) then
        LogEvent('Warning loading screen: Saved screen too large for screen');
    Asse(Sets);
    Repeat
        {$I-}BlockRead(dmp,Pixel,SizeOf(pixel));{$I+}
        If (IOResult <> 0) then
            Begin
                LogEvent('Error loading screen: Cannot read pixel.');
                Goto FileClose;
            End;
            if (pixel.x <> 0) or (pixel.y <> 0) or (pixel.color <> 0) then
                PutPixel(pixel.x,pixel.y,pixel.color);
            If (pixel.x > maxX) or (pixel.y > maxY) then
                LogEvent('Warning loading screen: Offscreen'+
                         ' pixel detected');
    Until ((pixel.x = 0) and (pixel.y = 0) and (pixel.color = 0)) or EoF(dmp);
    If (pixel.x <> 0) or (pixel.y <> 0) or (pixel.color <> 0) then
        LogEvent('Error loading screen: EoF marker not found where expected.');
FileClose:
    {$I-}Close(dmp);{$I+}
    If (IOResult <> 0) then
        LogEvent('Error loading screen: Cannot close file');
FuncEnd:
    Filemode := 2;{Read-Write}
End;
{----------------------------------------------------------------------------}
Begin
   scrgmode := false;
end.
