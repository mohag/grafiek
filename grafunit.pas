{
    Grafunit.pas - Procedures and functions for DosPlot
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
{$IFDEF FPC}
 {$INLINE ON}
 {$DEFINE EXCEPT}  // Free Pascal het exceptions wat gehandle moet word.
                   // Delphi ook maar dit behoort NIE te werk NIE
 {$MODE Delphi}
 {$H-}
 {$GOTO ON}
{$ENDIF}
{$N+}
{$E-}
{$G+}
Unit GrafUnit;

Interface

uses pcalcexp;
{--CONSTANTS------------------------------------------------------------------}
const
    version         = '1.1';
    configfilename  = 'dosplot.cfg';
    logfilename     = 'dosplot.log';
{--TYPES----------------------------------------------------------------------}
Type
{$IFNDEF FPC}
     Shortstring = String;
     Smallint    = Integer;
{$Endif}

{$IFNDEF FPC}
     SettingT = Packed Record
{$ELSE}
     SettingT = Record
{$Endif}
                   x,y      : longint;
                   kleur    : word;
                   skaal    : word;
                   detail   : word;
                   gd,gm    : smallint;
                   linemode : bytebool;
                   bgipath  : shortstring;
                   view     : bytebool;
                End;
     GraphT   = Record
                   postfix      : postft;
                   min          : real;
                   max          : real;
                End;

{--VARS-----------------------------------------------------------------------}
Var maxx, maxy : word;
    gm : smallint;
    gothfont, sansfont : smallint;
{-PROCEDURES AND FUNCTIONS----------------------------------------------------}
function UpperCase(S: string): string;
Procedure FuncInfo;
Procedure Credits;
Procedure Init;
Procedure Asse(sets: settingT);
Procedure Kleurkodes;
Procedure DefSetts(var settings : settingT);
Procedure SaveSettings(fname : string;var sets : settingT);
Procedure LoadSettings(fname : string;var sets : settingT);
Procedure LogEvent(const evt : STRING);
Function Str2Graph(inp : shortstring;var output : GraphT): longint;
Function Num2Str(inp : extended) :string;
Function Int2Str(inp : integer) :string;
Function SafeRound(inp : extended) : longint;
Function BuildString : shortstring;
{-----------------------------------------------------------------------------}
Implementation
{--USES------------------------------------------------------------------------}
uses {$ifdef Win32}crt, windows{$Else}Crt{$endif},graph,dos
{$IfNDEF FPC}
,bgifont3,VGAdriv
{$ENDIF};
{--TYPES----------------------------------------------------------------------}
TYPE
     SettingFT = file of settingT;
{-PROCEDURES AND FUNCTIONS----------------------------------------------------}
function UpperCase(S: string): string;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    s[I] := UpCase(S[i]);
  UpperCase := S;
end;
{-----------------------------------------------------------------------------}
Procedure FuncInfo;
{Shows what can be used at "custom vergelyking"}
BEGIN
  Window(1,10,41,36);
  TextBackground(Green);
  ClrScr;
  Window(2,11,41,36);
  TextColor(White);
  Writeln('Supported Functions:');
  Writeln('--------------------');
  Writeln;
  Writeln('|------------|------------------------|');
  Writeln('|Function:   |Description:            |');
  Writeln('|------------|------------------------|');
  Writeln('|ASIN/ARCSIN | Arcsin of the number   |');
  Writeln('|ACOS/ARCCOS | Arccos of the number   |');
  Writeln('|ATAN/ARCTAN | Arctan of the number   |');
  Writeln('|SIN         | Sine of the number     |');
  Writeln('|COS         | Cosine of the number   |');
  Writeln('|TAN         | Tangent of the number  |');
  Writeln('|COT         | Cotangent of the number|');
  Writeln('|SEC         | 1/cos(x)               |');
  Writeln('|COSEC       | 1/sin(x)               |');
  Writeln('|ABS         | Absolute value of num. |');
  Writeln('|SQRT        | Square root of number  |');
  Writeln('|LN          | Natural log. of number |');
  Writeln('|LOG         | Log of number base 10  |');
  Writeln('|EXP         | e^number; e=2,71828... |');
  Writeln('|SQR         | number^2               |');
  Writeln('|DEG         | Degrees -> Radians     |');
  Writeln('|TRUNC       | Integer part of number |');
  Writeln('|ROUND       | rounds num. to integer |');
  Writeln('---------------------------------------');
  Window(59,10,80,20);
  TextBackground(Blue);
  TextColor(Yellow);
  ClrScr;
  Window(60,11,80,20);
  Writeln('Operators:');
  Writeln('----------');
  Writeln;
  Writeln('+ Addition');
  Writeln('- Substraction');
  Writeln('* Multiplication');
  Writeln('/ Division');
  Writeln('^ Exponetiation');
  Write  ('% Modulus');
  Window(1,1,80,9);
  TextBackground(Black);
  TextColor(7);
  ClrScr;
END;
{-----------------------------------------------------------------------------}
Procedure Credits;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure RGBSetPalette(color,r,g,b : byte);
Begin
   {$Ifdef fpc}
   SetRGBPalette(color,r,g,b);
   {$Else}
   SetRGBPalette(color,round(r/255*63),round(g/255*63),round(b/255*63));
   {$ENDIF}
End;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
{$IFNDEF FPC}
  Function Fade(x,y : word;text : string;color : byte;font,size : byte;
                r,g,b : single;tyd : word) : boolean;
  var   ch     : char;
        i      : integer;
  Begin
     Fade := False;
     SetColor(color);
     SetRGBPalette(color,0,0,0);
     Settextjustify(1,1);
     SettextStyle(font,0,size);
     OuttextXY(x,y,text);
     ch := #0;
     i := 0;
     While (ch <> #27) and (i < 255) do
     Begin
        RGBSetPalette(color,Round(r*i),Round(g*i),Round(b*i));
        Delay(tyd);
        Inc(i);
        If keypressed then ch := Readkey;
     End;
     i := 255;
     While (ch <> #27) and (i >= 0) do
     Begin
        rgbSetPalette(color,Round(r*i),Round(g*i),Round(b*i));
        Delay(tyd);
        Dec(i);
        If keypressed then ch := Readkey;
     End;
     If ch = #27 then Fade := true;
  End;
{$ENDIF}
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
var oldpal : PaletteType;
    oldcol : word;
    run    : boolean;
    cancel : boolean;
Begin
   SetGraphMode(gm);
   GetPalette(oldpal);
   oldcol := GetColor;
   ClearViewPort;
   {$IFNDEF FPC}
   SettextStyle(0,0,1);
   SetColor(7);
   RGBSetPalette(7,100,100,100);
   Settextjustify(2,0);
   OuttextXY(maxx,maxy,'Press Esc to continue...');
   Delay(200);
   {StartUp procedure Finished}
 { Function Fade(x,y,text,color,font,size,r,g,b,tyd):boolean(is gecancel)}
   Run := true;
   cancel := Fade(maxx div 2,maxy div 2,'DosPlot version '+version,1,4,5,0,0.5,1,2);
   Run := not(Cancel);
   If run then
    Begin
      cancel := Fade(maxx div 2,maxy div 2,'is proudly presented',2,3,4,0,0,1,2);
      Run := not(Cancel);
    End;
   If run then
    Begin
      cancel := Fade(maxx div 2,maxy div 2,'by',3,3,4,0,1,0,10);
      Run := not(Cancel);
    End;
   If run then
    Begin
      cancel := Fade(maxx div 2,maxy div 2,'Gert van den Berg',4,3,4,1,0,0,10);
      Run := not(Cancel);
    End;
   if keypressed then readkey;
   {$ENDIF}
   ClearViewPort;
   SettextStyle(0,0,1);

   SetColor(1);
   RGBSetPalette(1,220,0,0);
   Settextjustify(0,2);
   OuttextXy(0,0  ,'DosPlot version '+version);

   SetColor(2);
   OuttextXy(0,10  ,buildstring);

   RGBSetPalette(2,180,180,180);
   Settextjustify(0,2);
   OuttextXy(0,40 ,'Copyright (C) 2004  Gert van den Berg');
   OuttextXy(0,60 ,'This program is free software; you can redistribute it and/or modify');
   OuttextXy(0,70 ,'it under the terms of the GNU General Public License as published by');
   OuttextXy(0,80 ,'the Free Software Foundation; either version 2 of the License, or');
   OuttextXy(0,90 ,'(at your option) any later version.');

   OuttextXy(0,110,'This program is distributed in the hope that it will be useful,');
   OuttextXy(0,120,'but WITHOUT ANY WARRANTY; without even the implied warranty of');
   OuttextXy(0,130,'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
   OuttextXy(0,140,'GNU General Public License for more details.');

   OuttextXy(0,160,'You should have received a copy of the GNU General Public License');
   OuttextXy(0,170,'along with this program; if not, write to the Free Software');
   OuttextXy(0,180,'Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA');

   OuttextXy(0,200,'This license can be found in license.txt');
   SetColor(3);
   RGBSetPalette(3,255,255,255);
   SetColor(4);
   RGBSetPalette(4,220,220,220);
   Settextjustify(0,0);
   OuttextXy(0,maxy,'Press any key to return to main menu...');
   Readkey;
   {ShutDown procedure starting}
   SetAllPalette(oldpal);
   SetColor(oldcol);
   RestoreCrtMode;
End;
{-----------------------------------------------------------------------------}
Procedure Init;
var c          : smallint;
    ch         : char;
    ec         : smallint;
    sets       : settingT;
    DirInfo    : Searchrec;
    registered : byte;
    VGADrv     : smallint;
    oldpath    : string;
    vesa       : boolean;
{$IFDEF FPC}
    hi,lo      : smallint;
{$ENDIF}
Begin
   maxx := 0;
   maxy := 0;
   LoadSettings(configfilename,sets);
   vesa := false;
   VGADRV := 9;
   SANSFont := 3;
   GothFont := 4;
   registered := 1;
   oldpath := sets.bgipath;

{$IFNDEF FPC} {Not needed in Free Pascal}
   VGADrv := RegisterBGIdriver(@EGAVGADriverProc);
   if VGADrv < 0 then
     registered := 0;{}

   GothFont := RegisterBGIfont(@GothicFontProc);
   if gothfont < 0 then
     registered := 0;{}
{   if RegisterBGIfont(@LittFontProc) < 0 then
     registered := false;{}
   sansfont := RegisterBGIfont(@SansFontProc);
   if Sansfont < 0 then
     registered := 0;{}
{   if RegisterBGIfont(@TriplexFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@BoldFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@EuroFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@LcomFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@ScriFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@SimpFontProc) < 0 then
     registered := false;
   if RegisterBGIfont(@TscrFontProc) < 0 then
     registered := false;{}

   If (paramStr(1) = '/G') or (paramStr(1) = '/g') then
    begin
        Writeln('Which video driver would you like to use? ');
        Writeln('  0) VESA');
        Writeln('  9) VGA');
        Write('>');
        ch := readkey;
        Val(ch,sets.gd,c);
      If c <> 0 then Halt;
      Case sets.gd of
        0   :
         begin
           ClrScr;
           Writeln('Which video mode would you like to use? ');
           Writeln('  0) 320x200');
           Writeln('  1) 640x200');
           Writeln('  2) 640x350');
           Writeln('  3) 640x480');
           Writeln('  4) 800x600');
           Writeln('  5) 1024x768');
           Writeln('  6) 1280x1024');
           Write('>');
           ch := readkey;
           Val(ch,sets.gm,c);
           sets.GD := InstallUserDriver('SVGA16', Nil);{}
           vesa := true;;
         end;
        9   :
         begin
           ClrScr;
           Writeln('Which video mode would you like to use? ');
           Writeln('  0) 640x200');
           Writeln('  1) 640x350');
           Writeln('  2) 640x480');
           Write('>');
           ch := readkey;
           if ch = '9' then ch := '2';
           Val(ch,sets.gm,c);
           If (registered = 1) then
            Begin
               sets.bgipath := '';
           {
               sets.gd := VGADrv;{}
               sets.gd := 9;
            End
             else
              sets.gd := 9;
         end;
        else
         Writeln('Invalid driver...');
         Readkey;
         Halt(1);
      End;{Case}
      If c <> 0 then Halt;
    end;       (* *)

   If ((not(registered=1)) or (vesa)) then FindFirst(sets.bgipath+'*.BGI', AnyFile, DirInfo)
    else sets.bgipath := '';
   while (DosError <> 0) do
   Begin
      ClrScr;{}
      Writeln('BGI files not found. Please enter path. (e.g. C:\BGI\) Type "exit" to quit.');
      Write('>');
      Readln(sets.bgipath);
      If UpperCase(sets.bgipath) = 'EXIT' then halt;
      If sets.bgipath[length(sets.bgipath)] <> '\' then sets.bgipath := sets.bgipath + '\';
      FindFirst(sets.bgipath+'*.BGI', AnyFile, DirInfo);
      oldpath := sets.bgipath;
   End;
{$ELSE}   // In Free Pascal
   If (Uppercase(paramStr(1)) = '/G') then
    Begin
        Writeln('Which colour depth do you want to use? ');
        Writeln(' 13) 16 colours');
        Writeln(' 15) 256 colours');
        Writeln(' 17) 32768 colours (15 bit)');
        Writeln(' 18) 65536 colours (16 bit)');
        Writeln;
        Writeln('256 colours are recommended as some video cards don''t support 16 colors at');
        Writeln('high resolutions.');
        Writeln;
        Writeln('256 colour mode selected.  Other modes disabled in ths version.');
        sets.gd := 15;
(*        Repeat
           Write('>');
           {$I-}readln(sets.gd);{$I+}
        Until (IOResult = 0) and (sets.gd in [13,15,17,18]);*)
        GetModeRange(sets.gd,lo,hi);
        Writeln('Which resolution would you like to use? ');
        Writeln(' -1) Cancel');
        Writeln('  0) Auto');
        For c := lo to hi do
         Case c of
           1 : Writeln(c :3,') 320x200');
           2 : Writeln(c :3,') 640x400');
           3 : Writeln(c :3,') 640x480');
           4 : Writeln(c :3,') 800x600');
           5 : Writeln(c :3,') 1024x768');
           6 : Writeln(c :3,') 1280x1024');
           7 : Writeln(c :3,') 1600x1200 ?');
           8 : Writeln(c :3,') 2048x1536 ?');
         Else
            Writeln (c :3,') UnKnown mode');
         End;
        Writeln;
        Writeln('Modes may differ at colour depths other than 8bit (256 colour).');
        If (hi = -1) then writeln('No Modes supported at this color depth.')
          else Writeln('Lowest supported mode: ',lo ,' Highest supported mode: ',hi);
        Repeat
           Write('>');
           {$I-}readln(sets.gm);{$I+}
        Until (IOResult = 0) and ((sets.gm = -1) or (sets.gm in [0,lo..hi]));
        If sets.gm = -1 then halt;
        If sets.gm = 0 then sets.gm := 30000;
    End;
{$ENDIF}

   Repeat
      InitGraph(sets.gd,sets.gm,sets.bgipath{});
      {$ifdef Win32}
      ShowWindow(GetActiveWindow,0);
      {$endif}
      ec := graphResult;
      If ec <> GrOK then
      Begin
         Writeln(GraphErrorMsg(ec));
         Writeln('Run GRAFIEK /G to change video mode');
         LogEvent('Error while initiliazing graphics.');
         LogEvent(GraphErrorMsg(ec));
         Logevent('Settings:');
         LogEvent('Driver: '+Int2str(sets.gd)+' '+Getdrivername);
         LogEvent('Mode: '+Int2str(sets.gm)+' ' + Getmodename(sets.gm));
         {$IFNDEF FPC}
         If ec = grFileNotFound then
          Begin
             Repeat
                Writeln('BGI files not found in ',oldpath,'. Please enter path. (e.g. C:\BGI\) Type "exit" to quit.');
                Write('>');
                Readln(sets.bgipath);
                If UpperCase(sets.bgipath) = 'EXIT' then halt;
                If sets.bgipath[length(sets.bgipath)] <> '\' then sets.bgipath := sets.bgipath + '\';
                FindFirst(sets.bgipath+'*.BGI', AnyFile, DirInfo);
                oldpath := sets.bgipath;
                ClrScr;{}
             Until (DosError = 0);
             Writeln('Please restart program');
             SaveSettings(configfilename,sets)
          End else
            Halt(1);
         {$ELSE}
         Halt(1);
         {$ENDIF}
      End;
      MaxX := Getmaxx; Maxy := getmaxy;
   Until ec = GrOk;
   if sets.x = 0 then sets.x := maxx div 2;
   if sets.y = 0 then sets.y := maxy div 2;
   sets.bgipath := oldpath;
   SaveSettings(configfilename,sets);
   gm := sets.gm;
End; (* *)
{----------------------------------------------------------------------------}
Procedure Asse(sets: settingT);
var oukleur : word;
    outs    : textsettingsType;
    a       : integer;
Begin
   OuKleur := GetColor;
   GettextSettings(outs);
   ClearDevice;
   If sets.skaal > 1 then
    Begin
       SetColor(DarkGray);
       a := sets.x;
       While a <= maxX do
        Begin
           line (a,0,a,MaxY);
           a:= a + sets.skaal;
        End;
       a := sets.x;
       While a >= 0 do
        Begin
           line (a,0,a,MaxY);
           a:= a - sets.skaal;
        End;
       a := sets.y;
       While a < maxY do
        Begin
           line (0,a,MaxX,a);
           a:= a + sets.skaal;
        End;
       a := sets.y;
       While a >= 0 do
        Begin
           line (0,a,MaxX,a);
           a:= a - sets.skaal;
        End;
    End;
   SetColor(White);
   Line(sets.x,0,sets.x,maxy);
   Line(0,sets.y,maxx,sets.y);

   Settextstyle(0,0,1); SetColor(green);
   SettextJustify(LeftText, Toptext); OuttextXY(sets.x+2,0,'Y');
   SettextJustify(RightText, Toptext); OuttextXY(maxx,sets.y+2,'X');
   with outs do
    Begin
       SetColor(oukleur);
       SettextStyle(font,direction,Charsize);
       SettextJustify(horiz,vert);
    End;
End;
{----------------------------------------------------------------------------}
Procedure Kleurkodes;
Begin
   Window(60,10,80,27);
   TextBackground(Blue);
   ClrScr;
   Window(61,11,80,27);
   ClrScr;
   TextColor(White);
   WriteLn('0  Black');
   WriteLn('1  Blue');
   WriteLn('2  Green');
   WriteLn('3  Cyan');
   WriteLn('4  Red');
   WriteLn('5  Magenta');
   WriteLn('6  Brown');
   WriteLn('7  LightGray');
   WriteLn('8  DarkGray');
   WriteLn('9  LightBlue');
   WriteLn('10 LightGreen');
   WriteLn('11 LightCyan');
   WriteLn('12 LightRed');
   WriteLn('13 LightMagenta');
   WriteLn('14 Yellow');
   WriteLn('15 White');
end;
{-----------------------------------------------------------------------------}
Procedure DefSetts(var settings : settingT);
Begin
      Settings.X := MaxX div 2;
      Settings.Y := MaxY div 2;
      Settings.skaal := 15;
      Settings.kleur := LightGreen;
      settings.detail := 50;
      settings.linemode := true;
      settings.view     := true;
end;
{----------------------------------------------------------------------------}
Procedure SaveSettings(fname : string;var sets : settingT);
var lr   : settingFT;
Begin
   Assign(lr,fname);
   Rewrite(lr);
   Write(lr, sets);
   Close(lr);
   LogEvent('Settings saved to '+fname);
End;
{----------------------------------------------------------------------------}
Procedure LogEvent(const evt : STRING);
var txt : text;
    h,min,s,hs,y,m,d : word;
BEGIN
  GetDate(y,m,d,hs);
  GetTime(h,min,s,hs);
  Assign(txt,logfilename);
  {$I-}Append(txt);{$I+}
  IF IOResult <> 0 then
   Begin
      REwrite(txt);
      Writeln(txt,'DosPlot version '+version+' log file');
      Writeln(txt,'------------------------------')
   End;
  Write(txt,d:2,'/',m:2,'/',y:4,' ',h:2,':',min:2,':',s:2,'.',hs:2,'  ');
  Writeln(txt,evt);
  {$I-}Close(txt);{$I+}
END;
{----------------------------------------------------------------------------}
Procedure LoadSettings(fname : string;var sets : settingT);
var lr   : settingFT;
    i    : byte;
label fixfile;
Begin
   Assign(lr,fname);
   {$I-}Reset(lr);{$I+}
   For i := 1 to 255 do
       sets.bgipath[i] := #0;
   If IOResult <> 0 then
    Begin
      fixfile:
      Rewrite(lr);
      DefSetts(sets);
      sets.gd     := 9;
      sets.gm     := 2;
      sets.bgipath := '.\bgi\';
      Write(lr,sets);
      LogEvent('Settings file "'+fname+'" corrupt or missing.  Repaired.');
    end
      else
       Begin
          {$I-}
          Read(lr,sets);
          {$I+}
          If IOresult <> 0 then goto fixfile
           else Logevent('Settings loaded from '+fname);
       End;
{   if maxx < 10 then sets.x := 319;
   if maxy < 10 then sets.y := 239;}
   Close(lr);
End;
{----------------------------------------------------------------------------}
Function Str2Graph(inp : shortstring;var output : GraphT): longint;
var
  return        : longint;
  retdetail     : Record
                     pcalc : smallint;
                     s2g   : smallint;
                  end absolute return;
  apart         : stringStack;
  postf         : postft;
  i             : integer;
  ts            : shortstring;
  tmpr          : extended; {Temp real}
  x,y,z         : extended;

label cleanup;

Begin
    x := 0;
    y := 0;
    z := 0;
    i := 0;
    retdetail.pcalc := 0;
    retdetail.s2g   := 0;
    apart.clear;
    postf.clear;
    ts := '';
    inp := uppercase(inp);
    output.min := -99999;
    output.max :=  99999;
    output.postfix.clear;

    LogEvent('Processing: '+inp);

    For i := 1 to length(inp) do
        If inp[i] <> '|' then
            ts := ts + inp[i]
        else
        Begin
            Apart.push(ts);
            ts := '';
        End; {If / else / For}
    Apart.push(ts);

    {$IFDEF EXCEPT}
    try
        pcalcexp.string2postf(apart.items[1],postf);
    except
    end;
    {$ELSE}
    pcalcexp.string2postf(apart.items[1],postf);
    {$ENDIF EXCEPT}
    apart.delete(1);

    retdetail.pcalc := pcalcexp.calcresult;

    if (return <> 0) then  {On fatal error}
        Begin
            LogEvent('Error processing '+inp);
            GOTO cleanup;
        End
    else
        LogEvent(inp+' converted to postfix');

    output.postfix  := postf;
    Case (apart.count) of
        0 : LogEvent('Domain not specified.');
        2 :
            Begin
                LogEvent('Finding minimum value of domain');
                {$IFDEF EXCEPT}
                try
                    string2postf(apart.items[1],postf);
                except
                end;
                {$ELSE}
                string2postf(apart.items[1],postf);
                {$ENDIF EXCEPT}
                i := pcalcexp.calcresult;
                if i = 0 then
                    begin
                        {$IFDEF EXCEPT}
                        try
                            tmpr := calcpostfix(postf,x,y,z);
                        except
                        end;
                        {$ELSE}
                        tmpr := calcpostfix(postf,x,y,z);
                        {$ENDIF EXCEPT}
                        If calcerror <> 0 then
                            Begin
                                LogEvent('Error finding minimum value of domain');
                                retdetail.s2g := calcresult {error in domain}
                            End
                        else
                            Begin
                                LogEvent('Found minimum value of domain');
                                output.min := tmpr;
                            End;
                    end else
                        LogEvent('Invalid minimum value specified');

                if retdetail.pcalc = 0 then
                    Begin
                        LogEvent('Finding maximum value of domain');
                        {$IFDEF EXCEPT}
                        try
                            string2postf(apart.items[2],postf);
                        except
                        end;
                        {$ELSE}
                        string2postf(apart.items[2],postf);
                        {$ENDIF EXCEPT}
                        i := pcalcexp.calcresult;
                        if i = 0 then
                            begin
                                {$IFDEF EXCEPT}
                                try
                                    tmpr := calcpostfix(postf,x,y,z);
                                except
                                end;
                                {$ELSE}
                                tmpr := calcpostfix(postf,x,y,z);
                                {$ENDIF EXCEPT}
                                If calcerror <> 0 then
                                    Begin
                                        LogEvent('Error finding maximum value of domain');
                                        retdetail.s2g := calcresult {error in domain}
                                    End
                                else
                                    Begin
                                        LogEvent('Found maximum value of domain');
                                        output.max := tmpr;
                                    End;
                            end else
                                LogEvent('Invalid maximum value specified');
                    End; {max calculation}
            End;
    else {case: if not 0 or 2}
        retdetail.s2g := -1;
    end;

CLEANUP:
    str2graph := return;
End;
{----------------------------------------------------------------------------}
Function Num2Str(inp : extended) :string;
var out : string;
Begin

   Str(inp:0:5,out);
   num2str := out;
End;
{----------------------------------------------------------------------------}
Function Int2Str(inp : integer) :string;
var out : string;
Begin

   Str(inp,out);
   int2str := out;
End;
{----------------------------------------------------------------------------}
Function SafeRound(inp : extended) : longint;
Begin
   If (inp < 2147483647) and (inp > -2147483647) then
     SafeRound := Round(inp) else
      If inp >= 2147483647 then SafeRound := 2147483647
       else SafeRound := -2147483647;
End;
{----------------------------------------------------------------------------}
Function BuildString : shortstring;
Begin
    {$IFDEF FPC}
    {$IFDEF WIN32}
     Buildstring := 'Free Pascal Win32 build';
    {$ENDIF}
    {$IFDEF GO32V2}
     Buildstring := 'Free Pascal Go32v2 build';
    {$ENDIF}
    {$IFDEF LINUX}
     Buildstring := 'Free Pascal Linux build';
    {$ENDIF}
   {$ENDIF}
   {$IFDEF Ver70}
    {$IFDEF WINDOWS}
     Buildstring := 'Borland Pascal Windows build';
    {$ENDIF}
    {$IFDEF MSDOS}
     Buildstring := 'Borland / Turbo Pascal Real Mode build';
    {$ENDIF}
    {$IFDEF DPMI}
     Buildstring := 'Borland Pascal Protected mode build';
    {$ENDIF}
   {$ENDIF}
   {$IFDEF DELPHI}
     Buildstring := 'Borland Delphi build';
   {$ENDIF}
End;
{----------------------------------------------------------------------------}
end.
