{*DEFINE DEBUG} {Change to $Define for more extensive logging}
{$IFDEF FPC}
 {$INLINE ON}
 {$MODE DELPHI}
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

    E-Mail: gertvdb@global.co.za

}
Unit ScrSave;

Interface

var scrgmode : boolean;

Procedure TxtMode(fname : string);{fname = filename sonder extention}
Procedure GraphMode(fname : string);{fname = filename sonder extention}
Procedure DeletescrFiles(fname : string);

Implementation

uses graph,grafunit;

const
{$IFDEF FPC}
    lynTSize = 32*1024;
{$ELSE}
    lynTSize = 32700;
{$ENDIF}

Type
     lynT        = Array[1..lynTSize]of byte;
     LynFT    = File of LynT;


Procedure TxtMode(fname : string);{fname = filename sonder extention}
Var
  Lyn             : LynT;
  LynF            : LynFT;
  i,z,a,b,s       : Longint;
  txt             : Text;
Begin
{$IFNDEF WIN32}
   LogEvent('Switching to textmode');
   If scrgmode then
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
      scrgmode := false;
      RestoreCrtMode;
      LogEvent('Switched to textmode');
   End else
       LogEvent('Already in textmode');
{$ENDIF}
End;
{----------------------------------------------------------------------------}
{
}
Procedure GraphMode(fname : string);{fname = filename sonder extention}
Var
  Lyn  : LynT;
  LynF : LynFT;
  i    : word;
  txt  : Text;
  s    : byte;
  ts   : string;
  ec   : integer;
Begin
{$IFNDEF WIN32}
   LogEvent('Switching to graphmode');
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

   SetGraphMode(gm);
   If (scrgmode) then
        LogEvent('Already in graphmode');
   scrgmode := true;
   i := 1;
   While not(EoF(LynF)) Do
   Begin
      Read(LynF,lyn);
      PutImage(0,((i-1)*s),lyn,NormalPut);
      Inc(i);
   End;
   Close(LynF);
   LogEvent('Switched to graphmode');
{$ENDIF}
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

Begin
   scrgmode := false;
end.
