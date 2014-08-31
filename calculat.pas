{
    Calculator - a dos calculator to test pcalcexp
    Copyright (C) 2003  Gert van den Berg

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
{$IFDEF DELPHI}
{$Define EXCEPT}
{$ENDIF}
{$IFDEF FPC}
{$Define EXCEPT}
{$Mode Delphi}
{$ENDIF}

Program Calculator;

Uses Crt, pcalcexp{, calcexp};

Var str : shortstring;
    post : postft;
    i    : byte;
    r,s  : extended;

Begin
 Repeat
   TextColor(7);
   TextBackground(0);
   ClrScr;
   Writeln('Tik asb. ''n som in.  Tik "Q" om uit te gaan.');
   Write('> '); Readln(str);
   If length(str) > 0 then
        If Upcase(str[1]) = 'Q' then halt;
   ClrScr;
   Writeln('String: ',str);
   Writeln;
   Writeln('Converting to postfix...');
   {$IFDEF FPC}
   try
      string2postf(str,post);
   except
       TextColor(LightRed);
       Writeln('Warning! PCalcExp error no. ',pcalcexp.calcerror,' encountered.');
       Writeln('Message: ',pcalcErrorMsG(pcalcexp.calcerror));
       TextColor(7);
   End;
   {$ELSE}
   string2postf(str,post);
   If pcalcexp.calcerror <> 0 then
    Begin
       TextColor(LightRed);
       Writeln('Warning! PCalcExp error no. ',pcalcexp.calcerror,' encountered.');
       Writeln('Message: ',pcalcErrorMsG(pcalcexp.calcerror));
       TextColor(7);
    End;
   {$ENDIF}
   Writeln('Done');
   Writeln;
   Writeln           ('TYPE      Value');
   Writeln           ('--------- ------');
   For i := 1 to post.count do
   Begin
      case post.items[i].itemtype of
       valu : Begin
                Write('VALUE     ');
                Writeln(post.items[i].valu:1:5);
              End;
       oper : Begin
                Write('OPERATOR  ');
                Writeln(post.items[i].oper);
              End;
       func : Begin
                Write('FUNCTION  ');
                Writeln(post.items[i].func);
              End;
       cons : Begin
                Write('CONSTANT  ');
                Writeln(post.items[i].func);
              End;
       v1   : Writeln('VAR       x');
       v2   : Writeln('VAR       y');
       v3   : Writeln('VAR       z');
       else
       Writeln('Unknown itemtype encountered');
      end;
   End;
   Writeln;
   {$IFNDEF EXCEPT}
   if pcalcexp.calcerror = 0 then r := CalcPostfix(post,0,0,0) else
    Begin
      r := 0;
      Writeln('Error while converting.');
    End;
   Writeln('Result = ',r:1:10);
   If pcalcexp.calcerror <> 0 then
    Begin
       TextColor(LightRed);
       Writeln('Warning! PCalcExp error no. ',pcalcexp.calcerror,' encountered.');
       Writeln('         Message: '+pcalcerrormsg(pcalcexp.calcerror));
       TextColor(7);
    End;
   Writeln;
   If Pcalcexp.calcresult <> 0 then r := 0;
   {$ELSE}
   if pcalcexp.calcerror = 0 then
       try
          r := CalcPostfix(post,0,0,0);
       except
          r := 0;
       End
   else Begin
      r := 0;
      Writeln('Error while converting.');
    End;
   Writeln('Result = ',r:1:10);
   If pcalcexp.calcerror <> 0 then
    Begin
       TextColor(LightRed);
       Writeln('Warning! PCalcExp error no. ',pcalcexp.calcerror,' encountered.');
       Writeln('         Message: '+pcalcerrormsg(pcalcexp.calcerror));
       TextColor(7);
    End;
   Writeln;
   If Pcalcexp.calcresult <> 0 then r := 0;
   {$ENDIF}
{   s := Calculate(str);
   Writeln('Result volgens calcexp = ',s:1:10);
   If calcexp.calcerror <> 0 then
    Begin
       TextColor(LightRed);
       Writeln('Warning! CalcExp error no. ',calcexp.calcerror,' encountered.');
       TextColor(7);
    End;
   If calcexp.calcresult <> 0 then s := 0;
   Writeln;
   If (round(r*10000) = round(s*10000)) then
    Begin
       TextColor(LightGreen);
       Writeln('Items Match');
       TextColor(7);
       Writeln;
    End else
    Begin
       TextColor(LightRed);
       Writeln('Items don''t match!!! Difference = ',abs(r-s):0:8);
       TextColor(7);
       Writeln;
    End;  }
   Writeln('Press any key to continue...');Readkey;
 Until False;
End.
