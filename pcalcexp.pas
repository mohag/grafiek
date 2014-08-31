{
    PCalcExp.pas version 1.1 - a pascal/delphi unit that convert a string
                               to postfix and that calculates the postfix
                               expression

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

{
Notes on compiling this program:
--------------------------------
    General:
    --------
        Free Pascal should generate the most effcient code
    Free Pascal:
    ------------
        Set target to DOS(GO32v2)
    Turbo/Borland Pascal:
    ---------------------
        Free Pascal compilation is recommended since it generates faster 32bit
        code
        BP only: Set target to REAL MODE or "Protected mode"
    Delphi:
    -------
        none
}
{$B-}
{$N+}
{$IFDEF FPC}
 {$INLINE ON}
 {$Define MATH} // use math unit for faster calculations
 {$MODE DELPHI}
 {$DEFINE DELPHI}
 {$R-}
 {$GOTO ON}
{$ENDIF}
{$IFDEF DPMI}
 {$DEFINE MSDOS}
{$ENDIF}
{$IFDEF DELPHI}
 {$Define Math} // use math unit for faster calculations
 {$H-}{no "Huge"(ANSI) Strings in delphi and Kylix (Turbo Pascal doesn't
  support "Huge" Strings)}
{$ENDIF}
unit Pcalcexp;

{
This unit contains procedures to convert a mathematical expression
in a string to postfix notation and to calculate the value of the
postfix expression.

This unit provides the following:

TYPES:
------
  postft : Type used for storing postfix

  Other types: See source code

  DELPHI & FREE PASCAL ONLY:
     EPCalcexpError : Execption class that is raised when errors are
                      encountered see calcResult for details.
GLOBAL VARIABLES:
-----------------
    CalcError : Integer (TP/BP) / Smallint (FPC/Delphi)
        Holds the value to be returned by CalcResult (See below)
    pcalcexception : boolean
        Delphi / Free Pascal:
            Controls whether exceptions are raised on errors. Default = true
        Turbo / Boland Pascal:
            For compatibility only. Ignored.
PROCEDURES AND FUNCTIONS:
-------------------------
  Procedure String2postf(str : string; var postf : postft);
    Converts the string in str to postfix and store result in postf

    NOTE: 1. In Delphi str is of type ShortString
          2. Run CalcResult to see if conversion was successful (see below)
             before calculating postf.

  Procedure PreCalcPostfix(var postf : postft);
    Calculates (almost) everything that can be calculated in postf without
    the variable values and store result back into postf

    NOTE: This is used to speed up the calculation of postf with multiple
          values for the variables.  Therefore there it is unneccesary to
          use it if postf is to be calculated for only one variable value
          or if postf contains no variables.  Ideal for graphing.

  Function Calcpostfix(var postf : postft;x,y,z : extended) : extended;
    Calculates the value of postf if first variable = x, second variable = y,
    ect.  Returns result.

  Function CalcResult : integer;
    Returns: last error code.

    NOTE: Use this to see if a operation ran successfully.

  Function Power(x,y : extended):Extended;
    Returns: Result of x^y

  Function pcalcErrorMsg(code : integer) : string;
    Returns: Description of error no. in code

}
Interface

{$IFDEF MATH}
Uses Sysutils;
{$ENDIF}

{$ifdef math}
var
    calcerror       : smallint;
    pcalcexception  : boolean;
{$else}
var
    calcerror       : integer;
    pcalcexception  : boolean;
{$endif}

Type
{$IFDEF MATH} {Under FP & Delphi}
     EPCalcExpError = Class(Ematherror);
{$Else} {Only under TP}
     Smallint       = integer;
     shortstring    = string;
{$ENDIF}
     RealStack   = Object
                     Items     : Array[1..30] of extended;
                     count     : byte;
                     Procedure   Push(value    : extended);
                     Procedure   Pop (var into : extended);
                     Function    PopF          : extended;
                     constructor Clear;
                   End;

     StringStack = Object
                     Items     : Array[1..50] of string[30];
                     count     : byte;
                     Procedure   Push(value    : shortstring);
                     Procedure   Pop (var into : shortstring);
                     Procedure   Delete(index : byte);
                     Procedure   Insert(index : byte;value : shortstring);
                     Function    PopF          : shortstring;
                     constructor Clear;
                   End;

     EntryT      = (func,valu,oper,cons,empty,v1,v2,v3);

     PostFItemT  = Record
                      itemtype : EntryT;
                      func : String[30];
                      valu : extended;
                      oper : char;
                   End;{of record}

     PostFT      = Object
                      count    : byte;
                      items    : array[1..40] of PostFitemT;
                      Procedure   Add  (item : postfitemT);
                      Procedure   Delete(index : byte);
                      Procedure   Get(index  : byte;var into : postfitemt;del : boolean);
                      constructor Clear;
                   End;


Procedure String2postf(str : shortstring; var postf : postft);
Procedure PreCalcPostfix(var postf : postft);
Function Calcpostfix(const postf : postft;const x,y,z : extended) : extended;
Function CalcResult : smallint;
Function Power(x,y : extended):Extended;
Function pcalcErrorMsg(code : smallint) : shortstring;

implementation

{$ifdef Math} // under free pascal & Delphi
Uses math;
{$endif}

const
      Funccount = 36;
      Functions : Array[1..funccount] of String[10]
                = ('ASIN','ARCSIN','ACOS','ARCCOS','ATAN','ARCTAN','SIN',
                   'COS','TAN','COT','SEC','COSEC','NEG','LN','LOG',
                   'EXP','TRUNC','ABS','ROUND','SQR','DEG','SQRT','SINH',
                   'COSH','TANH','ARCSINH','ARCTANH','ARCCOSH','ASINH',
                   'ATANH','ACOSH','SECH','COSECH','CSCH','COTH','CSC');{}

var pi : extended;

{=Object methods==========================================================}
Procedure Realstack.Push(value : extended);
Begin
   Inc(count);
   Items[count] := value;
End;
{-------------------------------------------------------------------------}
Procedure Realstack.Pop(var into : extended);
Begin
   If count > 0 then
    Begin
      into := items[count];
{      items[count] := 0;}
      FillChar(items[count],sizeof(items[count]),0);
      Dec(count);
    End else
    Begin
       Into := 0;
       calcerror := 401;
    End;
End;
{-------------------------------------------------------------------------}
Function Realstack.PopF : extended;
var tmp : extended;
Begin
   pop(tmp);
   popf := tmp;
End;
{-------------------------------------------------------------------------}
Constructor Realstack.Clear;
var i :byte;
Begin
   count := 0;
   for i := 1 to 30 do
    items[i] := 0;
End;
{-End of realstack start of stringstack-----------------------------------}
Procedure Stringstack.Push(value : shortstring);
Begin
   Inc(count);
   Items[count] := value;
End;
{-------------------------------------------------------------------------}
Procedure Stringstack.Delete(index : byte);
Begin
   If count <= 1 then clear else
    Begin
        Move(items[index+1],Items[index],Sizeof(Items[index])*(count-index));
{        For i := index to count-1 do
            items[i] := items[i+1];}
{        items[count] := '';}
        FillChar(items[count],sizeof(items[count]),0);
        dec(count);
    End;
End;
{-------------------------------------------------------------------------}
Procedure Stringstack.Insert(index : byte;value : shortstring);
var i : byte;
Begin
   for i := count downto (index) do
    items[i+1] := items[i];
   items[index] := value;
   inc(count);
End;
{-------------------------------------------------------------------------}
Procedure stringstack.Pop(var into : shortstring);
Begin
  If count > 0 then
   Begin
      into := items[count];
{      items[count] := '';}
      FillChar(items[count],sizeof(items[count]),0);
      Dec(count);
   End else
   Begin
       Into := '';
       calcerror := 402;
   End;
End;
{-------------------------------------------------------------------------}
Function Stringstack.PopF : shortstring;
var tmp : string;
Begin
   pop(tmp);
   popf := tmp;
End;
{-------------------------------------------------------------------------}
Constructor Stringstack.Clear;
Begin
   count := 0;
   FillChar(items,sizeof(items),0);
{   for i := 1 to 50 do
    items[i] := '';              {}
End;
{-end of stringstack satart of postft-------------------------------------}
Procedure PostFT.Add(item : postfitemT);
Begin
   Inc(count);
   Items[count] := item;
End;
{-------------------------------------------------------------------------}
Procedure PostFT.Delete(index : byte);
Begin
   If count <= 1 then clear else
    Begin
        Move(items[index+1],Items[index],Sizeof(Items[index])*(count-index));
{       For i := index to count-1 do
           items[i] := items[i+1];}
       {items[count].itemtype := empty;
       items[count].valu := 0;
       items[count].func := '';
       items[count].oper := #0;}
       FillChar(items[count],sizeof(items[count]),0);
       dec(count);
    End;
End;
{-------------------------------------------------------------------------}
Procedure PostFT.Get(index  : byte;var into : postfitemt;del : boolean);
Begin
  into := items[index];
  If del then delete(index);
End;
{-------------------------------------------------------------------------}
Constructor PostFt.Clear;
{var i :byte;
    tmp : PostfitemT;}
Begin
   count := 0;
{   with tmp do
   Begin
     valu := 0;
     func := '';
     oper := #0;
     itemtype := empty;
   End;
   for i := 1 to 40 do
     items[i] := tmp;}
   FillChar(items,sizeof(items),0);
End;
{-End of postft-----------------------------------------------------------}
{=====End of methods======================================================}
Procedure CalcExpError(code : integer);
{$IFDEF FPC}inline;{$endif}
Begin
  calcerror := code;
End;
{-------------------------------------------------------------------------}
Procedure RaiseExcept;
{$IFDEF Math}
var
    errorstring : ansistring;
{$ENDIF}
Begin
    {$IFDEF Math}
    if (calcerror <> 0) and pcalcexception then
        Begin
            errorstring := 'PCalcExp: Parser Error '+InttoStr(calcerror);
            errorstring := errorstring + ': '+Pcalcerrormsg(calcerror);
            raise EPCalcExpError.create(PChar(errorstring));
        End;
    {$ENDIF}
End;
{-------------------------------------------------------------------------}
Function Divide(x,y:extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
{$IFDEF math}
   try
      divide := x/y;
   except
      Divide := 0;
      calcexperror(101);
   end;
{$Else}
   If (y <> 0) and (calcerror = 0) then divide := x/y else
    begin
       Divide := 0;
       calcexperror(101);
    End;
{$endif}
End;
{-------------------------------------------------------------------------}
Function ln(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      ln := system.ln(x);
   except
      ln := 0;
      calcexperror(102);
   end;
  {$else}
   If x > 0 then ln := system.ln(x) else
    Begin
      calcexperror(102);
      ln := 0;
    End;
{$endif}
End;
{-------------------------------------------------------------------------}
Function sqrt(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
     sqrt := system.sqrt(x)
   except
     sqrt := 0;
     calcexperror(103);
   end;
  {$else}
   If x >= 0 then sqrt := system.sqrt(x) else
    Begin
      calcexperror(103);
      sqrt := 0;
    End;
{$endif}
End;
{-------------------------------------------------------------------------}
function arcsin(ratio : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
   {$IFdef Math}
  try
     arcsin := math.arcsin(ratio);
  except
     calcexperror(107);
     arcsin := 0;
  End;
   {$Else}
  If (ratio < -1) or (ratio > 1) then
  Begin
    arcsin := 0;
    calcexperror(104);
  End
  else
   arcsin := arctan(Divide(ratio,sqrt((1 - sqr(ratio)))));
   {$endif}
end;
{-------------------------------------------------------------------------}
function arccos(ratio : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
   {$IFdef Math}
  try
     arccos := math.arccos(ratio);
  except
     calcexperror(107);
     arccos := 0;
  End;
   {$Else}
  If (ratio < -1) or (ratio > 1) then
  Begin
    arccos := 0;
    calcexperror(105);
  End
  else
   arccos := -arcsin(ratio) + pi/2;
   {$endif}
end;
{-------------------------------------------------------------------------}
function tan(angle : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
  {$Ifdef math}
  try
     tan := math.tan(angle)
  except
     calcexperror(107);
     tan := 0;
  End;
  {$else}
  tan  := divide(sin(angle), cos(angle));
  {$endif}
end;
{-------------------------------------------------------------------------}
function cot(angle : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
  {$Ifdef math}
  try
     cot := math.cotan(angle)
  except
    calcexperror(107);
    cot := 0;
  end;
  {$else}
  cot  := divide(cos(angle), sin(angle));
  {$endif}
end;
{-------------------------------------------------------------------------}
function cosec(angle : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
   {$IFdef Math}
   try
      cosec := 1/system.sin(angle);
   except
      cosec := 0;
      calcexperror(107);
   end;
  {$else}
  cosec := divide(1, sin(angle));
{$endif}
end;
{-------------------------------------------------------------------------}
function sec(angle : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
   {$IFdef Math}
   try
      sec := 1/system.cos(angle);
   except
      sec := 0;
      calcexperror(107);
   end;
  {$else}
  sec  := divide(1, cos(angle));
{$endif}
end;
{-------------------------------------------------------------------------}
function log(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
begin
   {$IFdef Math}
   try
      log := math.log10(x);
   except
      calcexperror(107);
      log := 0;
   end;
  {$else}
  log := Divide(ln(x), exp(1));
{$endif}
end;
{-------------------------------------------------------------------------}
Function sinh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      sinh := Math.sinh(x);
   except
      calcexperror(107);
      sinh := 0;
   end;
  {$else}
  sinh := (exp(x)-exp(-x)/2)
{$endif}
End;
{-------------------------------------------------------------------------}
Function cosh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      cosh := math.cosh(x);
   except
      calcexperror(107);
      cosh := 0;
   end;
  {$else}
  cosh := (exp(x)+exp(-x))/2
{$endif}
End;
{-------------------------------------------------------------------------}
Function tanh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      tanh := math.tanh(x);
   except
      calcexperror(107);
      tanh := 0;
   end;
  {$else}
  tanh := Divide(sinh(x),cosh(x));
{$endif}
End;
{-------------------------------------------------------------------------}
Function sech(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      sech := 1/Math.cosh(x);
   except
      calcexperror(107);
      sech := 0;
   end;
  {$else}
  sech := Divide(1,cosh(x));
{$endif}
End;
{-------------------------------------------------------------------------}
Function cosech(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      cosech := 1/math.sinh(x);
   except
      calcexperror(107);
      cosech := 0;
   end;
  {$else}
  cosech := Divide(2,(exp(x)+exp(-x)))
{$endif}
End;
{-------------------------------------------------------------------------}
Function coth(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      coth := 1/math.tanh(x);
   except
      calcexperror(107);
      coth := 0;
   end;
  {$else}
  coth := Divide(cosh(x),sinh(x));
{$endif}
End;
{-------------------------------------------------------------------------}
Function arcsinh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      arcsinh := math.arsinh(x);
   except
      calcexperror(107);
      arcsinh := 0;
   end;
  {$else}
  arcsinh := ln(x+sqrt(x*x+1))
{$endif}
End;
{-------------------------------------------------------------------------}
Function arccosh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      arccosh := math.arcosh(x);
   except
      calcexperror(107);
      arccosh := 0;
   end;
   {$else}
   arccosh := ln(x+sqrt(x*x-1))
  {$endif}
End;
{-------------------------------------------------------------------------}
Function arctanh(x : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
{$IFNDEF FPC} {Free pascal has a bug in its artan function}
   {$IFdef Math}
   try
      arctanh := math.artanh(x);
   except
      calcexperror(107);
      arctanh := 0;
   end;
  {$else}
  arctanh := ln(divide((1+x),(1-x)))/2;
{$endif MATH}
{$ELSE}
    if (x < 1) and (x > -1) then
        arctanh := ln(divide((1+x),(1-x)))/2
    else
        Begin
            arctanh := 0;
            calcexperror(107);
        End;
{$endif FPC}
End;
{-------------------------------------------------------------------------}
Function fmod(x,y : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   {$IFdef Math}
   try
      fmod := x - int(x / y)*y;
   except
      calcexperror(107);
      fmod := 0;
   end;
   {$else}
   fmod := x - int(x / y)*y;
  {$endif}
End;
{-------------------------------------------------------------------------}
Function Power(x,y : extended):Extended;
{$IFDEF FPC}inline;{$endif}
 {$IFndef Math}
  Function Teken(mag : double) : shortint;
  var int : longint;
  Begin
     int := trunc(mag);
      If odd(int) then teken := -1 else teken := 1
  End;
 {$ENDIF}
Begin
   {$IFdef Math}
   try
      if (x < 0) and (y=int(y)) then power := math.intpower(x,trunc(y))
       else
         power := math.power(x,y);
   except
      power := 0;
      calcexperror(106);
   end;
  {$else}
 If (calcerror = 0) then
  Begin
   IF (x < 0) and (y=int(y)) then power := teken(y)*exp(y*ln(abs(x))) else
   IF y = 0 then power := 1 else
   IF x = 0 then power := 0 else
   IF x > 0 then power := exp(y*ln(x)) else calcexpError(106);
  End else power := 0;
{$endif}
    RaiseExcept;
End;
{-------------------------------------------------------------------------}
Function CalcResult : smallint;
Begin
   calcresult := calcerror;
   calcerror := 0;
End;

{==Procedures and functions===============================================}
{$IFNDEF Math}
function UpperCase(S: shortstring): shortstring;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
  if (S[I] >= 'a') and (S[I] <= 'z') then
  Dec(S[I], 32);
  UpperCase := S;
end;
{$ENDIF}
{-------------------------------------------------------------------------}
Function IsFunction (str : shortstring) : boolean;
var i : byte;
Begin
   IsFunction := false;
   str := Uppercase(str);
   For i := 1 to funccount do
    If functions[i] = str then IsFunction := true;
End;
{-------------------------------------------------------------------------}
Function IsNumber (str : shortstring) : boolean;
var i : byte;
Begin
   IsNumber := true;
   If not(str[1] in ['0'..'9']) then
        IsNumber := false;
   for i := 2 to length(str) do
      if not (str[i] in['0'..'9','E','.'{,'-','+'}]) then
         Isnumber := false;
End;
{-------------------------------------------------------------------------}
Procedure String2postf(str : shortstring; var postf : postft);
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure Strip(var toupper : shortstring);
var
  i,j : byte;
  t : string;
  b : shortint; {used to check brackets}
begin
  t := '';
  For i := 1 to length(toupper) do
   Begin
      if (toupper[i] = '[') then toupper[i] := '(';
      if (toupper[i] = ']') then toupper[i] := ')';
   End;
{  If toupper[1] = '-' then toupper := '0'+toupper;{}
  For i := 1 to length(toupper) do
   Begin
      If toupper[i] in ['A'..'Z','0'..'9','(',')','^'{,'!'},'+','-','*','/','.',',','%'] then
       t := t + toupper[i];
   End;
  toupper := t;
  t := '';
  For i := 1 to length(toupper) do
   if (toupper[i] <> ',') then
    t := t + toupper[i] else
     t := t + '.';
  toupper := t;
  b := 0;
  For i := 1 to length(toupper) do
   Begin
     If toupper[i] = '(' then inc(b) else
     If toupper[i] = ')' then dec(b);
   End; {If brackets are correct b will be 0}
  While b > 0 do {Add missing brackets to end}
   Begin
     toupper := toupper + ')';
     dec(b);
     if b < 0 then calcexperror(201);
   End;
  i := 1;
  While i < Length(toupper) do
   Begin
      Inc(i);
      If ((toupper[i] in ['A'..'Z']) and (Toupper[i-1] in ['0'..'9'])) or
         ((toupper[i] in ['X','Y','Z']) and (toupper[i-1] in ['Y','X','Z'])) then
       Begin
          Toupper := toupper + ' ';{Increase length with one char}
          For j := (length(toupper)) downto i+1 do
           toupper[j] := toupper[j-1];
          toupper[i] := '*';
       End;{If}
   End;
end;
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure RemoveUnarymin(var inp : stringstack);
var finished : boolean;
    i        : integer;
    ts       : shortstring;
    strt, nd : word;
    c1,c2    : smallint;
Begin
   Repeat
      finished := true;
      i := 1;
      c1 := 0; c2 := 0; {Initialize genral purpose counters}
      While (i <= (inp.count)) and (calcerror = 0) do
       Begin
         finished := true;
         strt := 0; nd := 0;
         if i > 1 then
            ts := inp.items[i-1] else ts := 'b (';
         if (inp.items[i] = '1 -') then
          Begin
             If (ts = 'b (') then
              Begin
                strt := i+1;
                ts := inp.items[i+1];
                if ((ts = 'b (') or (ts[1] = 'f')) then
                 begin
                   if (ts[1] <> 'f') then c2 := i+2 else c2 := i+3;
                   c1 := 1;
                   While (c1 > 0) and (c2 <= inp.count) do
                    Begin
                      If (inp.items[c2] = 'b (') then inc(c1) else
                       if (inp.items[c2] = 'b )') then dec(c1);
                      Inc(c2);
                    End;
                   if c1 <> 0 then calcexperror(208) else finished := false;
                   nd := c2;
                   c1 := 0;
                   c2 := 0;
                 end {'b ('} else
                if (ts[1] in ['v','c','x','y','z']) then
                 Begin
                    strt := i+1;
                    c2 := i+1;
                    c1 := 0;{bracket level counter}
                    While ((c1 > 0) or (inp.items[i,1] <> '1')) and
                          (c2 <= inp.count) and
                          not ((c1 = 0) and (inp.items[i] = 'b )')) do
                     Begin
                       ts := inp.items[c2];
                       if (ts = 'b (') then inc(c1) else
                       if (ts = 'b )') then dec(c1);
                       inc(c2);
                     End;{While}
                    nd := c2+1;
                    if c1 = 0 then finished := false else calcexperror(208);
                 end else calcexperror(208);
                i := 1;
              End;
          End; {end if - encoutered}
         inc(i);
         if (not(finished)) and (strt > 0) and (nd > strt) then
          Begin
             inp.insert(nd,'b )');
             inp.insert(strt,'b (');
             inp.insert(strt,'f NEG');
             inp.delete(strt -1);
          End;
       End;
   until (finished) or (calcerror <> 0);
End;
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
var i,j          : byte;
    ts           : string[30];
    apart        : stringstack;
    ttype        : char;
    tr           : Extended;{Temp Real}
    ec           : integer; {ec = error code}
    item         : PostFItemT;
    opstack      : StringStack;
    bracket      : boolean;
Begin
  str := Uppercase(str);
  postf.clear;
  opstack.clear;
  apart.clear;
  calcerror := 0;
  If length(str) < 1 then calcexperror(207);
  If (calcerror = 0) then Strip(str);
  i := 0;
  While (i < length(str)) and (calcerror = 0) do
  Begin
     Inc(i);
     ts := '';
     While (not(str[i] in ['+','-','*','^','%','/','(',')','[',']']))
            and (i <= length(str)) do
     Begin
        ts := ts + str[i];
        inc(i);
     End;
     If (length(ts) > 0) then
      apart.push(ts);
     if i <= length(str) then
      apart.push(str[i]);
  End;{Components of str now in the stringstack, "apart"}
  i := 1;
  ttype := #0;
  While (i <= apart.count) and (calcerror = 0) do{check if every statement is valid and identify}
   Begin                   {it}
     If (length(apart.items[i]) > 1) then
      Begin
          If IsFunction(apart.items[i])
           then ttype := 'f'   {function}
           else{If not a function}
             If ((apart.items[i] = 'PI')) THEN
              ttype := 'c' {constant} else {and not a constant}
           Begin{check if value}
             If IsNumber(apart.items[i]) then
                ttype := 'v'{value}
             else
                ttype := #0;{Error if not a number}
           End;
      End
      else
       Begin {If length = 1}
          If (length(apart.items[i]) = 0) then apart.delete(i)
           else
           case apart.items[i,1] of
             '+','-'    : ttype := '1';  {Lowest level of operator}
             '*','/','%': ttype := '2';  {second level operator}
             '^'        : ttype := '3';  {Highest level operator}
             'X'        : ttype := 'x';  {First variable}
             'Y'        : ttype := 'y';  {Second variable}
             'Z'        : ttype := 'z';  {Third variable}
             'E'        : ttype := 'c';  {e(=2.7...)}
             '(',')'    : ttype := 'b';  {Bracket}
             '0'..'9'   : ttype := 'v';  {value}
           else ttype := #0
           End;
       End;
     If ttype = #0 then
     Begin
        calcexperror(202);
        Postf.clear;
        Exit;
     End;
     Apart.items[i] := ttype + ' ' + apart.items[i];{Add type identifier and space to front of string}
     Inc(i);
   End;{Of identification While loop}
  {All parts of expression is now identified and in apart with identifier
   character at start of strings}
  i := 1;
  While i+1 < apart.count do
  Begin
     If ((apart.items[i] = 'b )') and (apart.items[i+1] = 'b (')) or
        ((apart.items[i,1] in ['x','y','v']) and (apart.items[i+1] = 'b (')) then
       Apart.insert(i+1,'2 *');
     inc(i);
  End;
  {(...)(....) converted to (...)*(....)}
  RemoveUnarymin(apart);
  {Start of conversion}
  If (calcerror = 0) then
  For i := 1 to apart.count do
   Begin
      ts := '';
      item.itemtype := empty;
      item.valu := 0;
      item.oper := #0;
      item.func := '';
      For j := 1 to (length(apart.items[i])-2) do
        ts := ts + apart.items[i,j+2];{set ts to item WITHOUT identifier}

      Case apart.items[i,1] of
        'v' : Begin{value}
                 Val(ts,tr,ec);
                 If ec <> 0 then
                  Begin
                     calcexperror(203);
                     postf.clear;
                     Exit;
                  End;
                 item.itemtype := valu;
                 item.valu := tr;
                 postf.add(item);
              End;
        'c' : Begin{constant}
                 item.itemtype := cons;
                 item.func := ts;
                 postf.add(item);
              End;
        'f' : Begin{function}
                 opstack.push(apart.items[i]);
              End;
        '1' : Begin{operator : lowest level (+-)}
                 While (opstack.items[opstack.count,1] in ['1'..'3','f']) do
                 Begin
                    Item.itemtype := empty;
                    item.oper := #0;
                    item.func := '';
                    item.valu := 0;
                    Case opstack.items[opstack.count,1] of
                      '1'..'3' : Begin
                                    item.itemtype := oper;
                                    item.oper := opstack.items[opstack.count,3];
                                 End;
                      'f'      : Begin
                                    ts := '';
                                    For j := 3 to (length(opstack.items[opstack.count])) do
                                      ts := ts + opstack.items[opstack.count,j];{set ts to item WITHOUT identifier}
                                    item.itemtype := func;
                                    item.func := ts;
                                 End;
                    End;{case}
                    Postf.add(item);{}
                    opstack.delete(opstack.count);
                 End;
                 opstack.push(apart.items[i]);
              End;
        '2' : Begin{operator : second level (*/)}
                 While (opstack.items[opstack.count,1] in ['2'..'3','f']) do
                 Begin
                    Item.itemtype := empty;
                    item.oper := #0;
                    item.func := '';
                    item.valu := 0;
                    Case opstack.items[opstack.count,1] of
                      '1'..'3' : Begin
                                    item.itemtype := oper;
                                    item.oper := opstack.items[opstack.count,3];
                                 End;
                      'f'      : Begin
                                    ts := '';
                                    For j := 3 to (length(opstack.items[opstack.count])) do
                                      ts := ts + opstack.items[opstack.count,j];{set ts to item WITHOUT identifier}
                                    item.itemtype := func;
                                    item.func := ts;
                                 End;
                    End;{case}
                    Postf.add(item);{}
                    opstack.delete(opstack.count);
                 End;
                 opstack.push(apart.items[i]);
              End;
        '3' : Begin{operator : highest level (^)}
                 While (opstack.items[opstack.count,1] in ['3','f']) do
                 Begin
                    Item.itemtype := empty;
                    item.oper := #0;
                    item.func := '';
                    item.valu := 0;
                    Case opstack.items[opstack.count,1] of
                      '1'..'3' : Begin
                                    item.itemtype := oper;
                                    item.oper := opstack.items[opstack.count,3];
                                 End;
                      'f'      : Begin
                                    ts := '';
                                    For j := 3 to (length(opstack.items[opstack.count])) do
                                      ts := ts + opstack.items[opstack.count,j];{set ts to item WITHOUT identifier}
                                    item.itemtype := func;
                                    item.func := ts;
                                 End;
                    End;{case}
                    Postf.add(item);{}
                    opstack.delete(opstack.count);
                 End;
                 opstack.push(apart.items[i]);
              End;
        'x' : Begin{variable : x}
                 item.itemtype := v1;
                 postf.add(item);
              End;
        'y' : Begin{variable : y}
                 item.itemtype := v2;
                 postf.add(item);
              End;
        'z' : Begin{variable : z}
                 item.itemtype := v3;
                 postf.add(item);
              End;
        'b' : Begin{bracket}
                 if ts = '(' then opstack.push(apart.items[i]) else
                 if ts = ')' then {take all items until bracket of opstack add to postf}
                  Repeat
                    ts := '';
                    bracket := false;
                    Item.itemtype := empty;
                    item.oper := #0;
                    item.func := '';
                    item.valu := 0;
                    For j := 1 to (length(opstack.items[opstack.count])-2) do
                      ts := ts + opstack.items[opstack.count,j+2];{set ts to item WITHOUT identifier}
                    Case opstack.items[opstack.count,1] of
                      '1'..'3' : Begin
                                    item.itemtype := oper;
                                    item.oper := ts[1];
                                 End;
                      'f'      : Begin
                                    item.itemtype := func;
                                    item.func := ts;
                                 End;
                      'b' :   bracket := true;
                    else
                      calcexperror(204);
                    End;{case}
                    if not bracket then Postf.add(item);
                    opstack.delete(opstack.count);
                  until (bracket)
                   else calcexperror(205);{End if}
              End;
      End;{Case}
   End;{For}
  While (opstack.count > 0) and (calcerror = 0) do
   Begin
      ts := '';
      item.itemtype := empty;
      item.valu := 0;
      item.oper := #0;
      item.func := '';
      For j := 1 to (length(opstack.items[opstack.count])-2) do
        ts := ts + opstack.items[opstack.count,j+2];{set ts to item WITHOUT identifier}
      Case opstack.items[opstack.count,1] of
        '1'..'3' : Begin
                      item.itemtype := oper;
                      item.oper := ts[1];
                   End;
        'f'      : Begin
                      item.itemtype := func;
                      item.func := ts;
                   End;
      else
         calcexperror(206);
      End;{case}
      Postf.add(item);
      opstack.delete(opstack.count);
   End; {While}
  {Str is now converted to postfix in postf}
  If calcerror <> 0 then postf.clear;
    RaiseExcept;
End;
{-------------------------------------------------------------------------}
{Function abs(x : extended) : extended;
Begin
   If x < 0 then abs := -x
    else abs := x;
end;
{-------------------------------------------------------------------------}
Function CalcOperator(var item : postfitemt; a,b : extended) : extended;
{$IFDEF FPC}inline;{$endif}
Begin
   If (item.itemtype = oper) then
    Begin
       Case (item.oper) of
          '+'     : CalcOperator := a + b;
          '-'     : CalcOperator := a - b;
          '*'     : CalcOperator := a * b;
          '/'     : CalcOperator := Divide(a,b);
          '^'     : CalcOperator := Power(a,b);
          '%'     : CalcOperator := fmod(a,b);
       else
          calcexperror(301);
          CalcOperator := 0;
       End;
    End else
    Begin
       CalcOperator := 0;
       calcexperror(307);
    End;
End;
{-------------------------------------------------------------------------}
Function Calcfunction(var item : postfitemt; val : extended) : extended;
{$IFDEF FPC}inline;{$endif}
{If modified remember to update functions and funccount constants in order
to make modifications work}
Begin
If item.itemtype = func then
  Begin
            IF (item.func = 'ASIN') or (item.func = 'ARCSIN') then CalcFunction := arcsin(val)else
            IF (item.func = 'ACOS') or (item.func = 'ARCCOS') then CalcFunction := arccos(val)else
            IF (item.func = 'ATAN') or (item.func = 'ARCTAN') then CalcFunction := arctan(val)else
            IF (item.func = 'SIN') then CalcFunction := sin(val)else
            IF (item.func = 'COS') then CalcFunction := cos(val)else
            IF (item.func = 'TAN') then CalcFunction := tan(val)else
            IF (item.func = 'COT') then CalcFunction := cot(val)else
            IF (item.func = 'SEC') then CalcFunction := sec(val)else
            IF (item.func = 'COSEC') or (item.func = 'CSC') then CalcFunction := cosec(val)else
            IF (item.func = 'NEG') or (item.func = '-') then CalcFunction := -val else
            IF (item.func = 'LN') then CalcFunction := ln(val)else
            IF (item.func = 'LOG') then CalcFunction := log(val)else
            IF (item.func = 'EXP') then CalcFunction := exp(val)else
            IF (item.func = 'TRUNC') then CalcFunction := int(val)else
            IF (item.func = 'ABS') then CalcFunction := abs(val)else
            IF (item.func = 'ROUND') then CalcFunction := round(val)else
            IF (item.func = 'SQR') then CalcFunction := sqr(val)else
            IF (item.func = 'DEG') then CalcFunction := {$Ifdef Math}
                                                        math.degtorad(val) // fp & delphi
                                                        {$else}
                                                        (val)/180*pi
                                                        {$endif}
                                                        else
            IF (item.func = 'SQRT') then CalcFunction := sqrt(val) else
            IF (item.func = 'SINH') then CalcFunction := sinh(val) else
            IF (item.func = 'COSH') then CalcFunction := cosh(val) else
            IF (item.func = 'TANH') then CalcFunction := tanh(val) else
            IF (item.func = 'COSECH') or (item.func = 'CSCH') then
                CalcFunction := cosech(val) else
            IF (item.func = 'SECH') then CalcFunction := sech(val) else
            IF (item.func = 'COTH') then CalcFunction := coth(val) else
            IF (item.func = 'ARCSINH') or (item.func = 'ASINH') then
                CalcFunction := arcsinh(val) else
            IF (item.func = 'ARCCOSH') or (item.func = 'ACOSH') then
                CalcFunction := arccosh(val) else
            IF (item.func = 'ARCTANH') or (item.func = 'ATANH') then
                CalcFunction := arctanh(val) else
{$IFNDEF Ver70} {Additional functions not supported in TP 7}
{$ENDIF}
              Begin
                 calcexperror(302);
                 CalcFunction := 0;
              End;
  End else
   Begin
      Calcfunction := 0 ;
      calcexperror(306);
   End;
End;
{-------------------------------------------------------------------------}
Procedure PreCalcPostfix(var postf : postft);
var i     : byte;
    a, b  : Extended;
    t1    : postfitemt;
    reals : realstack;
label endfunc;
Begin
    calcerror := 0;
    For i := 1 to postf.count do
        Begin
            t1.itemtype := empty;
            t1.oper := #0;
            t1.func := '';
            t1.valu := 0;
            If postf.items[i].itemtype = cons then
                if (postf.items[i].func = 'PI') then
                    Begin
                        t1.itemtype := valu;
                        t1.valu := pi;
                        postf.items[i] := t1;
                    End
                else
                    if (postf.items[i].func = 'E') then
                        Begin
                            t1.itemtype := valu;
                            t1.valu := exp(i);
                            postf.items[i] := t1;
                        End
                    else
                        calcexperror(303);
        End;
    {Constants replaced with their values}
    i := 1;
    While (i < postf.count) and (calcerror = 0) do
        Begin
            t1.itemtype := empty;
            t1.oper := #0;
            t1.func := '';
            t1.valu := 0;
            Inc(i);
            if (i > 2) then
                If (postf.items[i].itemtype = oper) and
                   (postf.items[i-1].itemtype = valu) and
                   (postf.items[i-2].itemtype = valu) then
                    Begin
                        a := postf.items[i-2].valu;
                        b := postf.items[i-1].valu;
                        t1.itemtype := valu;
                        t1.valu := Calcoperator(postf.items[i],a,b);
                        postf.items[i-2] := t1;
                        Postf.delete(i-1);
                        Postf.delete(i-1);{Delete old i}
                        i := 1;
                    End
                else
                    If (postf.items[i].itemtype = func) and
                       (postf.items[i-1].itemtype = valu) then
                        Begin
                            t1.itemtype := valu;
                            t1.valu := Calcfunction(postf.items[i],postf.items[i-1].valu);
                            postf.items[i-1] := t1;
                            Postf.delete(i);
                            i := 1;
                        End;
        End; {While}
    {Start of integrity check. Checks if there's enougth variables / constants/
    values for the functions and operators}
    reals.clear;
    For i := 1 to postf.count do
        Begin
            t1 := postf.items[i];
            if (calcerror <> 0) then
                GOTO EndFunc;

            Case t1.itemtype of
                valu  : reals.push(0);
                v1    : reals.push(0);
                v2    : reals.push(0);
                v3    : reals.push(0);
                cons  : if (t1.func = 'PI') then
                            reals.push(0)
                        else
                            if (t1.func = 'E') THEN
                                reals.push(0)
                            else
                                Begin
                                    calcexperror(303);
                                    GOTO EndFunc;
                                End;
                oper  : Begin
                            reals.pop(a); reals.pop(a);
                            reals.push(0);
                        End;{oper}
                func  : Begin
                            reals.pop(a);
                            reals.push(0);
                        End;{func}
                empty : Begin
                            calcexperror(305);
                            GOTO EndFunc;
                        End;
            else
                CalcExpError(308);
                GOTO EndFunc;
            end; {case itemtype}
        End;
    If (reals.count <> 1) then
        calcexperror(304);
    {END integrity check}
EndFunc:
    RaiseExcept;
End;
{-------------------------------------------------------------------------}
Function Calcpostfix(const postf : postft;const x,y,z : extended) : extended;
var
    t1  : postfitemt;
    i   : byte;
    r1, r2 : extended;
    reals : Realstack;
Label EndFunc;  {Goto used to improve performance}
Begin
    calcerror := 0;
    r1 := 0;
    r2 := 0;
    reals.clear;
    For i := 1 to postf.count do
        Begin
            t1 := postf.items[i];
            if (calcerror <> 0) then
                GOTO EndFunc;

            Case t1.itemtype of
                valu  : reals.push(t1.valu);
                v1    : reals.push(x);
                v2    : reals.push(y);
                v3    : reals.push(z);
                cons  : if (t1.func = 'PI') then
                            reals.push(pi)
                        else
                            if (t1.func = 'E') THEN
                                reals.push(exp(1))
                            else
                                Begin
                                    calcexperror(303);
                                    GOTO EndFunc;
                                End;
                oper  : Begin
                            reals.pop(r1); reals.pop(r2);
                            reals.push(CalcOperator(t1,r2,r1));
                        End;{oper}
                func  : Begin
                            reals.push(Calcfunction(t1,reals.popf));
                        End;{func}
                empty : Begin
                            calcexperror(305);
                            GOTO EndFunc;
                        End;
            else
                CalcExpError(308);
                GOTO EndFunc;
            end; {case itemtype}
        End;
    If (reals.count = 1) then
        calcpostfix := reals.popf
    else
        calcexperror(304);
EndFunc:
    IF (CalcError <> 0) then
        CalcPostfix := 0;
    RaiseExcept;
End;
{-------------------------------------------------------------------------}
Function pcalcErrorMsg(code : smallint) : shortstring;
Begin
   {1xy : Mathematical errors
    2xy : Conversion errors
    3xy : Error while calculating postfix
    4xy : Other errors}
    Case code of
        101 : pcalcerrormsg := 'Division by zero';
        102 : pcalcerrormsg := 'Logarithm of negative number';
        103 : pcalcerrormsg := 'Square root of negative number';
        104 : pcalcerrormsg := 'Arcsin(x) : |x| > 1';
        105 : pcalcerrormsg := 'Arccos(x) : |x| > 1';
        106 : pcalcerrormsg := 'Invalid power';
        107 : pcalcerrormsg := 'Exception raised';

        201 : pcalcerrormsg := 'Too much brackets';
        202 : pcalcerrormsg := 'Invalid item encountered';
        203 : pcalcerrormsg := 'Invalid number encountered';
        204 : pcalcerrormsg := 'General conversion error';
        205 : pcalcerrormsg := 'Bracket error';
        206 : pcalcerrormsg := 'Operator or function expected';
        207 : pcalcerrormsg := 'Empty string encountered';
        208 : pcalcerrormsg := 'Error while removing unary minus';

        301 : pcalcerrormsg := 'Invalid operator encountered';
        302 : pcalcerrormsg := 'Invalid function encountered';
        303 : pcalcerrormsg := 'Invalid constant encountered';
        304 : pcalcerrormsg := 'Stack not empty - Functions or operators missing';
        305 : pcalcerrormsg := 'Invalid postfix';
        306 : pcalcerrormsg := 'CalcFunction only handles functions!';
        307 : pcalcerrormsg := 'CalcOperator only handles operators!';
        308 : pcalcerrormsg := 'Unknown itemtype encountered';

        401 : pcalcerrormsg := 'Tried to pop empty realstack: value / variable / constant missing';
        402 : pcalcerrormsg := 'Tried to pop empty stringstack';

        -1  : pcalcerrormsg := 'Domain must have minimum and maximum value';
            {Special string for dosplot's domain detection}
    else
        pcalcerrormsg := 'Unknown error';
    End;
End;

Begin
    pcalcexception := true;
    pi := system.pi; {Initilize pi global variable to improve performance}
end.
