Program DosPlot;
{
    DosPlot - a dos program that plots graphs
    Copyright (C) 2004  Gert van den Berg

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

To do list:
-----------

3.  Add separate X and Y scale
5b. Translate source to English
6.  Add support for equations with custom subject (e.g. x^3+y^3=6xy
8.  Add screen shot support
10. Add comments to source
11. Port to windows
)

Notes on compiling this program:
--------------------------------
    Free Pascal:
    ------------
        Set target to DOS(GO32v2), WIN32 might work as well.  Do not run WIN32
        version from IDE.
    Turbo/Borland Pascal:
    ---------------------
        Free Pascal compilation is recommended since it generates faster 32bit
        code
        BP only: Set target to REAL MODE or "Protected mode"
}
(* Code for easy copy and pasting
    {$IFDEF EXCEPT}
    try
    except
    end;
    {$ELSE}
    {$ENDIF}
*)
{$F+}{$B-}{$N+}{$E-}{$G+}{$R-}
{$IFDEF FPC}
 {$INLINE ON}
 {$DEFINE EXCEPT}  // Free Pascal het exception support.
                   // Delphi ook maar dit behoort NIE te werk NIE
 {$MODE Delphi}
 {$M 131072, 9000000}
 {$GOTO ON}
{$ENDIF}
{$M 65520,0,655360}

Uses grafunit, pcalcexp, scrsave,crt
{$IFDEF WIN32},windows{$ENDIF}
{$IFDEF LINUX},ptcgraph{$ELSE},Graph{$ENDIF};

Type
{    coordintT  = Record
                     x,y  : longint;
                 End;
    coordrealT = Record
                     x,y  : extended;
                 End;{}
    coordT     = Record
                     x,y        : longint;
                     realx,
                     realy      : extended;
                     valid      : boolean;
                 End;{}
    CalcFuncT  = Function (const p1 : extended;
                           const p2 : postfT;
                           var   p3 : boolean) : extended;
const
    small   :   extended    =   1e-14;
    large   :   extended    =   1e10;

{----------------------------------------------------------------------------}
Function CalcRealY  (   const x     : extended;
                        const pf    : postfT;
                        var   error : boolean) : extended;
Begin
    error := false;
    {$IFDEF EXCEPT}
    try  // sorg dat nie exit op exception
        try
            calcRealy := CalcPostfix(pf,x,0,0);
        except
            calcrealy   := 0;
            error       := true or (calcerror <> 0);
        end;
    except
        error := true;
    end;
    {$ELSE}
    calcRealy      := CalcPostfix(pf,x,0,0);
    if (calcerror <> 0) then
        Begin
            calcrealy := 0;
            error := true;
        End;
    {$ENDIF}
End;
{----------------------------------------------------------------------------}
Function slope  (const   position    :   extended;
                 const   postf       :   postfT;
                 var     error       :   boolean)   :   extended; far;
{Returns and estimation of lim(x->position-)[postf]
Error = true if error calculating limit}
var
    x       :   extended;
    tmp     :   extended;
    tmp2    :   extended;
Begin
    {$IFDEF EXCEPT}
    try
    {$ENDIF}
    tmp     :=  calcrealy(position,postf,error);
    error   :=  error or (CalcError <> 0);
    If abs(tmp) > 1 then
        tmp2    :=  small*tmp
    else
        tmp2    :=  small;
    x       :=  position + tmp2;
    slope   :=  (calcrealy(x,postf,error)-tmp)/tmp2;
    {$IFDEF EXCEPT}
    except
        error   := true;
    end;
    {$ENDIF}
    error   :=  error or (CalcError <> 0);
    If Error then
        slope := 0;
End;
{----------------------------------------------------------------------------}
Function leftvalue (const   position    :   extended;
                    const   postf       :   postft;
                    var     error       :   boolean)   :   extended;
{Returns and estimation of lim(x->position-)[postf]
Error = true if error calculating limit}
var
    x   :   extended;
Begin
    x           := position - small;
    leftvalue   := calcrealy(x,postf,error);
End;
{----------------------------------------------------------------------------}
Function rightvalue(const   position    :   extended;
                    const   postf       :   postft;
                    var     error       :   boolean)   :   extended;
{Returns and estimation of lim(x->position+)[postf]
Error = true if error calculating limit}
var
    x   :   extended;
Begin
    x           := position + small;
    rightvalue  := calcrealy(x,postf,error);
End;
{----------------------------------------------------------------------------}
Function limitvalue(const   position    :   extended;
                    const   postf       :   postft;
                    var     error       :   boolean)   :   extended;
{Returns and estimation of lim(x->position)[postf]
Error = true if error calculating limit}
var
    l,r     :   extended;
    errorl,
    errorr  :   boolean;
Begin
    l           :=  leftvalue (position,postf,errorl);
    r           :=  rightvalue(position,postf,errorr);
    If (abs(l-r) < 1e-10) then
        begin
            limitvalue  :=  (l+r)/2;
            error       := false;
        end
    else
        error   :=  true;

    error       :=  error or errorl or errorr;
End;
{----------------------------------------------------------------------------}
Function discontinuous(const   min, max    :   extended;
                       const   postf       :   postft;
                       var     position    :   extended)   :   boolean;
{Return false if a function does not contain jump discontinuities between min
and max.

Position will contain position of discontinuity.

In order for this to work properly the gap between min and max must be
sufficiently small

Will return false if gradient is very large at cen or if postf(min/max/cen)
is undefined}
    Function Disconrecur(min,max        :   extended) : boolean;
    var
        discon      :   boolean;
        cen         :   extended;   { x value of center}
        miny,
        maxy,
        ceny        :   extended;
        mleft,
        mright,
        mcen        :   extended;
        error       :   boolean;
        cenl,cenr   :   extended;
        cenly,
        cenry       :   extended;
        mll,mlc,
        mcr,mrr     :   extended;
        dmp         :   boolean;
    label funcend;
    Begin
        error   := false;
        discon  := false;
        cen     :=  (min + max)/2;
        if (cen = min) then
            Begin
                discon  := false;
                goto funcend;
            End;
        miny    :=  limitvalue(min,postf,error);
        if error then
            Begin
                position    :=  min+small;
                GOTO funcend;
            end;
        maxy    :=  limitvalue (max,postf,error);
        if error then
            Begin
                position    :=  max-small;
                GOTO funcend;
            end;
        ceny    :=  limitvalue(cen,postf,error);
        error   :=  CalcResult <> 0;
        if error then
            Begin
                position    :=  cen;
                GOTO funcend;
            end;
        {$IFDEF EXCEPT}
        try
            mleft   := (ceny-miny)/(cen-min);
        except
            position    :=  (cen+min)/2;
            error   := true;
        end;
        try
            mright  := (ceny-maxy)/(cen-max);
        except
            position    :=  (cen+max)/2;
            error   := true;
        end;
        try
            mcen    := (maxy-miny)/(max-min);
        except
            position    :=  cen;
            error   := true;
        end;
        {$ELSE}
        if (cen - min) <> 0 then
            mleft   := (ceny-miny)/(cen-min)
        else
            Begin
                error   := true;
                position    := (cen + min)/2;
                GOTO funcend;
            End;
        if (cen - max) <> 0 then
            mright  := (ceny-maxy)/(cen-max)
        else
            Begin
                error   := true;
                position    := (cen + max)/2;
                GOTO funcend;
            End;
        if (max - min) <> 0 then
            mcen  := (maxy-miny)/(max-min)
        else
            Begin
                error   := true;
                position    := cen;
                GOTO funcend;
            End;
        {$ENDIF}
        if error then
            GOTO funcend;
        If ((mleft*mright > 0)and (mcen*mleft > 0) and (mcen*mright > 0)) then
            Begin
                discon  := false;
                error   := false;
                goto FuncEnd;
            End
        else
            discon  := true;
        cenl    :=  (cen+min)/2;
        cenr    :=  (cen+max)/2;
        cenly   :=  limitvalue(cenl,postf,error);
        if error then
            Begin
                position    :=  cenl;
                GOTO funcend;
            end;
        cenry   :=  limitvalue(cenr,postf,error);
        if error then
            Begin
                position    :=  cenr;
                GOTO funcend;
            end;
        {$IFDEF EXCEPT}
        try
            mll    := (miny-cenly)/(min-cenl);
        except
            position    :=  (min+cenl)/2;
            error   := true;
        end;
        try
            mlc    := (ceny-cenly)/(cen-cenl);
        except
            position    :=  (cenl+cen)/2;
            error   := true;
        end;
        try
            mcr    := (cenry-ceny)/(cenr-cen);
        except
            position    :=  (cenr+cen)/2;
            error   := true;
        end;
        try
            mrr    := (cenry-maxy)/(cenr-max);
        except
            position    :=  (cenr+cen)/2;
            error   := true;
        end;
        {$ELSE}
        if ((cenl - min) <> 0) then
            mll    := (miny-cenly)/(min-cenl)
        else
            Begin
                 position    :=  (min+cenl)/2;
                 error   := true;
                 GOTO funcend;
            end;
        if ((cenl - cen) <> 0) then
            mlc    := (ceny-cenly)/(cen-cenl)
        else
            Begin
                 position    :=  (cenl+cen)/2;
                 error   := true;
                 GOTO funcend;
            end;
        if ((cenr - cen) <> 0) then
            mcr    := (cenry-ceny)/(cenr-cen)
        else
            Begin
                 position    :=  (cenr+cen)/2;
                 error   := true;
                 GOTO funcend;
            end;
        if ((cenr - max) <> 0) then
            mrr    := (cenry-maxy)/(cenr-max)
        else
            Begin
                 position    :=  (cenr+cen)/2;
                 error   := true;
                 GOTO funcend;
            end;
        {$ENDIF}
        if error then
            GOTO funcend;

        if (mll*mlc) < 0 then
            Begin
                discon := true;
                dmp := disconrecur(min,cen);
            end
        else
            Begin
                if ((mcr*mrr) < 0) then
                    Begin
                        discon := true;
                        dmp := disconrecur(cen,max);
                    End
                else
                    Begin
                        error  := false;
                        position := cen;
                        goto funcend;
                    End;
            End;
    funcend:
        disconrecur :=  error or discon;
    End;
Begin
    discontinuous   := disconrecur(min,max);
End;
{----------------------------------------------------------------------------}
Procedure YPlotGraph(Settings    : SettingT;
                    grafiek     : grapht;
                    CalcFunc    : CalcFuncT;
                    form        : string);

var
    oukleur        : word;
    outs           : TextSettingsType;
    nuwedetail     : longint;
    discontinu     : boolean;
    def            : Record
                         maxX   :  single;
                         maxY   :  single;
                         minX   :  single;
                         minY   :  Single;
                         xunits :  single;
                         rndxu  :  word;
                         xusize :  single;
                     End;
    i              : Integer;
    j              : longint;
    oud,nuut       : coordT;
    err            : integer;
    dump           : extended;
    plotslope      : boolean;
    dump2          : boolean;
Begin
    PlotSlope := (@CalcFunc = @Slope);
    OuKleur := GetColor;
    GettextSettings(outs);
    nuwedetail := settings.detail*settings.skaal;
    discontinu := false;
    Setcolor(settings.kleur);
    LogEvent('Attempting to plot '+form);
    If PlotSlope then
        LogEvent('Plotting derivative');

    With Settings do with grafiek do
     Begin
       def.maxX := ((maxX - x) DIV skaal)+1;
       def.minx := -(x div skaal)-1;
       def.maxy := (y div skaal)+1;
       def.miny := ((y - maxy) div skaal)-1;
       If grafiek.min > def.minx then def.minx := grafiek.min;
       If grafiek.max < def.maxX then def.maxX := grafiek.max;
       def.xunits := (def.maxX - def.minx);
       def.rndxu  := Saferound(def.xunits);
       def.xusize := def.xunits/def.rndxu;
       nuwedetail := Saferound(nuwedetail * def.xusize); { ensure detail is
                                                           correct}
       {Variables initialized}

       If linemode then
        Begin
          nuut.realx := def.minx;
          oud.x      := round(x+(nuut.realx*skaal));
          oud.realx  := def.minx;
          nuut.Realy := CalcFunc(nuut.realx,postfix,discontinu);
          oud.y      := Saferound(y-nuut.realy*skaal);
          err        := CalcResult;
          discontinu := discontinu or (err <> 0);
          oud.valid  := discontinu;
        end; {Start position of graph calculated}

       For i := 0 to (def.rndXU-1) do
        Begin
           For j:= 0 to (nuwedetail -1) do
            Begin
               nuut.Realx   := (def.minx + (i+j/nuwedetail)*def.xusize);
               nuut.Realy   := CalcFunc(nuut.realx,postfix,dump2);
               Err := CalcResult;
               nuut.x     := round(x+(nuut.realx*skaal));
{               LogEvent('RealX: '+Num2Str(realx)+' Err: '+Int2Str(err));{VERY slow debugging line}
               If (err = 0) then
                  nuut.y     := Saferound(y-nuut.realy*skaal);

               If linemode then {If connect the dots}
                Begin
                   oud.x     := round(x+(oud.realx*skaal));
                   If discontinu then oud.y := nuut.y; {As vorige keer asimptoot was}

                   if (err <> 0) then
                       discontinu := true
                   else
              (*     if plotslope then
                    Begin
                        {$IFDEF FPC}
                        try
                        {$ENDIF}
                        If (abs(y-calcrealy(nuut.realx,postfix,discontinu)) >= 32767) then
                            discontinu := true;
                        if (abs(y-nuut.realy*skaal) >= 10000) then {Prevent overflows}
                            discontinu := true
                        {$IFDEF FPC}
                        except
                            discontinu := true
                        End;
                        {$ENDIF}
                    End else (* *)
                   if (abs(y-nuut.realy*skaal) >= 32767) then {Prevent overflows}
                       discontinu := true
                   else
                       discontinu := discontinuous(oud.realx,nuut.realx,
                                                   postfix,dump) or dump2;{}

                   If not discontinu then
                    Begin
                      If (nuut.x <> oud.x) or (nuut.y <> oud.y) then  {for speed.  Will not put same pixel twice}
                         line(oud.x,oud.y,nuut.x,nuut.y);
                      nuut.valid := true;
                    End;
                end else
                If err = 0 then
                 Begin
                   If (nuut.x <> oud.x) or (nuut.y <> oud.y) then  {for speed.  Will not put same pixel twice}
                       PutPixel(nuut.x,nuut.y,kleur);
                 End;{If err =0 /if linemode}
               If (oud.x <> nuut.x) then
                Begin
                  PutPixel(nuut.x, maxy, blue);
                End;
               Oud := nuut;
            End; {For j}
        End;{for i}
     End; {With Settings}

    SetColor(0);
    Line(0,maxy,maxX,maxy); {Ensure progress indicator is erased}
    LogEvent('Plotted '+form);
    with outs do
        Begin
            SetColor(oukleur);
            SettextStyle(font,direction,Charsize);
            SettextJustify(horiz,vert);
        End;
End;
{----------------------------------------------------------------------------}
Function GetScriptLine(Var lr       : text;
                       var linenum  : word;
                       var error    : byte) : string;
var
    Str :   String;
Begin
{$IFDEF EXCEPT}
    try
        Readln(lr,str);
        Inc(linenum);
        GetScriptLine := str;
    except
        LogEvent('Fatal Error reading line '+Int2Str(linenum)+
                 ': Unexpected end of file.');
        Error := 99;
        GetScriptLine := '';
    end;

{$ELSE}
    {$I-}Readln(lr,str);{$I+}
    Inc(linenum);
    GetScriptLine := str;
    If IOResult <> 0 then
        Begin
            LogEvent('Fatal Error reading line '+Int2Str(linenum)+
                     ': Unexpected end of file.');
            Error := 99;
            GetScriptLine := '';
        End;
{$ENDIF}
End;
{----------------------------------------------------------------------------}
Function GetParamCount(const line : string) : byte;
var
    i       :  integer;
    antw    :  byte;
Begin
    antw    :=  0;
    For i :=1 to length(line) do
         Begin
            If (antw = 0) and (line[i] = ' ') then
                Inc(antw);
            If (line[i] = '|') then
                Inc(antw);
         End;
    GetParamCount := antw;
End;
{----------------------------------------------------------------------------}
Function GetParam(const line : string;no : byte) : String;
var
    i       :  integer;
    ts      :  string;
    current :  byte;
Begin
    current :=  0;
    ts      := '';
    For i :=1 to length(line) do
        Begin
            If (current = 0) and (line[i] = ' ') then
                Inc(current);
            If (line[i] = '|') then
                Inc(current);
            If (current = no) then
                ts := ts + line[i];
            If (current > no) then
                exit;
        End;
    GetParam := ts;
End;
{----------------------------------------------------------------------------}
Function CalcValu  (const str : string;
                    const x,y : Extended;
                    Var error : boolean) : Extended;
var
    postf   :   PostfT;
Begin
    error   :=  false;
    {$IFDEF EXCEPT}
    try
    {$ENDIF}
        String2Postf(str, postf);
    {$IFDEF EXCEPT}
    except
        error := true;
        CalcValu    := 0;
    end;
    {$ENDIF}
    If CalcError <> 0 then
        error   :=  true;

    If not (error) then
        Begin
            {$IFDEF EXCEPT}
            try
            {$ENDIF}
                CalcValu := CalcPostfix(postf,x,y,0);
            {$IFDEF EXCEPT}
            except
                error       := true;
                CalcValu    := 0;
            End;
            {$ENDIF}
            If CalcError <> 0 then
                error   :=  true;
        End;
    If Error then
        CalcValu  := 0;
End;
{----------------------------------------------------------------------------}
Function Scriptplot(Settings : SettingT;filename : string) : byte;
{Error codes:
0  : none
1  : Non- fatal
99 : Fatal
}
Var
    oukleur        : word;
    outs           : TextSettingsType;
    srp            : TEXT;
    ioe            : integer;
    str,ts         : String;
    error          : byte;
    i, line        : Word;
    err            : integer;
    grafiek        : grapht;
    li             : longint;
    ce             : integer absolute li;
    LineType       : Commandt;
    boolerr        : boolean;
    parm           : string;
label
    readline, {Position of Readln(), used if line is empty}
    cleanup; {Program jump daarheen op fatal errors}

Begin
    error := 0;
    GettextSettings(outs);
    ioe := IOResult;
    OuKleur := GetColor;
    ioe := 0;

    LogEvent('Plotting from '+filename+' started');

    Case openscript(srp,filename) of
        0   :   LogEvent('File Opened successfully');
    else
        Goto Cleanup;
    End;

    line := 0;

    If not(Eof(srp)) then
        Begin
            str := GetScriptLine(srp,line,error);

            if (error = 99) then
                GOTO cleanup;
        End
    else {If not Eof}
        Begin
            LogEvent('Fatal Error: File Empty.');
            Error := 99;
            GOTO Cleanup;
        End;{else of in not Eof}

    LogEvent('Processing line no. '+int2str(line)+' "'+str+'"');
    LineType := IDScriptLine(str);
    If (LineType <> headerl) then
        Begin
            LogEvent('Error: Invalid file format.');
            Error := 99;
            GOTO Cleanup;
        End;

Readline:
    While not(Eof(srp)) do
        Begin
            LogEvent('Processing line no. '+int2str(line)+' "'+str+'"');
            str := UpperCase(GetScriptLine(srp,line,error));

            if (error = 99) then
                GOTO cleanup;

            If (str = '') then
                goto readline;

            LineType := IDScriptLine(str);
            LogEvent(ScriptLineDescript(LineType));

            If LineType = UnknownL then
                error := 1;

            If (MinParams(LineType) < GetParamCount(str)) then
                Begin
                    LogEvent('Too little parameters for line');
                    error := 1;
                End;
{Up 2 date to here}

            Case LineType of
                comment     :   ;{Do nothing}
                setcolorl   :
                    Begin
                        parm := GetParam(str,1);{TODO}
                        ioe  := SafeRound(
                                    ParseString(parm,boolerr,MaxX,MaxY,0)
                                );
                        If not(boolerr) then
                            Begin
                                Settings.kleur := ioe;
                                LogEvent('Color set.');
                            End
                        else
                            Begin
                                LogEvent('Error parsing color');
{                                error{}
                            End;
                    End;
                cls         :
                    Begin
                        ClearViewPort;
                        Asse(Settings);
                        LogEvent('Screen cleared');
                    End;
                setdetail   :
                    Begin
                    End;
                setlinemode :
                    Begin
                    End;
                setscale    :
                    Begin
                    End;
                setx        :
                    Begin
                    End;
                sety        :
                    Begin
                    End;
                plotx       :
                    Begin
                    End;
                ploty       :
                    Begin
                    End;
                plotxd      :
                    Begin
                    End;
                plotyd      :
                    Begin
                    End;
                putdotl     :
                    Begin
                    End;
                savel       :
                    Begin
                    End;
                loadl       :
                    Begin
                    End;
                pushposl    :
                    Begin
                    End;
                delayl      :
                    Begin
                    End;
                waitl       :
                    Begin
                    End;
                headerl     :
                    Begin
                        LogEvent('Error: Header not expected here');
                        error   := 1;
                    End;
            else
                LogEvent('Invalid Line');
            End;
            Case str[1] of
              ';' : ;
              'C' : Begin
                        if str[2] = ' ' then
                            begin
                                if length(ts) = 1 then
                                    case ts[1] of
                                        '0'  : settings.kleur := 0;
                                        '1'  : settings.kleur := 1;
                                        '2'  : settings.kleur := 2;
                                        '3'  : settings.kleur := 3;
                                        '4'  : settings.kleur := 4;
                                        '5'  : settings.kleur := 5;
                                        '6'  : settings.kleur := 6;
                                        '7'  : settings.kleur := 7;
                                        '8'  : settings.kleur := 8;
                                        '9'  : settings.kleur := 9;
                                    else
                                        LogEvent('Line '+Int2str(line)+' contains an invalid color, '+ts+'.');
                                    end {case}
                                else {if length}
                                if (length(ts) = 2) and (ts[1]='1') then
                                    case ts[2] of
                                       '1'  : settings.kleur := 11;
                                       '2'  : settings.kleur := 12;
                                       '3'  : settings.kleur := 13;
                                       '4'  : settings.kleur := 14;
                                       '5'  : settings.kleur := 15;
                                    else {case}
                                        LogEvent('Line '+Int2str(line)+' contains an invalid color, '+ts+'.');
                                        error := 1;
                                    end;{if length}
                            end else {if str[2]}
                                if ((str[1]+str[2]+str[3])= 'CLS') then
                                    begin
                                        GraphMode('screen');
                                        Asse(Settings);
                                        LogEvent('Screen cleared');
                                    end;{if 'cls'}
                    End;      {'C'}
              'D' : Begin
                        val(ts,i,err);
                        If (err = 0) and (i > 0) then
                            settings.detail := i
                        else
                        Begin
                            LogEvent('Detail value invalid');
                            Error := 1;
                        End;
                    end; {'D'}
              'L' : Begin
                       If (length(ts) = 1) then
                           case ts[1] of
                               '1' : settings.linemode := true;
                               '0' : settings.linemode := false;
                               'T' : settings.linemode := not(settings.linemode)
                           else
                               error := 1;
                               LogEvent('Linemode must be set to "1","0" or "T".');
                           end;{case & if}
                    end; {'L'}
              'S' : Begin
                        val(ts,i,err);
                        If (err = 0) and (i > 0) then
                            settings.skaal := i
                        else
                        Begin
                            LogEvent('Scale value invalid');
                            Error := 1;
                        End; {If err = 0}
                    end; {'S'}
              'Y' : Begin
                        If (ts <> 'C') then
                            Begin
                                val(ts,i,err);
                                If (err = 0) and (i > 0) then
                                    settings.x := i
                                else
                                Begin
                                    LogEvent('Y-axis position invalid');
                                    Error := 1;
                                End;
                            end else
                                settings.x := maxX div 2;
                    end; {'Y'}
              'X' : Begin
                        If (ts <> 'C') then
                            Begin
                                val(ts,i,err);
                                If (err = 0) and (i > 0) then
                                    settings.y := i
                                else
                                Begin
                                    LogEvent('X-axis position invalid');
                                    Error := 1;
                                End;
                            end else
                                settings.Y := maxy div 2;
                    end; {'X'}
              'P' : Begin
                        li := Str2graph(ts,grafiek);

                        If ce = 0 then
                            Begin
                                PrecalcPostfix(grafiek.postfix);
                                If pcalcexp.calcerror = 0 then
                                    Begin
                                        {$IFDEF FPC}
                                        YPlotGraph(Settings, grafiek,@CalcRealy,ts);
                                        {$ELSE}
                                        YPlotGraph(Settings, grafiek,CalcRealy,ts);
                                        {$ENDIF}
                                        LogEvent('Graph "'+ts+'" plotted');
                                    End else
                                    Begin
                                        LogEvent('Ongeldige vergelyking: "'+ts+'"');
                                        LogEvent('Error '+Int2Str(pcalcexp.calcerror)+': '+pcalcErrorMsg(pcalcexp.calcerror));
                                        Error := 1;
                                    End;
                            End else
                            Begin
                                LogEvent('Ongeldige vergelyking: "'+ts+'"');
                                LogEvent('Error '+int2Str(ce)+': '+pcalcErrorMsg(ce));
                                Error := 1;
                            End;
                    end; {'P'}
            else
                LogEvent('Error identifying line.');
                if length(str) > 0 then
                    error := 1;
            end; {case str[1] of}

            LogEvent('Finished with line '+Int2Str(line));
        End; {While not(Eof)}

cleanup:
    LogEvent('Plotting completed.');
    case error of
        0   : LogEvent('No errors encountered');
        1   : LogEvent('Non-fatal errors encountered');
        99  : LogEvent('Fatal error encountered');
    End;
    ScriptPlot := Error;
    LogEvent('--------------------------------------------------------');
{$IFDEF EXCEPT}
    try
        Close(srp);
    except
        LogEvent('Error Closing file "'+filename+'" after plotting.');
    end;
{$ELSE}
    {$I-}Close(srp);{$I+}
    error := IOResult;
{$ENDIF}
    with outs do
        Begin
            SetColor(oukleur);
            SettextStyle(font,direction,Charsize);
            SettextJustify(horiz,vert);
        End;
{$IFNDEF WIN32}    {TODO}
    If settings.view then readkey;
{$ENDIF}
End;
{----------------------------------------------------------------------------}
Procedure Menu;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
Procedure SettingsWindow(Settings : SettingT);
Begin
{$IFNDEF LINUX}
    Window(63,10,79,30);
{$ELSE}
    Window(63,4,79,24);
{$ENDIF}
    TextBackground(Green);
    ClrScr;
{$IFNDEF LINUX}
    Window(64,11,79,29);
{$ELSE}
    Window(64,5,79,24);
{$ENDIF}
    TextColor(White);
    WriteLn('Settings:');
    WriteLn;
    WriteLn('Axes:');
    WriteLn(' x=',Settings.Y);
    WriteLn(' y=',Settings.X);
    WriteLn(' Scale=',Settings.skaal);
    Writeln;
    Writeln('Graph:');
    Writeln(' Colour=',Settings.Kleur);
    Writeln(' Detail=',Settings.detail);
    Writeln(' Linemode=',Settings.linemode);
    Writeln(' Autoview=',settings.view);
    Writeln;
    Writeln('Graphics:');
    Writeln(' Driver=',Settings.gd);
    Writeln(' Mode=',Settings.gm);
    Write  ('  ',maxX+1,'X',maxy+1);
End;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
var
    ch          : char;
    Settings    : SettingT;
{    i           : shortint;}
    form        : shortstring;
    grafiek     : grapht;
    li          : longint;
    errdet      : record
                    ce   : smallint;
                    s2g  : smallint;
                  end absolute li;
    derivative : boolean;

Begin
    LoadSettings(configfilename,settings);
    LogEvent('DosPlot '+version+' started');
    LogEvent(BuildString);
    LogEvent('Settings:');
    LogEvent('Driver: '+Int2str(settings.gd)+' '+Getdrivername);
    LogEvent('Mode: '+Int2str(settings.gm)+' ' + Getmodename(settings.gm));
    LogEvent('   '+Int2str(maxX)+'x'+Int2str(maxy));
    LogEvent('   Colours: '+Int2str(getmaxcolor));
    LogEvent('Asse: X='+Int2str(settings.y)+' Y='+Int2Str(settings.X));
    LogEvent('Scale: '+Int2Str(Settings.skaal));
    If settings.linemode then
        LogEvent('Linemode: ON  Detail='+Int2Str(settings.detail))
    Else
        LogEvent('Linemode: OFF Detail='+Int2Str(settings.detail));

    Asse(Settings);
    Repeat
        TxtMode('screen');   {}
        ch := #0;
        TextMode(co80+font8x8);
        TextBackground(Blue);
        ClrScr;
        TextColor(LightGray);
        GotoXY(48,49);
        Write('Copyright '+year+' Gert van den Berg');
        GotoXY(49,50);
        Write('See About / Credits for details');
{$IFNDEF LINUX}
        Window(20,14,60,50-18);
{$ELSE}
        Window(20,2,60,24);
{$ENDIF}
        TextBackground(Red);
        ClrScr;
        SettingsWindow(Settings);
        TextBackground(Red);
{$IFNDEF LINUX}
        Window(23,16,80-23,51-18);
{$ELSE}
        Window(23,3,80-23,24);
{$ENDIF}
        TextColor(Yellow);
        Writeln('DosPlot Version '+version);
        Writeln('--------------------');
        Writeln;
    {$IFNDEF WIN32}
        Writeln('0.  Show screen with graphs');
    {$ENDIF}
        Writeln('1.  Plot Graph');
        Writeln('2.  Plot Graph of derivative');
        Writeln('3.  Plot Graph(s) from script');
        Writeln('4.  Save screen dump to file');
        Writeln('5.  Load screen dump from file');
        Writeln('6.  Settings');
        Writeln('8.  Plot Axes & clear screen');
        {$IFNDEF FPC}
            WriteLn('9.  Credits');
        {$ELSE}
            WriteLn('9.  About');
        {$ENDIF}
        Writeln;
        Writeln('Esc Exit');
        Repeat
            ch := Upcase(ReadKey);
        Until ch in ['0'..'6',#27,'8'..'9'];
        derivative := (ch = '2');
        If derivative then
            ch := '1';
        Case (ch) of
    {$IFNDEF WIN32}
            '0' :
                Begin
                    GraphMode('screen');
                    Readkey;
                End;
    {$ENDIF}
            '8' :
                Begin
                    GraphMode('screen');
                    Asse(Settings);
                End;
            '1' :
           Begin
{$IFNDEF LINUX}
              Window(1,1,80,50);
{$ELSE}
              Window(1,1,80,25);
{$ENDIF}
              TextColor(7);
              TextBackground(0);
              ClrScr;
              FuncInfo;
              WriteLn('Enter function: (e.g "y=x*sin(1/x)")');
              If Derivative then
                Writeln('This will plot an estimation of the function''s'+
                        ' derivative');
              Write  (' y=');
              Readln(form);
              li := str2graph(form,grafiek);
              If errdet.ce = 0 then
               Begin
                  If errdet.s2g <> 0 then
                      Begin
                          Writeln;
                          Writeln('Warning: Invalid domain');
                          Writeln('Error no. '+Int2str(errdet.s2g)+' '+pcalcErrorMsg(errdet.s2g));
                          WriteLn;
                          Writeln('Press any key to continue...');
                          readkey;
                      end;
                  {$IFDEF EXCEPT}
                  try
                      pcalcexp.calcerror := 0;
                      LogEvent('Precalculating '+form);
                      PrecalcPostfix(grafiek.postfix);{}
                  except
                      Writeln;
                      TextColor(LightRed);
                      LogEvent('Error precalculating '+form);
                      Writeln('Invalid function ('+form+')');
                      Writeln('Error ',pcalcexp.calcerror,': ',pcalcErrorMsg(pcalcexp.calcerror));
                      TextColor(7);
                      Writeln;
                      Writeln('Press any key to return to menu...');
                      Readkey;
                  End;

                  If pcalcexp.CalcResult = 0 then
                      Begin
                          LogEvent('Precalculation of '+form+' succeeded.');
                          GraphMode('screen');
                          If not derivative then
                            YPlotGraph(Settings, grafiek,@calcrealy,form)
                          else
                            YPlotGraph(Settings, grafiek,@slope,form);
    {$IFNDEF WIN32}
                          If settings.view then readkey;
    {$ENDIF}
                      End;
                  {$ELSE}
                  LogEvent('Precalculating '+form);
                  PrecalcPostfix(grafiek.postfix);{}
                  If pcalcexp.calcerror = 0 then
                      Begin
                          LogEvent('Precalculation of '+form+' succeeded.');
                          GraphMode('screen');
                          If not derivative then
                            PlotGraph(Settings, grafiek,calcrealy,form)
                          else
                            PlotGraph(Settings, grafiek,slope,form);
                          If settings.view then readkey;
                      End
                  else
                      Begin
                          LogEvent('Error precalculating '+form);
                          Writeln;
                          TextColor(LightRed);
                          Writeln('Invalid function');
                          Writeln('Error ',pcalcexp.calcerror,': ',pcalcErrorMsg(pcalcexp.calcerror));
                          TextColor(7);
                          Writeln;
                          Writeln('Press any key to return to menu...');
                          Readkey;
                      End;
                  {$ENDIF}
               End else
               Begin
                  Writeln;
                  TextColor(LightRed);
                  Writeln('Invalid Function');
                  Writeln('Error ',errdet.ce,': ',pcalcErrorMsg(errdet.ce));
                  TextColor(7);
                  Writeln;
                  Writeln('Press any key to return to menu...');
                  Readkey;
               End;
           End;
            '3' :
                Begin
{$IFNDEF LINUX}
                    Window(1,1,80,50);
{$ELSE}
                    Window(1,1,80,25);
{$ENDIF}
                    TextColor(7);
                    TextBackground(0);
                    ClrScr;
                    WriteLn('Please enter script''s filename:');
                    Write  ('>');
                    Readln(form);
                    GraphMode('Screen');
                    ScriptPlot(settings,form);
                End;
            '4' :
                Begin
{$IFNDEF LINUX}
                    Window(1,1,80,50);
{$ELSE}
                    Window(1,1,80,25);
{$ENDIF}
                    TextColor(7);
                    TextBackground(0);
                    ClrScr;
                    WriteLn('Please enter screen dump filename: (View and ',
                            'convert to bitmap with screenview');
                    Writeln('program (included)) (*.dmp)');
                    Write  ('>');
                    Readln(form);
                    If (pos('.',form) = 0) then
                        form := form + '.dmp';
                    GraphMode('screen');
                    SaveScreen(form,Settings);
                    TxtMode('screen');
                End;
            '5' :
                Begin
{$IFNDEF LINUX}
                    Window(1,1,80,50);
{$ELSE}
                    Window(1,1,80,25);
{$ENDIF}
                    TextColor(7);
                    TextBackground(0);
                    ClrScr;
                    WriteLn('Please enter screen dump filename: (*.dmp)');
                    Write  ('>');
                    Readln(form);
                    If ((pos('.',form) = 0) and not(FileExists(form))) then
                        form := form + '.dmp';
                    GraphMode('screen');
                    LoadScreen(form);
                    TxtMode('screen');
                End;
            '6' :
           Begin
              Repeat
                 ch := #0;
                 TextMode(co80+font8x8);
{$IFNDEF LINUX}
                 Window(1,1,80,50);
{$ELSE}
                 Window(1,1,80,25);
{$ENDIF}
                 TextBackground(Blue);
                 ClrScr;
{$IFNDEF LINUX}
                 Window(10,10,80-20,50-10);
{$ELSE}
                 Window(10,2,60,23);
{$ENDIF}
                 TextBackground(Red);
                 ClrScr;
{$IFNDEF LINUX}
                 Window(13,12,80-20,50-12);
{$ELSE}
                 Window(13,3,60,23);
{$ENDIF}
                 TextColor(Yellow);
                 Writeln('Settings:');
                 Writeln('---------');
                 Writeln;
                 Writeln(' 0  Reset defaults');
                 Writeln(' 1  Reset position of Y-axis to screen center');
                 Writeln(' 2  Reset position of X-axis to screen center');
                 Writeln(' 3  Modify position of Y-axis');
                 Writeln(' 4  Modify position of X-axis');
                 Writeln(' 5  Modify graph scale');
                 Writeln(' 6  Modify graph colour');
                 Writeln(' 7  Modify graph detail');
                 Writeln(' 8  Toggle linemode');
                 Writeln(' 9  Toggle autoview');
                 Writeln;
                 Writeln('Esc Return to main menu');
                 Writeln;
                 TextColor(LightRed+blink); Writeln('Warning:');
                 TextColor(15); Writeln('Options 0-5 clear the graphing screen');
                 SettingsWindow(Settings);
{$IFNDEF LINUX}
                 Window(1,1,80,50);
{$ELSE}
                 Window(1,1,80,25);
{$ENDIF}
                 ch := Readkey;
                 TextColor(7);
                 TextBackground(0);
                 ClrScr;
                 Case ch of
                    '0' :
                         Begin
                            DefSetts(settings);
                            GraphMode('screen');
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '1' :
                         Begin
                            GraphMode('screen');
                            Settings.X := MaxX div 2;
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '2' :
                         Begin
                            GraphMode('screen');
                            Settings.Y := MaxY div 2;
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '3' :
                         Begin
                            ClrScr;
                            WriteLn('New position of Y-axis (in pixels)? (0-639 in VGA mode(default))');
                            Write('>');
                            Readln(Settings.X);
                            GraphMode('screen');
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '4' :
                         Begin
                            WriteLn('New position of X-axis (in pixels)? (0-479 in VGA mode(default))');
                            Write('>');
                            Readln(Settings.Y);
                            GraphMode('screen');
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '5' :
                         Begin
                            WriteLn('New scale? (1-65535 pixels per unit)');
                            Write('>');
                            Readln(Settings.skaal);
                            GraphMode('screen');
                            Asse(Settings);
                            TxtMode('screen');
                         End;
                    '6' :
                         Begin
                           Kleurkodes;
{$IFNDEF LINUX}
                           Window(1,1,80,50);
{$ELSE}
                           Window(1,1,80,25);
{$ENDIF}
                           TextColor(7);
                           TextBackground(0);
                           WriteLn('New colour''s code?');
                           Write('>');
                           Readln(Settings.kleur);
                         End;
                    '7' :
                         Begin
                            Writeln('Please note: Increasing the detail can hamper performance.  Default: 50');
                            WriteLn('New detail? (1-65535 calculations per pixel)');
                            Write('>');
                            Readln(Settings.detail);
                         End;
                    '8' :   settings.linemode := not(settings.linemode);
                    '9' :   settings.view := not(settings.view);
                 End;
              Until ch=#27;
              ch := #0;
           End;
            '9' :  Credits;

        End; {Main menu case}
    Until ch=#27;
    SaveSettings(configfilename,settings);

    LogEvent('DosPlot '+version+' terminating...');
    LogEvent(BuildString);
    LogEvent('Settings:');
    LogEvent('Driver: '+Int2str(settings.gd)+' '+Getdrivername);
    LogEvent('Mode: '+Int2str(settings.gm)+' ' + Getmodename(settings.gm));
    LogEvent('   '+Int2str(maxX)+'x'+Int2str(maxY));
    LogEvent('   Colours: '+Int2str(getmaxcolor));
    LogEvent('Asse: X='+Int2str(settings.y)+' Y='+Int2Str(settings.X));
    LogEvent('Scale: '+Int2Str(Settings.skaal));
    If settings.linemode then
        LogEvent('Linemode: ON  Detail=' + Int2Str(settings.detail))
    Else
        LogEvent('Linemode: OFF Detail=' + Int2Str(settings.detail));
    LogEvent('DosPlot '+version+' terminated normally');
    LogEvent('-----------------------------------------------------------');
End;
{----------------------------------------------------------------------------}
var OldMode :integer;
Begin
    {$IFNDEF EXCEPT}
    pcalcexception := false;
    {$ENDIF}
    oldmode := LastMode;
    LogEvent('-----------------------------------------------------------');
    DeleteScrFiles('screen');
    Directvideo := false;
    If (ParamStr(1) = '/?') Then
        Begin
            ClrScr;
            Writeln('USAGE:');
            Writeln(' GRAFIEK [/G]');
            Writeln('    /G (Optional) Modify display mode');
            Halt;
        End;
    ClrScr;
    Init;
    scrgmode := true;
    Menu;
    DeleteScrFiles('screen');
    TextMode(oldmode);
End.
