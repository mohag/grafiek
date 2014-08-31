{
    DosPlot - a dos program that plots graphs
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

To do list:
-----------

3.  Add seperate X and Y scale
5b. Translate source to English
6.  Add support for equations with custom subject (e.g. x^2+y^2=100
8.  Add screenshot support
10. Add comments to source
11. Port to windows
)

Please note that this program may compile to win32.  This is not supported!

Notes on compiling this program:
--------------------------------
    Free Pascal:
    ------------
        Set target to DOS(GO32v2)
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
{$B-}
{$N+}
{$E-}
{$G+}
{$R-}
{$IFDEF FPC}
 {$INLINE ON}
 {$DEFINE EXCEPT}  // Free Pascal het exceptions wat gehandle moet word.
                   // Delphi ook maar dit behoort NIE te werk NIE
 {$MODE Delphi}
 {$M 131072, 9000000}
 {$GOTO ON}
{$ENDIF}
{$M 65520,0,655360}
Program DosPlot;

Uses Graph, grafunit, pcalcexp, scrsave,
{$IFDEF WIN32}crt,windows{$ELSE}crt{$ENDIF};

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
const
    small   :   extended    =   1e-14;
    large   :   extended    =   1e10;
{----------------------------------------------------------------------------}
Function CalcRealY  (   const x,y,z  : extended;
                        const pf : postft) : extended;
{$IFDEF FPC}inline;{Ensures faster code}{$ENDIF}
Begin
    {$IFDEF EXCEPT}
    try  // sorg dat nie exit op exception
        try
            calcRealy := CalcPostfix(pf,x,y,z);
        except
            calcrealy := 0;
        end;
    except
        ;
    end;
    {$ELSE}
    calcRealy      := CalcPostfix(pf,x,y,z);
    if calcerror <> 0 then
        calcrealy := 0;
    {$ENDIF}
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
    leftvalue   := calcrealy(x,0,0,postf);
    error       := (CalcResult <> 0);
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
    rightvalue  := calcrealy(x,0,0,postf);
    error       := (CalcResult <> 0);
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
Function slope  (const   position    :   extended;
                 const   postf       :   postft;
                 var     error       :   boolean)   :   extended;
{Returns and estimation of lim(x->position-)[postf]
Error = true if error calculating limit}
var
    x   :   extended;
Begin
    x       :=  position + small;
    slope   :=  (calcrealy(x,0,0,postf)-calcrealy(position,0,0,postf))/small;
    error   :=  (CalcResult <> 0);
End;
{----------------------------------------------------------------------------}
Function discontinuous(const   min, max    :   extended;
                       const   postf       :   postft;
                       var     position    :   extended)   :   boolean;
{Return false if a function does not contain jump dicontinuities between min
and max.

Position will contain position of dicontinuity.

In order for this to work properly the gap between min and max must be
suffiently small

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
Procedure PlotGraph(Settings    : SettingT;
                    grafiek     : grapht;
                    yval        : extended;
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

Begin
    OuKleur := GetColor;
    GettextSettings(outs);
    nuwedetail := settings.detail*settings.skaal;
    discontinu := false;
    Setcolor(settings.kleur);
    LogEvent('Attempting to plot '+form);

    With Settings do with grafiek do
     Begin
       def.maxx := ((maxX - x) DIV skaal)+1;
       def.minx := -(x div skaal)-1;
       def.maxy := (y div skaal)+1;
       def.miny := ((y - maxy) div skaal)-1;
       If grafiek.min > def.minx then def.minx := grafiek.min;
       If grafiek.max < def.maxx then def.maxx := grafiek.max;
       def.xunits := (def.maxx - def.minx);
       def.rndxu  := Saferound(def.xunits);
       def.xusize := def.xunits/def.rndxu;
       nuwedetail := saferound(nuwedetail * def.xusize); { ensure detail is
                                                           correct}
       {Variables initialized}

       If linemode then
        Begin
          nuut.realx := def.minx;
          oud.x      := round(x+(nuut.realx*skaal));
          oud.realx  := def.minx;
          nuut.Realy := CalcRealy(nuut.realx,yval,0,postfix);
          oud.y      := Saferound(y-nuut.realy*skaal);
          err        := calcresult;
          discontinu := (err <> 0);
          oud.valid  := discontinu;
        end; {Start position of graph calculated}

       For i := 0 to (def.rndXU-1) do
        Begin
           For j:= 0 to (nuwedetail -1) do
            Begin
               nuut.Realx   := (def.minx + (i+j/nuwedetail)*def.xusize);
               nuut.Realy   := CalcRealy(nuut.realx,yval,0,postfix);
               Err := calcresult;
               nuut.x     := round(x+(nuut.realx*skaal));
{               LogEvent('RealX: '+Num2Str(realx)+' Err: '+Int2Str(err));{VERY slow debugging line}
               If (err = 0){ and ((y-realy*skaal) < y+200)} then
                  nuut.y     := Saferound(y-nuut.realy*skaal);

               If linemode then {If connect the dots}
                Begin
                   oud.x     := round(x+(oud.realx*skaal));
                   If discontinu then oud.y := nuut.y; {As vorige keer asimptoot was}

                   if (err <> 0) then
                       discontinu := true
                   else
                   if (abs(y-nuut.realy*skaal) >= 32767) then {Prevent overflows}
                       discontinu := true
                   else
                       discontinu := discontinuous(oud.realx,nuut.realx,
                                                   postfix,dump);{}

                   If not discontinu then
                    Begin
                      If (nuut.x <> oud.x) or (nuut.y <> oud.y) then  {for speed.  Will not put same pixel twice}
                         line(oud.x,oud.y,nuut.x,nuut.y);
                      nuut.valid := true;
                    End;
                   oud := nuut;
                end else
                If err = 0 then
                 Begin
                   If (nuut.x <> oud.x) or (nuut.y <> oud.y) then  {for speed.  Will not put same pixel twice}
                       PutPixel(nuut.x,nuut.y,kleur);
                 End;{If err =0 /if linemode}
               If (oud.x <> nuut.x) then
                Begin
                  PutPixel(oud.x,  maxy, black);
                  PutPixel(nuut.x, maxy, lightred);
                End;
               Oud := nuut;
            End; {For j}
        End;{for i}
     End; {With Settings}

    SetColor(0);
    Line(0,maxy,maxx,maxy); {Ensure progress indicator is erased}
    LogEvent('Plotted '+form);
    with outs do
        Begin
            SetColor(oukleur);
            SettextStyle(font,direction,Charsize);
            SettextJustify(horiz,vert);
        End;
End;
{----------------------------------------------------------------------------}
Procedure Scriptplot(Settings : SettingT;filename : string);
{Error codes:
0  : none
1  : Non- fatal
99 : Fatal
}
Var
    oukleur        : word;
    outs           : textsettingsType;
    lr             : TEXT;
    ioe            : integer;
    str,ts         : String;
    error          : byte;
    i, line        : Word;
    err            : integer;
    grafiek        : grapht;
    li             : longint;
    ce             : integer absolute li;

label
    readline, {Position of readln(), used if line is empty}
    cleanup; {Program jump daarheen op fatal errors}

Begin
   error := 0;
   LogEvent('Plotting from '+filename+' started');
   GettextSettings(outs);
   ioe := IOResult;
   If Ioe <> 0 then
        ioe := 0;
   OuKleur := GetColor;
   {$IFDEF EXCEPT}
   Try
      Assign(lr, filename);
   except
      LogEvent('Fatal Error: Invalid Filename! Long filenames not allowed');
      error := 99;
   end;

   if (error = 99) then
       GOTO cleanup;

   try
      Reset(lr);
   except
      LogEvent('Fatal Error: Cannot open file "'+filename+'".');
      error := 99;
   end;

   if (error = 99) then
       GOTO cleanup;

   {$ELSE}
   {$I-}Assign(lr,filename);{$I-}
   IF IOResult = 0 then
      {$I-}REset(lr){$I+}
    Else
     Begin
        LogEvent('Fatal Error: Filename invalid.  Long filenames not allowed.');
        error := 99;
        GOTO cleanup;
     End;

   If (Ioresult <> 0) then
    Begin
       LogEvent('Fatal Error: Cannot open file "'+'".');
       error := 99;
       GOTO cleanup;
    End;
   {$Endif}

   LogEvent('File Opened successfully');
   If not(Eof(lr)) then
      Begin
      {$IFDEF EXCEPT}
          try
              Readln(lr,str);
          except
              LogEvent('Fatal Error: File shorter than one line.');
              Error := 99;
          end;

          if (error = 99) then
              GOTO cleanup;

      {$Else}
          {$I-}Readln(lr,str);{$I+}
          If (IOResult <> 0) then
             Begin
                LogEvent('Fatal Error: File shorter than one line.');
                Error := 99;
                GOTO Cleanup;
             End;
      {$ENDIF}
      End else {If not Eof}
      Begin
         LogEvent('Fatal Error: File Empty.');
         Error := 99;
         GOTO Cleanup;
      End;{else of in not eof}

   If (UpperCase(str) <> 'GRAPHSCRIPT') then
       Begin
           LogEvent('Error: Invalid file format.');
           Error := 99;
           GOTO Cleanup;
       End; {if not "graphscript"}

   line := 1;
Readline:
   While not(Eof(lr)) do
     Begin
         {$IFDEF EXCEPT}
         try
             Readln(lr,str);
             Inc(line);
         except
             LogEvent('Fatal Error: Unexpected end of file.');
             Error := 99;
         end;

         if (error = 99) then
             GOTO cleanup;

         {$ELSE}
         {$I-}Readln(lr,str);{$I+}
         Inc(line);
         If IOResult <> 0 then
            Begin
               LogEvent('Error: Unexpected end of file.');
               Error := 99;
               GOTO Cleanup;
            End;
         {$ENDIF}
         If str = '' then
            goto readline;
         Str := Uppercase(str);
         LogEvent('Processing line no. '+int2str(line)+' "'+str+'"');
         If length(str) >= 1 then
             Case str[1] of
                 ';' : LogEvent('Line is commment');
                 'C' : Begin
                           if Str[2] = ' ' then
                               LogEvent('Line sets color')
                           else
                               if ((str[1]+str[2]+str[3])= 'CLS') then
                                   LogEvent('Line clears screen')
                               else
                               Begin
                                   LOgEvent('Error identifying line');
                                   error := 1;
                               end;
                       End;
                 'D' : LogEvent('Line sets detail');
                 'L' : LogEvent('Line sets linemode');
                 'S' : LogEvent('Line sets scale');
                 'X' : LogEvent('Line sets X-axis');
                 'Y' : LogEvent('Line sets Y-axis');
                 'P' : LogEvent('Line plots a graph');
             else
                 error := 1;
                 LogEvent('Error identifying line.');
             end; {Case str[1], if length >= 1}

         IF (str[1] in ['C','D','L','S','X','Y','P']) then
             if (length(str) >= 3) and (str[2]=' ') then
                  Begin
                      ts := '';
                      For i := 3 to length(str) do
                           ts := ts + str[i];
                  End else{if length}
                  if str <> 'CLS' then
                      Begin
                          LogEvent('Line too short for type or invalid.');
                          error := 1;
                      end;{if cls, if str[1]}

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
                                        Logevent('Line '+Int2str(line)+' contains an invalid color, '+ts+'.');
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
                                        Logevent('Line '+Int2str(line)+' contains an invalid color, '+ts+'.');
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
                            Logevent('Detail value invalid');
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
                               Logevent('Linemode must be set to "1","0" or "T".');
                           end;{case & if}
                    end; {'L'}
              'S' : Begin
                        val(ts,i,err);
                        If (err = 0) and (i > 0) then
                            settings.skaal := i
                        else
                        Begin
                            Logevent('Scale value invalid');
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
                                    Logevent('Y-axis position invalid');
                                    Error := 1;
                                End;
                            end else
                                settings.x := maxx div 2;
                    end; {'Y'}
              'X' : Begin
                        If (ts <> 'C') then
                            Begin
                                val(ts,i,err);
                                If (err = 0) and (i > 0) then
                                    settings.y := i
                                else
                                Begin
                                    Logevent('X-axis position invalid');
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
                                        PlotGraph(Settings, grafiek,0,ts);
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
     End; {While not(eof)}

cleanup:
   LogEvent('Plotting completed.');
   case error of
       0   : Logevent('No errors encountered');
       1   : Logevent('Non-fatal errors encountered');
       99  : Logevent('Fatal error encountered');
   End;

   LogEvent('--------------------------------------------------------');
   {$IFDEF EXCEPT}
   try
     Close(lr);
   except
     LogEvent('Error Closing file "'+filename+'" after plotting.');
   end;
   {$ELSE}
   {$I-}Close(lr);{$I+}
   error := IOREsult;
   {$ENDIF}
   with outs do
    Begin
       SetColor(oukleur);
       SettextStyle(font,direction,Charsize);
       SettextJustify(horiz,vert);
    End;
    {$IFNDEF WIN32}
    If settings.view then readkey;
    {$ENDIF}
End;
{----------------------------------------------------------------------------}
Procedure Menu;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
Procedure SettingsWindow(Settings : SettingT);
Begin
    Window(63,10,79,30);
    TextBackground(Green);
    ClrScr;
    Window(64,11,79,29);
    TextColor(White);
    WriteLn('Settings:');
    Writeln;
    Writeln('Axes:');
    Writeln(' x=',Settings.Y);
    Writeln(' y=',Settings.X);
    Writeln(' Scale=',Settings.skaal);
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
    Write  ('  ',maxx+1,'X',maxy+1);
End;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
var
    ch       : char;
    Settings : SettingT;
{    i        : shortint;}
    form     : shortstring;
    grafiek  : grapht;
    li       : longint;
    errdet   : record
                   ce   : smallint;
                   s2g  : smallint;
               end
                   absolute li;

Begin
    LoadSettings(configfilename,settings);
    LogEvent('DosPlot '+version+' started');
    LogEvent(BuildString);
    Logevent('Settings:');
    LogEvent('Driver: '+Int2str(settings.gd)+' '+Getdrivername);
    LogEvent('Mode: '+Int2str(settings.gm)+' ' + Getmodename(settings.gm));
    LogEvent('   '+Int2str(maxx)+'x'+Int2str(maxy));
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
        Window(20,16,80-20,50-18);
        TextBackground(Red);
        ClrScr;
        SettingsWindow(Settings);
        TextBackground(Red);
        Window(23,18,80-23,51-20);
        TextColor(Yellow);
        Writeln('DosPlot Version 1.1'{+version});
        Writeln('--------------------');
        Writeln;
        Writeln('0.  Show screen with graphs');
        Writeln('1.  Plot Graph');
        Writeln('2.  Plot Graph(s) from script');
        Writeln('3.  Settings');
        Writeln('8.  Plot Axes & clear screen');
        {$IFNDEF FPC}
            writeln('9.  Credits');
        {$ELSE}
            writeln('9.  About');
        {$ENDIF}
        Writeln;
        Writeln('Esc Exit');
        Repeat
            ch := Upcase(ReadKey);
        Until ch in ['0'..'3',#27,'8'..'9'];
        Case (ch) of
            '0' :
                Begin
                    GraphMode('screen');
                    Readkey;
                End;
            '8' :
                Begin
                    GraphMode('screen');
                    Asse(Settings);
                End;
            '1' :
           Begin
              Window(1,1,80,50);
              TextColor(7);
              TextBackground(0);
              ClrScr;
              FuncInfo;
              WriteLn('Enter function: (e.g "y=x*sin(1/x)")');
              Write  (' y=');
              Readln(form);
              li := str2graph(form,grafiek);
              If errdet.ce = 0 then
               Begin
                  If errdet.s2g <> 0 then
                      Begin
                          Writeln;
                          Writeln('Warning: Invalid domain');
                          Writeln('Error no. '+Int2str(errdet.s2g)+' '+pcalcerrormsg(errdet.s2g));
                          writeln;
                          Writeln('Press any key to continue...');
                          readkey;
                      end;
                  {$IFDEF EXCEPT}
                  try
                      pcalcexp.calcerror := 0;
                      LogEvent('Percalculating '+form);
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

                  If pcalcexp.calcresult = 0 then
                      Begin
                          LogEvent('Percalculation of '+form+' suceeded.');
                          GraphMode('screen');
                          PlotGraph(Settings, grafiek,0,form);
                          {$IFNDEF WIN32}
                          If settings.view then readkey;
                          {$ENDIF}
                      End;
                  {$ELSE}
                  LogEvent('Percalculating '+form);
                  PrecalcPostfix(grafiek.postfix);{}
                  If pcalcexp.calcerror = 0 then
                      Begin
                          LogEvent('Percalculation of '+form+' suceeded.');
                          GraphMode('screen');
                          PlotGraph(Settings, grafiek,0,form);
                          {$IFNDEF WIN32}
                          If settings.view then readkey;
                          {$ENDIF}
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
            '2' :
                Begin
                    Window(1,1,80,50);
                    TextColor(7);
                    TextBackground(0);
                    ClrScr;
                    WriteLn('Please enter script''s filename:');
                    Write  ('>');
                    Readln(form);
                    Graphmode('Screen');
                    ScriptPlot(settings,form);
                End;
            '3' :
           Begin
              Repeat
                 ch := #0;
                 TextBackground(Black);
                 TextMode(co80+font8x8);
                 Window(0,0,80,50);
                 TextBackground(Blue);
                 ClrScr;
                 Window(10,10,80-20,50-10);
                 TextBackground(Red);
                 ClrScr;
                 Window(13,12,80-20,50-12);
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
                 Textcolor(lightRed+blink); Writeln('Warning:');
                 Textcolor(15); Writeln('Options 0-5 clear the graphing screen');
                 SettingsWindow(Settings);
                 Window(1,1,80,50);
                 ch := Readkey;
                 Textcolor(7);
                 TextBackground(0);
                 ClrScr;
                 Case ch of
                    '0' :
                         Begin
                            DefSetts(settings);
                            Graphmode('screen');
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '1' :
                         Begin
                            Graphmode('screen');
                            Settings.X := MaxX div 2;
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '2' :
                         Begin
                            Graphmode('screen');
                            Settings.Y := MaxY div 2;
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '3' :
                         Begin
                            ClrScr;
                            WriteLn('New position of Y-axis (in pixels)? (0-639 in VGA mode(default))');
                            Write('>');
                            Readln(Settings.X);
                            Graphmode('screen');
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '4' :
                         Begin
                            WriteLn('New position of X-axis (in pixels)? (0-479 in VGA mode(default))');
                            Write('>');
                            Readln(Settings.Y);
                            Graphmode('screen');
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '5' :
                         Begin
                            WriteLn('New scale? (1-65535 pixels per unit)');
                            Write('>');
                            Readln(Settings.skaal);
                            Graphmode('screen');
                            Asse(Settings);
                            Txtmode('screen');
                         End;
                    '6' :
                         Begin
                           Kleurkodes;
                           Window(1,1,80,50);
                           Textcolor(7);
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
    LogEvent(Buildstring);
    Logevent('Settings:');
    LogEvent('Driver: '+Int2str(settings.gd)+' '+Getdrivername);
    LogEvent('Mode: '+Int2str(settings.gm)+' ' + Getmodename(settings.gm));
    LogEvent('   '+Int2str(maxx)+'x'+Int2str(maxy));
    LogEvent('   Colours: '+Int2str(getmaxcolor));
    LogEvent('Asse: X='+Int2str(settings.y)+' Y='+Int2Str(settings.X));
    LogEvent('Scale: '+Int2Str(Settings.skaal));
    If settings.linemode then
        LogEvent('Linemode: ON  Detail='+Int2Str(settings.detail))
    Else
        LogEvent('Linemode: OFF Detail='+Int2Str(settings.detail));
    LogEvent('DosPlot '+version+' terminated normally');
    LogEvent('-----------------------------------------------------------');
End;
{----------------------------------------------------------------------------}
var oldmode :integer;
Begin
   {$IFNDEF EXCEPT}
   pcalcexception := false;
   {$ENDIF}
   oldmode := LastMode;
   LogEvent('-----------------------------------------------------------');
   DeleteScrFiles('screen');
   Directvideo := false;
    If (ParamStr(1) = '/?') THen
     Begin
       CLrScr;
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

