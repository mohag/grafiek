Program scriptgen;

const
      Funccount = 36;
      Functions : Array[1..funccount] of String[10]
                = ('ASIN','ARCSIN','ACOS','ARCCOS','ATAN','ARCTAN','SIN',
                   'COS','TAN','COT','SEC','COSEC','NEG','LN','LOG',
                   'EXP','TRUNC','ABS','ROUND','SQR','DEG','SQRT','SINH',
                   'COSH','TANH','ARCSINH','ARCTANH','ARCCOSH','ASINH',
                   'ATANH','ACOSH','SECH','COSECH','CSCH','COTH','CSC');{}
      maxcolor  = 255;{Set to 16 under turbo pascal or gm = 9}

var
    lr  :   Text;
    i   :   integer;

Begin
    Assign(lr,'testscr.srp');
    Rewrite(lr);
    Writeln(lr,'GRAPHSCRIPT');
    Writeln(lr,'CLS');
    For i := 1 to funccount do
        Begin
            Writeln(lr,'C ',(i mod maxcolor+1));
            Writeln(lr,'P ',functions[i],'(X)');
        End;
    Close(lr);
End.
