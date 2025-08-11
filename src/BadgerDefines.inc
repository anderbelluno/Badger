{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{$IFNDEF FPC}
{$IF CompilerVersion >= 20} // Delphi 2009+
{$DEFINE Delphi2009Plus}
{$IFEND}
{$ENDIF}