{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

{$IFNDEF FPC}
{$IF CompilerVersion >= 20} // Delphi 2009+
{$DEFINE Delphi2009Plus}
{$IFEND}
{$IF CompilerVersion >= 21} // Delphi XE+ — TThread.Start disponível
{$DEFINE DelphiXEPlus}
{$IFEND}
{$ENDIF}

{$IF DEFINED(MSWINDOWS) OR DEFINED(WINDOWS)}
  {$DEFINE BADGER_WINDOWS}
{$IFEND}

{$IF DEFINED(LINUX) OR DEFINED(POSIX)}
  {$DEFINE BADGER_LINUX}
{$IFEND}
