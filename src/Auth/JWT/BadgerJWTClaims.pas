unit BadgerJWTClaims;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, SuperObject;

type
  TBadgerJWTClaims = class
  private
    FUserID: string;
    FRole: string;
    FIss: Int64;
    FExp: Int64;
  public
    property UserID: string read FUserID write FUserID;
    property Role: string read FRole write FRole;
    property Iss: Int64 read FIss write FIss;
    property Exp: Int64 read FExp write FExp;

    function ToJSON: ISuperObject;
    class function FromJSON(const AJSON: ISuperObject): TBadgerJWTClaims;
  end;

implementation

{ TBadgerJWTClaims }

function TBadgerJWTClaims.ToJSON: ISuperObject;
begin
  Result := SO();
  Result.S['user_id'] := FUserID;
  Result.S['role'] := FRole;
  Result.I['iss'] := FIss;
  Result.I['exp'] := FExp;
end;

class function TBadgerJWTClaims.FromJSON(const AJSON: ISuperObject): TBadgerJWTClaims;
begin
  Result := TBadgerJWTClaims.Create;
  Result.UserID := AJSON.S['user_id'];
  Result.Role := AJSON.S['role'];
  Result.Iss := AJSON.I['iss'];
  Result.Exp := AJSON.I['exp'];
end;

end.

