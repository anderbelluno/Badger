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
  procedure RequireType(const AName: string; const AValue: ISuperObject; AType: TSuperType);
  begin
    if (AValue = nil) or (not ObjectIsType(AValue, AType)) then
      raise Exception.CreateFmt('Invalid claim type for "%s"', [AName]);
  end;
var
  VUserID, VRole, VIss, VExp: ISuperObject;
begin
  if (AJSON = nil) or (not ObjectIsType(AJSON, stObject)) then
    raise Exception.Create('Invalid claims payload');

  VUserID := AJSON.O['user_id'];
  VRole := AJSON.O['role'];
  VIss := AJSON.O['iss'];
  VExp := AJSON.O['exp'];

  RequireType('user_id', VUserID, stString);
  RequireType('role', VRole, stString);
  RequireType('iss', VIss, stInt);
  RequireType('exp', VExp, stInt);

  Result := TBadgerJWTClaims.Create;
  Result.UserID := VUserID.AsString;
  Result.Role := VRole.AsString;
  Result.Iss := VIss.AsInteger;
  Result.Exp := VExp.AsInteger;
end;

end.

