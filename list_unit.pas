unit list_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TList }

  generic TList<T> = class
  private
    Items: array of T;
  public
    procedure Add(Value: T);
    procedure insertItem(Value: T; index: integer);
    procedure replaceItem(index: integer; Value: T);
    procedure deleteItemAt(index: integer);
    procedure deleteItem(keyvalue: T);
    procedure Clear;
    function getItem(Index: integer): T;
    function isEmpty: boolean;
    function Count: integer;
    function indexOf(Value: T): integer;

  end;
  //Various Lists
  TListOfString = specialize TList<String>;
  TListOfInt = specialize TList<Integer>;
  TListOfBool = specialize TList<Boolean>;
  TListofDouble = specialize TList<Double>;
  //add more as you need
  //{Name of List} = specialize TList<{Desired Datatype}>;

  MyException = class(Exception)

  end;

implementation

{ TList }

procedure TList.Add(Value: T);
begin
  SetLength(items, Length(items) + 1);
  items[Length(items) - 1] := Value;
end;

procedure TList.insertItem(Value: T; index: integer);
var
  i: integer;
  tValue: T;
begin
  if (index = 0) and (Length(items) = 0) then
  begin
    add(Value);
  end
  else if (index < count) and (index >= 0) then
  begin
    SetLength(items, Count + 1);
    for i := Count - 1 downto index + 1 do
    begin
      items[i] := items[i - 1];
    end;
    items[index] := Value;
  end
  else
  begin
    raise MyException.Create('Out of bounds: Length: ' + Count.toString +
      ' Index: ' + index.toString);
  end;
end;

procedure TList.replaceItem(index: integer; Value: T);
begin
  if index >= Count then
  begin
    raise MyException.Create('Out of bounds: Length: ' +
      Length(items).toString + ' Index: ' + Index.toString);
  end
  else
  begin
    items[index] := Value;
  end;
end;

procedure TList.deleteItemAt(index: integer);
var
  tempValue: T;
  i: integer;

begin
  if index >= count then
  begin
    raise MyException.Create('Out of bounds: Length: ' +
      Length(items).toString + ' Index: ' + Index.toString);
  end
  else
  begin
    for i := index to Count - 2 do
    begin
      tempValue := items[i + 1];
      items[i + 1] := items[i];
      items[i] := tempValue;
    end;
    SetLength(items, Count - 1);

  end;

end;

procedure TList.deleteItem(keyvalue: T);
var
  index: integer;
begin
  index := 0;
  if Length(items) <> 0 then
  begin
    while (index < Length(items)) and (items[index] <> Keyvalue) do
    begin
      Inc(index);
    end;
    if index >= Count then
    begin
      raise MyException.Create('Keyword not in list');
    end
    else
    begin
      deleteItemAt(index);
    end;

  end
  else
  begin
    raise MyException.Create('List is empty');
  end;
end;

procedure TList.Clear;
begin
  SetLength(items, 0);
end;

function TList.getItem(Index: integer): T;
begin
  Result := items[Index];
end;

function TList.isEmpty: boolean;
begin
  if Count = 0 then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TList.Count: integer;
var
  i: integer;
begin
  Result := Length(items);
end;

function TList.indexOf(Value: T): integer;
var
  index: integer;
begin
  index := 0;
  if Length(items) <> 0 then
  begin
    while (index < Length(items)) and (items[index] <> Value) do
    begin
      Inc(index);
    end;
    if index >= Count then
    begin
      raise MyException.Create('Item not in List');
    end
    else
    begin
      Result := index;
    end;
  end
  else
  begin
    raise MyException.Create('List is empty');
  end;
end;
end.

