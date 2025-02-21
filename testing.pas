program ArrSearch;
uses SysUtils;

const
  SIZE = 160;
  NUMOFKEYS = 10;

type
  IntArray = array of Integer;

var
  m: Integer;  

{ Recursive Linear Search }
procedure recLineSearch(arr: IntArray; key, index: Integer; var result: array of Integer);
begin
    if index >= Length(arr) then 
        Exit;
    
    m := m + 1;

    if arr[index] = key then
    begin
        if result[0] = -1 then
            result[0] := index
        else  
            result[1] := index;
    end;
    
    recLineSearch(arr, key, index+1, result);
end;

{ Recursive Binary Search }
procedure recBinSearch(arr: IntArray; key, left, right: Integer; var result: array of Integer);
var 
    mid: Integer;
begin
    if left > right then Exit;
    
    mid := (left + right) div 2;
    m := m + 1;

    if arr[mid] = key then
    begin
        if (result[0] = -1) or (mid < result[0]) then result[0] := mid;
        if(result[1]=-1) or (mid < result[0]) then result[1] := mid;
        
        recBinSearch(arr, key, left, mid-1, result);
        recBinSearch(arr, key, mid+1, right, result);
    end
    else if arr[mid] < key then
        recBinSearch(arr, key, mid+1, right, result)
    else 
        recBinSearch(arr, key, left, mid-1, result);
end;

{ Selection Sort - Sorting using Linear Search principles }
procedure SelectionSort(var arr: IntArray);
var
  i, j, minIdx, temp, n: Integer;
begin
  n := Length(arr);
  for i := 0 to n - 2 do
  begin
    minIdx := i;
    for j := i + 1 to n - 1 do
    begin
      m := m + 1;
      if arr[j] < arr[minIdx] then
        minIdx := j;
    end;
    if minIdx <> i then
    begin
      temp := arr[i];
      arr[i] := arr[minIdx];
      arr[minIdx] := temp;
    end;
  end;
end;

{ Binary Insertion Sort - Sorting using Binary Search }
function BinarySearchPosition(arr: IntArray; item, left, right: Integer): Integer;
var
  mid: Integer;
begin
  while left <= right do
  begin
    mid := (left + right) div 2;
    m := m + 1;
    if arr[mid] < item then
      left := mid + 1
    else
      right := mid - 1;
  end;
  BinarySearchPosition := left;
end;

procedure BinaryInsertionSort(var arr: IntArray);
var
  i, j, selected, pos: Integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    selected := arr[i];
    pos := BinarySearchPosition(arr, selected, 0, i - 1);
    
    for j := i downto pos + 1 do
      arr[j] := arr[j - 1];
    
    arr[pos] := selected;
  end;
end;

{ Generate Random Array }
procedure genArray(var myarray: IntArray; size, maxValue: Integer);
var
  i: Integer;
begin
  Randomize;
  SetLength(myarray, size);
  for i := 0 to size - 1 do
    myarray[i] := Random(maxValue);
end;

{ Generate Random Keys }
procedure genKeys(var keys: IntArray; count, maxValue: Integer);
var
  i: Integer;
begin
  Randomize;
  SetLength(keys, count);
  for i := 0 to count - 1 do
    keys[i] := Random(maxValue);
end;

{ Print Array to Console }
procedure printArrayToConsole(myarray: IntArray);
var
  i: Integer;
begin
  if Length(myarray) <= 256 then
  begin
    for i := 0 to Length(myarray) - 1 do
      Write(myarray[i], ' ');
    WriteLn;
  end;
end;

{ Write to File }
procedure writeToFile(filename: string; keys, myarray: IntArray);
var
  myTextFile: TextFile;
  i: Integer;
  result: array[0..1] of Integer;
begin
  Assign(myTextFile, filename);
  Rewrite(myTextFile);

  WriteLn(myTextFile, 'Total Search Operations: ', m);
  WriteLn(myTextFile, '==========================');
  WriteLn(myTextFile, 'Search Results:');
  WriteLn(myTextFile, '---------------');

  for i := 0 to Length(keys) - 1 do
  begin
    result[0] := -1;
    result[1] := -1;
    recLineSearch(myarray, keys[i], 0, result);
    recBinSearch(myarray, keys[i], 0, Length(myarray) - 1, result);
    if result[0] = -1 then
      WriteLn(myTextFile, 'Key ', keys[i], ': Not found')
    else
      WriteLn(myTextFile, 'Key ', keys[i], ': Found in range [', result[0], ', ', result[1], ']');
  end;
  WriteLn(myTextFile, '==========================');
  WriteLn(myTextFile, 'Time Complexity Analysis:');
  WriteLn(myTextFile, '-------------------------');
  WriteLn(myTextFile, 'n: ', SIZE, ' Operations: ', m);
  Close(myTextFile);
end;

{ Main Program Execution }
var
  myarray, keys: IntArray;
begin
  m := 0;
  genKeys(keys, NUMOFKEYS, 500);
  genArray(myarray, SIZE, 500);
  SelectionSort(myarray);
  WriteLn('Array Sorted using Selection Sort:');
  printArrayToConsole(myarray);
  writeToFile('arrsearch.txt', keys, myarray);
end.
