program ArrSearch;
uses SysUtils;

const
  SIZE = 160;  // Size of the array
  NUMOFKEYS = 10;  // Number of keys to search

type
  IntArray = array of Integer;

var
  m: Integer;  // Global operation counter

{ Recursive Linear Search (Counts Comparisons) }
procedure recLineSearch(arr: IntArray; key, index: Integer; var result: array of Integer);
begin
    if index > Length(arr) then 
        Exit;
    
    m := m + 1;  // Increment comparison count

    if arr[index] = key then
    begin
        if result[0] = -1 then
            result[0] := index
        else  
            result[1] := index;
    end;
    
    recLineSearch(arr, key, index+1, result);
end;

{ Recursive Binary Search (Counts Comparisons) }
procedure recBinSearch(arr: IntArray; key, left, right: Integer; var result: array of Integer);
var 
    mid: Integer;
begin
    if left > right then Exit;
    
    mid := (left + right) div 2;
    m := m + 1;  // Increment comparison count

    if arr[mid] = key then
    begin
        if result[0] = -1 then result[0] := mid;
        result[1] := mid;

        recBinSearch(arr, key, left, mid-1, result);
        recBinSearch(arr, key, mid+1, right, result);
    end
    else if arr[mid] < key then
        recBinSearch(arr, key, mid+1, right, result)
    else 
        recBinSearch(arr, key, left, mid-1, result);
end;

{ Bubble Sort (Counts Comparisons & Swaps) }
procedure BubbleSort(var myarray: IntArray);
var
  i, j, tmp, n: Integer;
begin
  n := Length(myarray);
  for i := 0 to n - 2 do
  begin
    for j := 0 to n - i - 2 do
    begin
      m := m + 1;  // Increment comparison count
      if myarray[j] > myarray[j + 1] then
      begin
        m := m + 1;  // Increment swap count
        tmp := myarray[j];
        myarray[j] := myarray[j + 1];
        myarray[j + 1] := tmp;
      end;
    end;
  end;
end;

{ Generate an array with random values }
procedure genArray(var myarray: IntArray; size, maxValue: Integer);
var
  i: Integer;
begin
  Randomize;
  SetLength(myarray, size);
  for i := 0 to size - 1 do
    myarray[i] := Random(maxValue);
end;

{ Generate random keys to search for }
procedure genKeys(var keys: IntArray; count, maxValue: Integer);
var
  i: Integer;
begin
  Randomize;
  SetLength(keys, count);
  for i := 0 to count - 1 do
    keys[i] := Random(maxValue);
end;

{ Print array to console if size <= 256 }
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

{ Write operation count and search results to file }
procedure writeToFile(filename: string; keys, myarray: IntArray);
var
  myTextFile: TextFile;
  i: Integer;
  result: array[0..1] of Integer;
begin
  Assign(myTextFile, filename);
  Rewrite(myTextFile);

  { Print measured operations count (m) }
  WriteLn(myTextFile, 'Total Search Operations: ', m);
  WriteLn(myTextFile, '==========================');

  { Search Results }
  WriteLn(myTextFile, 'Search Results:');
  WriteLn(myTextFile, '---------------');

  for i := 0 to Length(keys) - 1 do
  begin
    result[0] := -1;  // Initialize first index
    result[1] := -1;  // Initialize last index

    recLineSearch(myarray, keys[i], 0, result);
    recBinSearch(myarray, keys[i], 0, Length(myarray) - 1, result);

    { Print search results for each key in a unique format }
    if result[0] = -1 then
      WriteLn(myTextFile, 'Key ', keys[i], ': Not found')
    else
      WriteLn(myTextFile, 'Key ', keys[i], ': Found in range [', result[0], ', ', result[1], ']');
  end;

  { Time Complexity Table (Optional Analysis) }
  WriteLn(myTextFile, '==========================');
  WriteLn(myTextFile, 'Time Complexity Analysis:');
  WriteLn(myTextFile, '-------------------------');
  WriteLn(myTextFile, 'n: ', SIZE, ' Operations: ', m, ' T(n): O(n^2)  O(g(n)): O(n^2)');

  Close(myTextFile);
end;

{ Main Program Execution }
var
  myarray, keys: IntArray;
begin
  m := 0;  // Reset operation counter

  genKeys(keys, NUMOFKEYS, 500);  // Generate 10 random keys
  
  genArray(myarray, SIZE, 500);  // Generate random array of size 160
  BubbleSort(myarray);  // Sort array and count operations
  
  WriteLn('Array Sorted:');
  printArrayToConsole(myarray);

  { Write operation count and search results to file }
  writeToFile('arrsearch.txt', keys, myarray);
end.
