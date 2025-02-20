program RecursiveBinarySearchExample;

uses
  SysUtils;

const
  MAX_SIZE = 10;  
  OUTPUT_FILE = 'arrsearch.txt';

type
  TArray = array[1..MAX_SIZE] of Integer;
  TRange = array[1..2] of Integer;  // Stores [first index, last index]

// Recursive function to find the first occurrence of the key
function FindFirst(A: TArray; low, high, key: Integer): Integer;
var 
mid: Integer;
begin
  if low > high then
    Exit(-1);  // Key not found

  // Middle index
  mid := (low + high) div 2;

  if (A[mid] = key) and ((mid = 1) or (A[mid - 1] <> key)) then
    Exit(mid)  // First occurrence found
  else if A[mid] < key then
    Exit(FindFirst(A, mid + 1, high, key))  // Search right
  else
    Exit(FindFirst(A, low, mid - 1, key));  // Search left
end;

// Recursive function to find the last occurrence of the key
function FindLast(A: TArray; low, high, key: Integer): Integer;
var 
mid: Integer;
begin
  if low > high then
    Exit(-1);

  mid := (low + high) div 2;

  if (A[mid] = key) and ((mid = MAX_SIZE) or (A[mid + 1] <> key)) then
    Exit(mid)  // Last occurrence found
  else if A[mid] > key then
    Exit(FindLast(A, low, mid - 1, key))  // Search left
  else
    Exit(FindLast(A, mid + 1, high, key));  // Search right
end;

// Wrapper function to find both first and last occurrences
function RecursiveBinarySearch(A: TArray; n, key: Integer): TRange;
begin
  RecursiveBinarySearch[1] := FindFirst(A, 1, n, key);
  RecursiveBinarySearch[2] := FindLast(A, 1, n, key);
end;

// Generate a random sorted array
procedure GenerateSortedArray(var A: TArray; n: Integer);
var
  i, temp: Integer;
  swapped: Boolean;
  
begin
  for i := 1 to n do
    A[i] := Random(10);  // Numbers between 0-9 for duplicates
  
  // Sort array (using simple Bubble Sort for now)
   
  repeat
    swapped := False;
    for i := 1 to n - 1 do
      if A[i] > A[i + 1] then
      begin
        temp := A[i];
        A[i] := A[i + 1];
        A[i + 1] := temp;
        swapped := True;
      end;
  until not swapped;
end;

// Write results to file
procedure WriteResultsToFile(A: TArray; n, key: Integer; range: TRange);
var
  f: Text;
  i: Integer;
begin
  Assign(f, OUTPUT_FILE);
  Rewrite(f);
  
  Write(f, 'Sorted Array: ');
  for i := 1 to n do
    Write(f, A[i], ' ');
  Writeln(f);
  
  Writeln(f, 'Searching for key: ', key);
  if range[1] <> -1 then
    Writeln(f, 'Key found at indices: [', range[1], ', ', range[2], ']')
  else
    Writeln(f, 'Key not found.');

  Close(f);
end;

var
  A: TArray;
  key: Integer;
  range: TRange;
  i: Integer;

begin
  Randomize;
  GenerateSortedArray(A, MAX_SIZE);  // Generate and sort the array

  key := Random(10);  // Pick a random key to search for
  Writeln('Searching for key: ', key);

  range := RecursiveBinarySearch(A, MAX_SIZE, key);

  // Write results to file
  WriteResultsToFile(A, MAX_SIZE, key, range);

  // Display results on console
  Writeln('Sorted Array: ');
  for i := 1 to MAX_SIZE do
    Write(A[i], ' ');
  Writeln;

  if range[1] <> -1 then
    Writeln('Key found at indices: [', range[1], ', ', range[2], ']')
  else
    Writeln('Key not found.');

  Writeln('Results saved to ', OUTPUT_FILE);
end.
