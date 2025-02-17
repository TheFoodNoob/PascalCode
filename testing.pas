program InfiniteIntegerPrimeTest;

uses sysutils;

type
  PNode = ^Node;
  Node = record
    data: integer;   // Store a single digit (0-9)
    next, prev: PNode;
  end;

var
   head1, tail1, head2, tail2, headSum, tailSum: PNode;  

// Append a digit to the linked list (used for number storage)
procedure AppendDigit(value: integer; var head, tail: PNode);
var
  newNode: PNode;
begin
  new(newNode);
  newNode^.data := value;
  newNode^.next := nil;
  newNode^.prev := tail;
  
  if tail <> nil then
    tail^.next := newNode
  else
    head := newNode;
    
  tail := newNode;
end;

// Prepend a digit (used to store sum result)
procedure PrependDigit(value: integer; var head, tail: PNode);
var
  newNode: PNode;
begin
  new(newNode);
  newNode^.data := value;
  newNode^.prev := nil;
  newNode^.next := head;
  
  if head <> nil then
    head^.prev := newNode
  else
    tail := newNode;
  
  head := newNode;
end;

// Adding two large numbers using linked lists
procedure AddNumbers(tail1, tail2: PNode; var headSum, tailSum: PNode);
var
  carry, digit1, digit2, digitSum: integer;
  current1, current2: PNode;
begin
  carry := 0;
  current1 := tail1;
  current2 := tail2;
  headSum := nil;
  tailSum := nil;

  while (current1 <> nil) or (current2 <> nil) or (carry <> 0) do
  begin
    digit1 := 0; 
    digit2 := 0;

    if current1 <> nil then
    begin
      digit1 := current1^.data;
      current1 := current1^.prev; 
    end;

    if current2 <> nil then
    begin
      digit2 := current2^.data;
      current2 := current2^.prev; 
    end;

    // Compute sum
    digitSum := digit1 + digit2 + carry;
    carry := digitSum div 10;
    digitSum := digitSum mod 10;

    // Add this digit to the front of the sum list
    PrependDigit(digitSum, headSum, tailSum);
  end;
end;

// Convert linked list to string representation
function GetNumberAsString(num: PNode): AnsiString;
var
  current: PNode;
  result: string;
begin
  result := '';
  current := num;
  
  while current <> nil do
  begin
    result := result + IntToStr(current^.data);
    current := current^.next;
  end;

  GetNumberAsString := result;
end;

// Compute Modulo for large number stored in linked list
function ModuloLinkedList(const num: PNode; divisor: Integer): Integer;
var
  current: PNode;
  result: Integer;
begin
  result := 0;
  current := num;
  
  while current <> nil do
  begin
    result := (result * 10 + current^.data) mod divisor;
    current := current^.next;
  end;

  ModuloLinkedList := result;
end;

// Fast Prime Checking
function IsPrimeLinkedList(const num: PNode): Boolean;
var
  i: Integer;
begin
  // Handle special cases
  if ModuloLinkedList(num, 2) = 0 then Exit(False);
  if ModuloLinkedList(num, 3) = 0 then Exit(False);

  // Check divisibility up to a reasonable limit
  for i := 5 to 1000 do
  begin
    if ModuloLinkedList(num, i) = 0 then Exit(False);
  end;

  IsPrimeLinkedList := True;
end;

// Slow Prime Checking (Checking Perfect Squares)
function IsPrimeSlowLinkedList(const num: PNode): Boolean;
var
  i, numValue, square: Integer;
begin
  numValue := ModuloLinkedList(num, 10);  // Convert to integer (modulo operation to avoid overflow)

  // Handle small number cases
  if numValue < 2 then
    Exit(False);

  // Check divisibility by perfect squares (1, 4, 9, 16, 25, etc.)
  i := 1;
  while (i * i) <= numValue do
  begin
    square := i * i;
    
    // If numValue is divisible by the perfect square, it's not prime
    if numValue mod square = 0 then
      Exit(False);
      
    i := i + 1;
  end;

  IsPrimeSlowLinkedList := True;
end;

// Generate a random large number (1-30 digits)
procedure GenerateRandomNumber(var head, tail: PNode);
var
  length, i, digit: integer;
begin
  length := Random(30) + 1;
  for i := 1 to length do
  begin
    digit := Random(10);
    AppendDigit(digit, head, tail);
  end;
end;

// Save results to a file
procedure SaveToFile(filename: string; output: AnsiString);
var
    outputFile: Text;
begin
    Assign(outputFile, filename);
    Rewrite(outputFile);

    if IOResult <> 0 then 
    begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;

    writeln(outputFile, output);
    Close(outputFile);

    Writeln('Output saved to file: ', filename);
end;

// Main program execution
var
  output: AnsiString;
begin
  Randomize;
  head1 := nil; tail1 := nil;
  head2 := nil; tail2 := nil;
  headSum := nil; tailSum := nil;

  writeln('Generating random number 1...');
  GenerateRandomNumber(head1, tail1);

  writeln('Generating random number 2...');
  GenerateRandomNumber(head2, tail2);

  // Output the numbers
  output := 'Number 1: ' + GetNumberAsString(head1) + sLineBreak;
  output := output + 'Number 2: ' + GetNumberAsString(head2) + sLineBreak;

  // Perform addition
  AddNumbers(tail1, tail2, headSum, tailSum);

  // Output the sum
  output := output + 'Sum: ' + GetNumberAsString(headSum) + sLineBreak;

  // Fast Prime Check
  if IsPrimeLinkedList(headSum) then
    output := output + 'Sum is prime (Fast Check).' + sLineBreak
  else
    output := output + 'Sum is not prime (Fast Check).' + sLineBreak;

  // Slow Prime Check
  if IsPrimeSlowLinkedList(headSum) then
    output := output + 'Sum is prime (Slow Check).' + sLineBreak
  else
    output := output + 'Sum is not prime (Slow Check).' + sLineBreak;

  // Display and save output
  writeln(output);
  SaveToFile('infinteger.txt', output);
end.
