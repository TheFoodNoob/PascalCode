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
   fastPrimeOperations, slowPrimeOperations: Int64;  // Counters for operations
   digitCount: Integer;  // Number of digits for the current test

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

// // Convert linked list to string representation
// function GetNumberAsString(num: PNode): AnsiString;
// var
//   current: PNode;
//   result: string;
// begin
//   result := '';
//   current := num;
  
//   while current <> nil do
//   begin
//     result := result + IntToStr(current^.data);
//     current := current^.next;
//   end;

//   GetNumberAsString := result;
// end;

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
    slowPrimeOperations := slowPrimeOperations + 1;
    result := (result * 10 + current^.data) mod divisor;
    current := current^.next;
  end;

  ModuloLinkedList := result;
end;

// **SLOW Prime Checking (Full Trial Division)**
function IsPrimeSlowLinkedList(const num: PNode): Boolean;
var
  i: Integer;
begin
  writeln('Checking divisibility for slow prime check...');
  
  slowPrimeOperations := slowPrimeOperations + 1;  // Divisibility check for 2
  writeln('Checking divisibility by 2...');
  if ModuloLinkedList(num, 2) = 0 then
    Exit(False);
  
  slowPrimeOperations := slowPrimeOperations + 1;  // Divisibility check for 3
  writeln('Checking divisibility by 3...');
  if ModuloLinkedList(num, 3) = 0 then
    Exit(False);

  // Check divisibility up to 10,000 (higher range than fast check)
  i := 5;
  while i <= 10000 do
  begin
    slowPrimeOperations := slowPrimeOperations + 1;  // Each divisibility check
    if ModuloLinkedList(num, i) = 0 then
      Exit(False);
    i := i + 2; // Skip even numbers
  end;

  Exit(True);
end;

// Rename the ModuloLinkedList function to avoid name conflict
function ModuloLinkedListLarge(const num: PNode; divisor: Int64): Integer;
var
  current: PNode;
  result: Integer;
begin
  result := 0;
  current := num;
  
  while current <> nil do
  begin
    fastPrimeOperations := fastPrimeOperations + 1;
    result := (result * 10 + current^.data) mod divisor;
    current := current^.next;
  end;

  ModuloLinkedListLarge := result;
end;

// Change the divisor to a larger number and handle larger ranges
function IsPrimeFastLinkedList(const num: PNode): Boolean;
var
  i, numValue, square: Integer;
begin
  numValue := ModuloLinkedListLarge(num, 100000);  // Use larger divisor to get the full value
  
  // Handle small number cases
  fastPrimeOperations := fastPrimeOperations + 1;
  if numValue < 2 then
    Exit(False);

  writeln('Checking divisibility for fast prime check...');
  
  // Check divisibility by perfect squares up to sqrt(n)
  i := 1;
  while (i * i) <= numValue do
  begin
    fastPrimeOperations := fastPrimeOperations + 1;
    square := i * i;
    
    writeln('Checking square ', square, '...');  // Debug print statement for each square check
    
    // If numValue is divisible by the perfect square, it's not prime
    fastPrimeOperations := fastPrimeOperations + 1;
    if numValue mod square = 0 then
      Exit(False);
    
    i := i + 1;
  end;

  IsPrimeFastLinkedList := True;
end;

// Generate a fixed size large number (e.g., 20 digits)
procedure GenerateFixedSizeNumber(var head, tail: PNode; size: Integer);
var
  i, digit: integer;
begin
  for i := 1 to size do
  begin
    digit := Random(10);  // Generate a random digit between 0 and 9
    AppendDigit(digit, head, tail);
  end;
end;

// Save results to a file (overwrite)
procedure SaveNewRunToFile(filename: string; output: AnsiString);
var
    outputFile: Text;
begin
    Assign(outputFile, filename);
    Rewrite(outputFile);  // Overwrite the file for the new run
    if IOResult <> 0 then 
    begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;

    writeln(outputFile, output);
    Close(outputFile);

    Writeln('Starting new run. Output saved to file: ', filename);
end;

// Save iteration results to a file (append)
procedure AppendIterationToFile(filename: string; output: AnsiString);
var
    outputFile: Text;
begin
    Assign(outputFile, filename);
    Append(outputFile);  // Append the output to the existing file
    if IOResult <> 0 then 
    begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;

    writeln(outputFile, output);
    Close(outputFile);

    Writeln('Iteration result appended to file: ', filename);
end;

// Procedure to free the linked list
procedure FreeLinkedList(var head: PNode);
var
  current, temp: PNode;
begin
  current := head;
  
  // Traverse the list and free each node
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);  // Deallocate memory for the node
  end;
  
  head := nil;  // Ensure the head pointer is nil after freeing the list
end;

// Recursive function to print the linked list to the file
procedure PrintLinkedListToFile(const num: PNode; var outputFile: Text);
begin
  if num <> nil then
  begin
    // Write the current digit to the file
    Write(outputFile, IntToStr(num^.data));
    
    // Recursively call for the next node
    PrintLinkedListToFile(num^.next, outputFile);
  end;
end;


// Main program execution
var 
  outputFile: Text;
begin
  Randomize;

  // Start new run and overwrite the file with an initial message
  SaveNewRunToFile('infinteger.txt', 'Starting a test with a fixed number of digits.' + sLineBreak);
  
  // Only process one set of numbers
  digitCount := 10;  // You can change this value to test with different sizes
  
  head1 := nil; tail1 := nil;
  head2 := nil; tail2 := nil;
  headSum := nil; tailSum := nil;
  fastPrimeOperations := 0;  // Initialize counters
  slowPrimeOperations := 0;  // Initialize counters

  // Generate numbers with digitCount digits
  writeln('Generating fixed-size number with ', digitCount, ' digits...');
  GenerateFixedSizeNumber(head1, tail1, digitCount);
  GenerateFixedSizeNumber(head2, tail2, digitCount);

  // Open the file to write the output
  Assign(outputFile, 'infinteger.txt');
  Append(outputFile);  // Append the output to the existing file
  
  // Output the numbers by printing them to the file
  writeln(outputFile, 'Number 1 (Digits: ', IntToStr(digitCount), '): ');
  PrintLinkedListToFile(head1, outputFile);
  writeln(outputFile);  // Print a newline
  
  writeln(outputFile, 'Number 2 (Digits: ', IntToStr(digitCount), '): ');
  PrintLinkedListToFile(head2, outputFile);
  writeln(outputFile);  // Print a newline

  // Perform addition
  AddNumbers(tail1, tail2, headSum, tailSum);

  // Output the sum
  writeln(outputFile, 'Sum: ');
  PrintLinkedListToFile(headSum, outputFile);
  writeln(outputFile);  // Print a newline

  // Fast Prime Check (Square Root Method)
  if IsPrimeFastLinkedList(headSum) then
    writeln(outputFile, 'Sum is prime (Fast Check).')
  else
    writeln(outputFile, 'Sum is not prime (Fast Check).');

  // Slow Prime Check (Divisibility Test)
  if IsPrimeSlowLinkedList(headSum) then
    writeln(outputFile, 'Sum is prime (Slow Check).')
  else
    writeln(outputFile, 'Sum is not prime (Slow Check).');

  // Output operation counts
  writeln(outputFile, 'Fast Prime Check Operations: ', IntToStr(fastPrimeOperations));
  writeln(outputFile, 'Slow Prime Check Operations: ', IntToStr(slowPrimeOperations));

  // Close the file after finishing
  Close(outputFile);

  writeln('Test completed and results saved.');

  // Free memory for linked lists
  FreeLinkedList(head1);
  FreeLinkedList(head2);
  FreeLinkedList(headSum);
end.


