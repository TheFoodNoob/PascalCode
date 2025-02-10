program InfiniteIntegerPrimeTest;

uses sysutils;

type
  PNode = ^Node;
  Node = record
    data: integer;   // Store a single digit (0-9)
    next, prev: PNode;
  end;

var
   head1, tail1, head2, tail2, headSum, tailSum: PNode;  // Pointers for the numbers and sum

// Function to append a digit to the doubly linked list
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

// Function to prepend a digit to the front of the doubly linked list
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
    tail := newNode; // If list was empty, set tail too
  
  head := newNode;
end;


// Main logic for adding two "infinite" numbers
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

  // Traverse both lists from the tail to the head
  while (current1 <> nil) or (current2 <> nil) or (carry <> 0) do
  begin
    // Get the current digits (if list is shorter, treat as 0)
    if current1 <> nil then
    begin
      digit1 := current1^.data;
      current1 := current1^.prev; // Move to previous node
    end
    else
      digit1 := 0;

    if current2 <> nil then
    begin
      digit2 := current2^.data;
      current2 := current2^.prev; // Move to previous node
    end
    else
      digit2 := 0;

    // Compute sum and carry
    digitSum := digit1 + digit2 + carry;
    carry := digitSum div 10;  // Extract new carry
    digitSum := digitSum mod 10;  // Keep only last digit

    // Add this digit to the front of the sum list
    PrependDigit(digitSum, headSum, tailSum);

  end;
end;


// Function to print a large number from the linked list (skip leading zeros)
procedure PrintNumber(num: PNode);
var
  current: PNode;
  leadingZero: boolean;
begin
  current := num;
  leadingZero := true; // Flag to skip leading zeros

  while current <> nil do
  begin
    // Skip leading zeros
    if leadingZero and (current^.data = 0) then
    begin
      current := current^.next; // Move to the next node
      continue;  // Skip printing the zero
    end;

    leadingZero := false; // After the first non-zero digit, we print all digits
    write(current^.data); // Print the digit
    current := current^.next; // Move to the next node
  end;
  writeln; // Newline after the number
end;

// Function to compute Modulo of a large number represented by a linked list
function ModuloLinkedList(const num: PNode; divisor: Integer): Integer;
var
  current: PNode;
  result: Integer;
begin
  result := 0;
  current := num;
  
  // Process each digit in the linked list
  while current <> nil do
  begin
    result := (result * 10 + current^.data) mod divisor; // Incrementally build the number's mod value
    current := current^.next;
  end;

  ModuloLinkedList := result; // Return the final result modulo divisor
end;

// Function to check if a number represented by a linked list is prime
function IsPrimeLinkedList(const num: PNode): Boolean;
var
  i, modResult: Integer;
begin
  // Special cases: 0, 1, and even numbers are not prime
  if (ModuloLinkedList(num, 2) = 0) then
    Exit(False);

  // Check divisibility up to a reasonable limit (e.g., 1000)
  for i := 3 to 1000 do
  begin
    modResult := ModuloLinkedList(num, i);
    if modResult = 0 then
      Exit(False); // If divisible by any number, it's not prime
  end;

  // If no divisors found, the number is prime
  IsPrimeLinkedList := True;
end;

// Function to generate a random large number as a linked list
procedure GenerateRandomNumber(var head, tail: PNode);
var
  length, i, digit: integer;
begin
  // Generate a random length between 1 and 256
  length := Random(256) + 1;
  for i := 1 to length do
  begin
    digit := Random(10);  // Random digit between 0 and 9
    AppendDigit(digit, head, tail);
  end;
end;

procedure SaveToFile(filename: string; output: AnsiString);
var
    outputFile: Text;
begin
    Assign(outputFile, filename); // Link file variable to the actual file
    Rewrite(outputFile); // Open file for writing (this erases previous content)

    if IOResult <> 0 then 
    begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;

    // Write the gathered output string to the file
    writeln(outputFile, output);

    Close(outputFile); // Close the file

    if IOResult <> 0 then 
    begin
        Writeln('Error closing file: ', filename);
    end;

    Writeln('Output saved to file: ', filename);
end;

function GetNumberAsString(num: PNode): AnsiString;
var
  current: PNode;
  result: string;
  leadingZero: boolean;
begin
  result := '';
  current := num;
  leadingZero := true; // Flag to skip leading zeros

  while current <> nil do
  begin
    // Skip leading zeros
    if leadingZero and (current^.data = 0) then
    begin
      current := current^.next; // Move to the next node
      continue;  // Skip printing the zero
    end;

    leadingZero := false; // After the first non-zero digit, we print all digits
    result := result + IntToStr(current^.data); // Append to result string
    current := current^.next; // Move to the next node
  end;

  GetNumberAsString := result; // Return the number as a string
end;



// Main program logic
var
  output: AnsiString;
begin
  Randomize;
  head1 := nil;
  tail1 := nil;
  head2 := nil;
  tail2 := nil;
  headSum := nil;
  tailSum := nil;

  // Generate two random large numbers with random length (up to 256 digits)
  writeln('Generating random number 1...');
  GenerateRandomNumber(head1, tail1);  // Random length (1-256 digits)
  writeln('Generating random number 2...');
  GenerateRandomNumber(head2, tail2);  // Random length (1-256 digits)
  
  // Gather the output for the first number
  output := 'Number 1: ';
  output := output + GetNumberAsString(head1) + sLineBreak;

  // Gather the output for the second number
  output := output + 'Number 2: ';
  output := output + GetNumberAsString(head2) + sLineBreak;

  // Add the numbers and store the result in the sum linked list
  AddNumbers(tail1, tail2, headSum, tailSum);

  // Gather the output for the sum
  output := output + 'Sum: ';
  output := output + GetNumberAsString(headSum) + sLineBreak;

  // Test if the sum is prime
  if IsPrimeLinkedList(headSum) then
    output := output + 'The sum is prime.' + sLineBreak
  else
    output := output + 'The sum is not prime.' + sLineBreak;


// writeln('Number 1: ', GetNumberAsString(head1));
// writeln('Number 2: ', GetNumberAsString(head2));
// writeln('Sum: ', GetNumberAsString(headSum));

// if IsPrimeLinkedList(headSum) then
//   writeln('The sum is prime.')
// else
//   writeln('The sum is not prime.');



writeln(output);  // Check the output before saving to the file


  // Save all output to the file
  SaveToFile('infinteger.txt', output);
end.
