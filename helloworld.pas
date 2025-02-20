program HelloWorld;



type
  PNode = ^Node;  // Pointer to a node
  Node = record
    data: integer;
    next, prev: PNode;  // Next and previous pointers
  end;

var
  head, tail: PNode;  // Global pointers for the start and end of the list

// Procedure to append a new node to the list
procedure Append(value: integer);
var
  newNode: PNode;
begin
  // Create a new node and allocate memory
  new(newNode);
  newNode^.data := value;
  newNode^.next := nil;  // Set next to nil
  newNode^.prev := tail; // Set prev to the current tail

  if head = nil then
  begin
    // If the list is empty, make the new node both the head and the tail
    head := newNode;
    tail := newNode;
  end
  else
  begin
    // If the list is not empty, update the current tail's next pointer
    tail^.next := newNode;
    tail := newNode;  // Update the tail to the new node
  end;
end;

// Procedure to print the list
procedure PrintList;
var
  current: PNode;
begin
  current := head;
  if current = nil then
  begin
    writeln('List is empty.');
    exit;
  end;

  // Traverse the list and print each node's data
  while current <> nil do
  begin
    write(current^.data, ' ');
    current := current^.next;
  end;
  writeln;
end;

begin
    writeln('Hello, World!');

     head := nil;  // Initialize the list as empty
  tail := nil;

  // Append some values to the list
  Append(10);
  Append(20);
  Append(30);

  // Print the list
  PrintList;  // Expected output: 10 20 30
end.
