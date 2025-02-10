 
program LinkedListDefinition;

uses sysutils;
type 
PNode = ^Node; //pointer to a node
Node = record
    data: string; //store large numbers
    next, prev: PNode; 
    end;

var
    head, tail: PNode; //global pointers for the start and end of a list

// begin
//     head := nil;
//     tail := nil;
//     writeln('Doubly Linked List Defined');
// end;

procedure Append(value: string);
var
    newNode: PNode;
begin
    //create new node and allocate memory
    new(newNode);
    
if newNode = nil then begin
    writeln('Memory allocation failed');
    exit;
end;
           
    newNode^.data := value; //give data the value
    newNode^.next := nil; // make the arrow next equal to null
    newNode^.prev := tail; // prev is going to point to the tail as we are going to create a new tail.

    {
    Node* newNode = new Node; //allocates memory for a new node
    newNode -> data = value //sets the data
    newNode -> next = nullptr // initializes next pointer
    newNode ->prev = tail //set previous pointer to the tail now that a new node is going to be appended

    }


    if head = nil then begin
    //if the list is empty then the new node is both the head and the tail
    head := newNode;
    tail := newNode;
    end
    else begin
    //if the list is not empty, update the previous tail's next pointer
    tail^.next := newNode; // same as tail->next = newNode;
    tail := newNode; //same as tail = newNode;
    end;
end;

function GenerateLargeNumber: string;
var
    num: string;
    i, length: integer;
begin
    length := Random(256) + 1; //Random length from 1 - 256 digits
    num := '';
    //ensure first digit is nonzero
    num := num +chr(Random(9) + 49); //49 = ASCII '1', ensures first digit is 1-9

    //generate remaining digits
    for i := 2 to length do
        num := num + chr(Random(10)+48); //48 = ASCII '0', ensures digits 0-9

    GenerateLargeNumber := num;
end;

function CompareLargeNumbers(num1, num2: string): Integer;
begin
    // If different lengths, the longer number is greater
    if Length(num1) > Length(num2) then
        Exit(1)  // num1 is greater
    else if Length(num1) < Length(num2) then
        Exit(-1) // num2 is greater
    else 
        Exit(CompareStr(num1, num2)); // Lexicographic comparison (only when lengths are equal)
end;



procedure BubbleSort;
var
    current, nextNode: PNode;
    swapped: Boolean;
    temp: string;
begin
    if head = nil then
    exit; //if the list is empty, then exit
    swapped := true;
    //repeat until no swaps are made
    while swapped do begin
    swapped := false;
    current := head;
    while current^.next <> nil do begin
    nextNode := current^.next;
    //if statement to check if current node is larger than next node, if so, swap
        if CompareLargeNumbers(current^.data, nextNode^.data) > 0 then begin
            //swap the data
            temp := current^.data;
            current^.data := nextNode^.data;
            nextNode^.data := temp;
            //set swapped flag to true
            swapped := true;
            
            end;
    current := current^.next; //move to the next node
        end;
    end;
end;




procedure RecursiveBubbleSort(current: PNode);
var
    nextNode: PNode;
    temp: string;
begin
    if (current = nil) or (current^.next = nil) then
    exit; //Base case that exits if empty list or only one node, cause if so it is already sorted

    nextNode := current^.next;
    //First ensure we don't access a null pointer
    if(nextNode <> nil) and (CompareLargeNumbers(current^.data, nextNode^.data) > 0) then
    begin
        //swapping values
        temp := current^.data;
        current^.data := nextNode^.data;
        nextNode^.data := temp;
    end;
    //move forward in the list
    RecursiveBubbleSort(nextNode);
end;

procedure FullBubbleSort(head: PNode);
var
    i: PNode;
    begin
        if (head = nil) or (head^.next = nil) then exit; //empty list or only one node then exit cause already sorted
        //keep running the recursion until it runs through every element
        i := head;
        while i <> nil do
        begin
            RecursiveBubbleSort(head); //sorts one pass
            i := i^.next; //move to the next node sort
        end;

    end;


procedure RemoveDupes; //function to remove nodes with duplicate values
var
    current, checkNode, temp: PNode; //define current node and checking node
    begin
        current := head; //begin at the first node
        while current <> nil do begin //while we have not reached the end of the list
            checkNode := current^.next; 
            while checkNode <> nil do begin //iterate each check until we reach the end
                if current^.data = checkNode^.data then begin
                temp := checkNode; //create temp node to be deleted
                    checkNode := checkNode^.next; //move node before deletion
                //remove duplicate node
                    if temp^.next <> nil then
                        temp^.next^.prev := temp^.prev; //if removed node has a node next, connect next node to its previous
                    if temp^.prev <> nil then
                        temp^.prev^.next := temp^.next; // if removed node has a previous node, connect previous to the next
                    if temp = tail then begin
                        tail := temp^.prev;
                        if tail = nil then
                            head := nil;
                    end;
                    dispose(temp); //deallocate memory of the duplicate node
                    
                end
                else
                    checkNode := checkNode^.next; //move to the next node to be compared to base node
               end;
               current := current^.next; //move to the next node to be compared to all nodes
            end; 
        end;




procedure PrintList;
var
    current: PNode; //create current node as a placeholder for the current node
begin
    current := head; //set current to the beginning/head
    if current = nil then begin
    writeln('Error: List is empty.');
    exit;
    end;
    while current <> nil do begin //while current != nullptr do this
        write(current^.data, ' '); //print out data of the current node and then create a space in between output arguments
        current := current^.next;
    end;
    writeln;
end;

procedure SaveToFile(filename: string);
var
    outputFile: Text;
    current: PNode;
begin
    Assign(outputFile, filename); //link file variable to to the actual file
    Rewrite(outputFile); //open file for writing (this erases previous content)

     if IOResult <> 0 then begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;
    
    current := head;
    while current <> nil do
    begin
        writeln(outputFile, current^.data); //write each number to a file
        current := current^.next;
    end;

    Close(outputFile); //close the file

    if IOResult <> 0 then begin
        Writeln('Error closing file: ', filename);
    end;


    Writeln('Sorted list saved to file: ', filename);
end;

begin
    Randomize;
    head := nil;
    tail := nil;

    // Generate and append random numbers
    writeln('Generating random large numbers...');
    Append(GenerateLargeNumber);
    Append(GenerateLargeNumber);
    Append(GenerateLargeNumber);
    Append(GenerateLargeNumber);
    Append(GenerateLargeNumber);

    writeln('Initial List:');
    PrintList;

    // Sort the list
    writeln('Sorting...');
    BubbleSort;

    writeln('Sorted List:');
    PrintList;
    SaveToFile('intlist.txt');

end.