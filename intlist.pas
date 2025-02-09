 
program LinkedListDefinition;

type 
PNode = ^Node; //pointer to a node
Node = record
    data: integer;
    next, prev: PNode; 
    end;

var
    head, tail: PNode; //global pointers for the start and end of a list

// begin
//     head := nil;
//     tail := nil;
//     writeln('Doubly Linked List Defined');
// end;

procedure Append(value: integer);
var
    newNode: PNode;
begin
    //create new node and allocate memory
    new(newNode);           
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

procedure BubbleSort;
var
    current, nextNode: PNode;
    swapped: Boolean;
    temp: Integer;
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
        if current^.data > nextNode^.data then begin
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
    temp: Integer;
begin
    if current = nil then
        exit; //if we reach the end of the list, then exit
    
    nextNode := current^.next;
    if current<>nil then begin
    //compare adjacent nodes and if current is greater than next node then swap values
        if current^.data > nextNode^.data then begin
            temp := current^.data;
            current^.data := nextNode^.data;
            nextNode^.data := temp;
            end;
            //recusively call bubble sort now using next node as the current node for the next recusion step
            if nextNode <> nil then
                RecursiveBubbleSort(nextNode);
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

begin
    head := nil;
    tail := nil;

    //TEST THE APPEND FUNCTION
    Append(25);
    Append(20);
    Append(30);
    //sort the list into ascending order
    //BubbleSort;
    //sort recusively
    RecursiveBubbleSort(head);
    //PRINT THE LIST
    PrintList; //EXPECTED OUTPUT: 10 20 30

end.