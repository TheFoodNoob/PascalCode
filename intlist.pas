program intlist;

uses sysutils;

type
    PDigit = ^DigitNode;
    DigitNode = record
        digit: char;
        next: PDigit;
    end;

    PNode = ^Node;
    Node = record
        data: string;       // Holds the number as a string representation
        next, prev: PNode;  // Pointers to next and previous nodes in the outer list
        digits: PDigit;     // Pointer to the inner linked list representing digits
    end;

var
    head, tail, bubbleSortedHead, recursiveBubbleSortedHead: PNode;
    operationCount, numOperations: Int64; // Count operations for complexity analysis
    bubbleOperations, recursiveBubbleOperations: Int64;  // To count operations during sorting

// Function to generate large random numbers and store them as linked lists of digits
function GenerateLargeNumber: PNode;
var
    numNode: PNode;
    digitNode, currentDigit: PDigit;
    length, i: Integer;
begin
    New(numNode);
    numNode^.next := nil;
    numNode^.prev := nil;
    New(numNode^.digits);  // Initialize the digits linked list
    numNode^.digits := nil;

    length := Random(256) + 1;  // Random length between 1 and 256
    currentDigit := nil;
    for i := 1 to length do
    begin
        New(digitNode);
        digitNode^.digit := chr(Random(10) + 48);  // '0' to '9'
        digitNode^.next := nil;
        
        if numNode^.digits = nil then
            numNode^.digits := digitNode
        else
            currentDigit^.next := digitNode;
        
        currentDigit := digitNode;
    end;

    GenerateLargeNumber := numNode;
end;

// Procedure to append a new node to the doubly linked list
procedure Append(value: PNode);
begin
    if head = nil then
    begin
        head := value;
        tail := value;
    end
    else
    begin
        tail^.next := value;
        value^.prev := tail;
        tail := value;
    end;
end;

// Function to compare two large numbers represented as linked lists of digits
function CompareLargeNumbers(num1, num2: PNode): Integer;
var
    digit1, digit2: PDigit;
    length1, length2: Integer;
begin
    // Calculate lengths of both numbers by counting the digits
    length1 := 0;
    digit1 := num1^.digits;
    while digit1 <> nil do
    begin
        operationCount := operationCount + 1;  // Increment operation count for each comparison
        Inc(recursiveBubbleOperations);
        Inc(bubbleOperations);  // Increment for comparison operation
        Inc(length1);
        digit1 := digit1^.next;
    end;

    length2 := 0;
    digit2 := num2^.digits;
    while digit2 <> nil do
    begin
        Inc(length2);
        digit2 := digit2^.next;
    end;

    // Compare lengths first
    if length1 > length2 then
        Exit(1)
    else if length1 < length2 then
        Exit(-1);

    // If lengths are the same, compare digit by digit
    digit1 := num1^.digits;
    digit2 := num2^.digits;
    while (digit1 <> nil) and (digit2 <> nil) do
    begin
        if digit1^.digit > digit2^.digit then
            Exit(1)
        else if digit1^.digit < digit2^.digit then
            Exit(-1);
        
        digit1 := digit1^.next;
        digit2 := digit2^.next;
    end;

    // If we reach here, both numbers are equal
    Exit(0);
end;

// Function to swap the data of two nodes (swap the contents)
procedure SwapData(node1, node2: PNode);
var
    tempDigits: PDigit;
    tempData: string;
begin
    // Swap the digits part of the nodes
    tempDigits := node1^.digits;
    node1^.digits := node2^.digits;
    node2^.digits := tempDigits;

    // Swap the data (string representation) of the nodes
    tempData := node1^.data;
    node1^.data := node2^.data;
    node2^.data := tempData;
end;

// Iterative Bubble Sort for doubly linked list (O(n²))
procedure BubbleSort;
var
    current, nextNode: PNode;
    swapped: Boolean;
begin
    if bubbleSortedHead = nil then Exit;

    bubbleOperations := 0;  // Reset operation count

    repeat
        swapped := False;
        current := bubbleSortedHead;

        while current^.next <> nil do
        begin
            nextNode := current^.next;
            Inc(bubbleOperations);  // Increment for comparison operation
            if CompareLargeNumbers(current, nextNode) > 0 then
            begin
                Inc(bubbleOperations);  // Increment for swap operation
                SwapData(current, nextNode);
                swapped := True;
            end;
            current := current^.next;
        end;
    until not swapped;
end;

// Recursive function to perform one pass of the bubble sort
procedure RecursiveBubbleSort(current: PNode);
var
    nextNode: PNode;
begin
    if (current = nil) or (current^.next = nil) then
        exit; // Base case: exit if list is empty or only one element (it's already sorted)

    nextNode := current^.next;
    
    // Increment the operation count for each comparison
    Inc(recursiveBubbleOperations);  // Increment comparison count

    // First, ensure we don't access a null pointer and compare values
    if (nextNode <> nil) and (CompareLargeNumbers(current, nextNode) > 0) then
    begin
        // Increment the operation count for each swap
        Inc(recursiveBubbleOperations);  // Increment swap operation count

        // Swap values (data and digits) between the nodes
        SwapData(current, nextNode);
    end;

    // Recursively call the function to sort the next pair
    RecursiveBubbleSort(nextNode);
end;

// Function to call the recursive bubble sort for multiple passes through the list
procedure FullRecursiveBubbleSort(head: PNode);
var
    i: PNode;
begin
    if (head = nil) or (head^.next = nil) then
        exit; // Exit if the list is empty or only has one element (already sorted)

    // Reset recursiveBubbleOperations count before starting
    recursiveBubbleOperations := 0;

    // Keep calling the recursive function for every pass through the list
    i := head;
    while i <> nil do
    begin
        RecursiveBubbleSort(head); // Perform one pass of sorting
        i := i^.next; // Move to the next node
    end;
end;


// Function to create a copy of the list
function CopyList(originalHead: PNode): PNode;
var
    copyHead, copyTail, newNode: PNode;
begin
    copyHead := nil;
    copyTail := nil;
    
    while originalHead <> nil do
    begin
        New(newNode);
        newNode^.data := originalHead^.data;
        newNode^.digits := originalHead^.digits;  // Shallow copy of digits (could deep copy if needed)
        newNode^.next := nil;
        newNode^.prev := copyTail;
        
        if copyTail <> nil then
            copyTail^.next := newNode
        else
            copyHead := newNode;
        
        copyTail := newNode;
        originalHead := originalHead^.next;
    end;
    
    CopyList := copyHead;
end;

// Procedure to print the linked list recursively
procedure PrintListRecursive(node: PNode);
var
    digit: PDigit;
begin
    if node = nil then
    begin
        writeln;
        Exit;
    end;
    
    // Print the digits of the current large number
    digit := node^.digits;
    while digit <> nil do
    begin
        write(digit^.digit);
        digit := digit^.next;
    end;
    writeln;
    
    PrintListRecursive(node^.next);
end;

// Procedure to save the sorted list and operation counts to a file
procedure SaveToFile(filename: string; bubbleOperations, recursiveBubbleOperations: Int64; head, bubbleSortedHead, recursiveBubbleSortedHead: PNode);
var
    outputFile: Text;
    current: PNode;
    digit: PDigit;
begin
    Assign(outputFile, filename);
    Rewrite(outputFile);
    
    if IOResult <> 0 then
    begin
        Writeln('Error opening file: ', filename);
        Exit;
    end;

    // Write the initial unsorted list and operations
    writeln(outputFile, 'Unsorted List:');
    current := head;
    while current <> nil do
    begin
        digit := current^.digits;
        while digit <> nil do
        begin
            write(outputFile, digit^.digit);
            digit := digit^.next;
        end;
        writeln(outputFile);
        current := current^.next;
    end;
    writeln(outputFile, 'Operations during Unsorted List Generation: ', operationCount);
    writeln(outputFile);  // Empty line to separate sections

    // Write the results for Bubble Sort
    writeln(outputFile, 'Bubble Sort Results:');
    writeln(outputFile, 'Number of Operations: ', bubbleOperations);
    writeln(outputFile, 'Sorted List (Bubble Sort):');
    current := bubbleSortedHead;
    while current <> nil do
    begin
        digit := current^.digits;
        while digit <> nil do
        begin
            write(outputFile, digit^.digit);
            digit := digit^.next;
        end;
        writeln(outputFile);
        current := current^.next;
    end;
    writeln(outputFile, 'Operations during Bubble Sort: ', bubbleOperations);
    writeln(outputFile);  // Empty line to separate sections

    

    // Write the results for Recursive Bubble Sort
    writeln(outputFile, 'Recursive Bubble Sort Results:');
    writeln(outputFile, 'Number of Operations: ', recursiveBubbleOperations);
    writeln(outputFile, 'Sorted List (Recursive Bubble Sort):');
    current := recursiveBubbleSortedHead;
    while current <> nil do
    begin
        digit := current^.digits;
        while digit <> nil do
        begin
            write(outputFile, digit^.digit);
            digit := digit^.next;
        end;
        writeln(outputFile);
        current := current^.next;
    end;
    writeln(outputFile, 'Operations during Recursive Bubble Sort: ', recursiveBubbleOperations);
    writeln(outputFile);  // Empty line to separate sections

    // Write metadata to file
    writeln(outputFile, '# of Numbers Generated: ', numOperations);
    Close(outputFile);

    if IOResult <> 0 then
        Writeln('Error closing file: ', filename);

    Writeln('Sorted list saved to file: ', filename);
end;



// Procedure to count operations and generate complexity table
procedure ComplexityAnalysis(m: Integer; operationCount: Int64);
begin
    writeln('M: ', m, ' | Total Operations: ', operationCount);
end;

var
    i: Integer;
    
begin
    Randomize;
    head := nil;
    tail := nil;
    operationCount := 0;

    // Generate a random number of operations (e.g., between 5 and 20)
    numOperations := Random(151)+10;  //Random(16) // Random number between 10 and 160
    writeln('Generating ', numOperations, ' random large numbers...');

    // Append a random number of large numbers to the list
    for i := 1 to numOperations do
    begin
        Append(GenerateLargeNumber);
    end;

    // Complexity analysis after number generation
    ComplexityAnalysis(numOperations, operationCount);

    writeln('Initial List:');
    PrintListRecursive(head);

     // Create copies of the original list for sorting with bubble sort and recursive bubble sort
    bubbleSortedHead := CopyList(head);  // Copy for Bubble Sort
    recursiveBubbleSortedHead := CopyList(head);  // Copy for Recursive Bubble Sort

    // Bubble Sort
    operationCount := 0;  // Reset operation count before sorting
    writeln('Bubble Sort...');
    BubbleSort;
    ComplexityAnalysis(numOperations, operationCount);  // After sorting
    writeln('Bubble Sorted List:');
    PrintListRecursive(bubbleSortedHead);

    

    // Recursive Bubble Sort
    operationCount := 0;  // Reset operation count before sorting
    writeln('Recursive Bubble Sort...');
    FullRecursiveBubbleSort(recursiveBubbleSortedHead);
    ComplexityAnalysis(numOperations, operationCount);  // After sorting
    writeln('Recursive Bubble Sorted List:');
    PrintListRecursive(recursiveBubbleSortedHead);

    // Save to file
    SaveToFile('intlist.txt', bubbleOperations, recursiveBubbleOperations, head, bubbleSortedHead, recursiveBubbleSortedHead);

    // Print final complexity analysis (total operations after all steps)
    ComplexityAnalysis(numOperations, operationCount); // Final total operations
end.
