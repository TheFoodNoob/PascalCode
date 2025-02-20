program DoublyLinkedListSort;

uses
  SysUtils;

type
  PNode = ^TNode;
  TNode = record
    data: integer;
    prev, next: PNode;
  end;
var
bubbleOperations, insertionOperations, selectionOperations, mergeOperations, quickIterOperations, quickRecOperations, quickMedOperations: Int64;  // To count operations during sorting
{ Append a new node with the given value at the end of the list }
procedure AppendNode(var head: PNode; value: integer);
var
  newNode, temp: PNode;
  
  
begin
  New(newNode);
  newNode^.data := value;
  newNode^.next := nil;
  newNode^.prev := nil;
  
  if head = nil then
    head := newNode
  else
  begin
    temp := head;
    while temp^.next <> nil do
      temp := temp^.next;
    temp^.next := newNode;
    newNode^.prev := temp;
  end;
end;

{ Print the elements of the list to the given file variable }
procedure PrintList(var f: Text; head: PNode);
var
  temp: PNode;
begin
  temp := head;
  while temp <> nil do
  begin
    Write(f, temp^.data, ' ');
    temp := temp^.next;
  end;
  WriteLn(f);
end;

{ Bubble sort implementation for the doubly linked list.
  It swaps the data values between nodes. }
procedure BubbleSort(var head: PNode);
var
  swapped: boolean;
  temp: PNode;
  tempData: integer;
begin
Inc(bubbleOperations);
  if head = nil then exit;
  repeat
  Inc(bubbleOperations);
    swapped := false;
    temp := head;
    while (temp^.next <> nil) do
    begin
    Inc(bubbleOperations);
      if temp^.data > temp^.next^.data then
      begin
      Inc(bubbleOperations);
        tempData := temp^.data;
        temp^.data := temp^.next^.data;
        temp^.next^.data := tempData;
        swapped := true;
      end;
      temp := temp^.next;
    end;
  until not swapped;
end;

{ Insertion sort implementation for the doubly linked list.
  It removes out-of-place nodes and re-inserts them in the sorted part. }
procedure InsertionSort(var head: PNode);
var
  current, nextNode, pos: PNode;
begin
Inc(insertionOperations);
  if head = nil then exit;

  current := head^.next;
  while current <> nil do
  begin
  Inc(insertionOperations);
    nextNode := current^.next;
    pos := current^.prev;
    { Find the spot where current node should be inserted }
    while (pos <> nil) and (current^.data < pos^.data) do
    begin
    Inc(insertionOperations);
      pos := pos^.prev;
    end;
    { If current is not already in correct position, re-link it }
    Inc(insertionOperations);
    if current^.prev <> pos then
    begin
      Inc(insertionOperations);
      { Unlink current node }
      if current^.prev <> nil then
        current^.prev^.next := current^.next;
      Inc(insertionOperations);
      if current^.next <> nil then
        current^.next^.prev := current^.prev;
      
      { Insert current node after pos }
      Inc(insertionOperations);
      if pos = nil then
      begin
        { Inserting at the beginning }
        Inc(insertionOperations);
        current^.next := head;
        head^.prev := current;
        current^.prev := nil;
        head := current;
      end
      else
      begin
        Inc(insertionOperations);
        current^.next := pos^.next;
        if pos^.next <> nil then
        begin
          pos^.next^.prev := current;
          Inc(insertionOperations);
          end;
        pos^.next := current;
        current^.prev := pos;
      end;
    end;
    current := nextNode;
  end;
end;

{ ================== Merge Sort for Doubly Linked List =================== }

{ Split the list into two halves; frontref and backref }
procedure SplitList(head: PNode; var first, second: PNode);
var
  slow, fast: PNode;
begin
Inc(mergeOperations);
  if head = nil then
  begin
    first := nil;
    second := nil;
    Exit;
  end;
  slow := head;
  fast := head^.next;
  while (fast <> nil) do
  begin
  Inc(mergeOperations);
    fast := fast^.next;
    Inc(mergeOperations);
    if fast <> nil then
    begin
      Inc(mergeOperations);
      slow := slow^.next;
      fast := fast^.next;
    end;
  end;
  first := head;
  second := slow^.next;
  slow^.next := nil;
  Inc(mergeOperations);
  if second <> nil then
    second^.prev := nil;
end;

{ Merge two sorted lists }
function SortedMerge(a, b: PNode): PNode;
var
  resultList: PNode;
begin
Inc(mergeOperations);
  if a = nil then
    Exit(b);
  Inc(mergeOperations);
  if b = nil then
    Exit(a);
    
  Inc(mergeOperations);
  if a^.data <= b^.data then
  begin
    resultList := a;
    resultList^.next := SortedMerge(a^.next, b);
    Inc(mergeOperations);
    if resultList^.next <> nil then
      resultList^.next^.prev := resultList;
    resultList^.prev := nil;
  end
  else
  begin
    resultList := b;
    resultList^.next := SortedMerge(a, b^.next);
    Inc(mergeOperations);
    if resultList^.next <> nil then
      resultList^.next^.prev := resultList;
    resultList^.prev := nil;
  end;
  SortedMerge := resultList;
end;

{ Merge sort that returns the new head of the sorted list }
function MergeSortList(head: PNode): PNode;
var
  first, second: PNode;
begin
Inc(mergeOperations);
  if (head = nil) or (head^.next = nil) then
    Exit(head);
  SplitList(head, first, second);
  first := MergeSortList(first);
  second := MergeSortList(second);
  MergeSortList := SortedMerge(first, second);
end;

{ ================= QuickSort (Hoare partition scheme) for Doubly Linked List ================= }

{ Helper: Get tail of the list }
function GetTail(head: PNode): PNode;
begin
  Inc(quickIterOperations);
  if head = nil then Exit(nil);
  while head^.next <> nil do
  begin
    head := head^.next;
    Inc(quickIterOperations);
    end;
  GetTail := head;
end;

{ Swap the data of two nodes }
procedure SwapData(a, b: PNode);
var
  temp: integer;
begin
  Inc(quickIterOperations);
  temp := a^.data;
  a^.data := b^.data;
  b^.data := temp;
end;

{ Partition using Hoare’s scheme.
  Returns the partition node. }
function PartitionHoare(low, high: PNode): PNode;
var
  pivot: integer;
  i, j: PNode;
begin
  pivot := low^.data;
  i := low;
  j := high;
  
  while True do
  begin
  Inc(quickIterOperations);
    while (i <> high) and (i^.data < pivot) do
    begin
      Inc(quickIterOperations);
      i := i^.next;
    end;
    while (j <> low) and (j^.data > pivot) do
    begin
      j := j^.prev;
      Inc(quickIterOperations);
    end;
      
    if i = j then
    begin
    Inc(quickIterOperations);
      PartitionHoare := i;
      Exit;
    end;
    
    if (i^.prev = j) or (j^.next = i) then
    begin
    Inc(quickIterOperations);
      PartitionHoare := j;
      Exit;
    end;
    
    SwapData(i, j);
    i := i^.next;
    j := j^.prev;
  end;
end;

{ Recursive QuickSort using Hoare’s partition }
procedure QuickSortHoareRec(low, high: PNode);
var
  p: PNode;
begin
  if (low <> nil) and (high <> nil) and (low <> high) and (low <> high^.next) then
  begin
  Inc(quickRecOperations);
    p := PartitionHoare(low, high);
    QuickSortHoareRec(low, p);
    if (p <> nil) and (p^.next <> nil) then
    begin
    Inc(quickRecOperations);
      QuickSortHoareRec(p^.next, high);
      end;
  end;
end;

{ Iterative QuickSort using Hoare’s partition.
  Uses an array as a simple stack of node-pairs. }
type
  TNodePair = record
    low, high: PNode;
  end;

procedure QuickSortHoareIter(head, tail: PNode);
const
  MAX_STACK = 1000;
var
  stack: array[1..MAX_STACK] of TNodePair;
  top: Integer;
  p: TNodePair;
  part: PNode;
begin
Inc(quickIterOperations);
  top := 0;
  { Push initial pair }
  Inc(top);
  stack[top].low := head;
  stack[top].high := tail;
  
  while top > 0 do
  begin
  Inc(quickIterOperations);
    p := stack[top];
    Dec(top);
    { Partition the sublist }
    part := PartitionHoare(p.low, p.high);
    
    { If there are elements on the left side of pivot, push left sublist }
    if (p.low <> part) then
    begin
    Inc(quickIterOperations);
      Inc(top);
      stack[top].low := p.low;
      stack[top].high := part;
    end;
    { If there are elements on the right side of pivot, push right sublist }
    if (part <> p.high) and (part^.next <> nil) then
    begin
    Inc(quickIterOperations);
      Inc(top);
      stack[top].low := part^.next;
      stack[top].high := p.high;
    end;
  end;
end;

{ =============== QuickSort using median-of-three pivot =============== }

{ Get the middle node between low and high (inclusive) }
function GetMiddle(low, high: PNode): PNode;
var
  slow, fast: PNode;
begin
  slow := low;
  fast := low;
  while (fast <> high) and (fast^.next <> high) do
  begin
  Inc(quickMedOperations);
    fast := fast^.next^.next;
    slow := slow^.next;
  end;
  GetMiddle := slow;
end;

{ Choose median-of-three pivot from low, middle, high.
  Swap chosen pivot into the low node. }
procedure ChooseMedianPivot(low, high: PNode);
var
  mid: PNode;
  a, b, c: integer;
begin
  mid := GetMiddle(low, high);
  a := low^.data;
  b := mid^.data;
  c := high^.data;
  
  { Determine median value and swap it into low }
  if (a < b) then
  begin
  Inc(quickMedOperations);
    if (b < c) then
      begin
      SwapData(low, mid);        { median is b }
      Inc(quickMedOperations);
      end
    else if (a < c) then
    begin
      SwapData(low, high);       { median is c }
      Inc(quickMedOperations);
      end
    else
      begin
      Inc(quickMedOperations);
        { median is a, already at low; do nothing }
      end;
  end
  else
  begin
  Inc(quickMedOperations);
    if (a < c) then
      begin
      Inc(quickMedOperations);
        { median is a, already at low; do nothing }
      end
    else if (b < c) then
    begin
      SwapData(low, high);       { median is c }
    Inc(quickMedOperations);
    end
    else
    begin
      SwapData(low, mid);       { median is b }
      Inc(quickMedOperations);
      end;
  end;
end;

{ Recursive QuickSort using median-of-three pivot and Hoare partition }
procedure QuickSortMedianRec(low, high: PNode);
var
  p: PNode;
begin
  if (low <> nil) and (high <> nil) and (low <> high) and (low <> high^.next) then
  begin
  Inc(quickMedOperations);
    ChooseMedianPivot(low, high);
    p := PartitionHoare(low, high);
    QuickSortMedianRec(low, p);
    if (p <> nil) and (p^.next <> nil) then
    begin
      QuickSortMedianRec(p^.next, high);
      Inc(quickMedOperations);
    end;
  end;
end;

{ Selection sort implementation for the doubly linked list.
  It selects the minimum element in each iteration and swaps it with the current node. }
procedure SelectionSort(var head: PNode);
var
  temp, minNode, current: PNode;
  tempData: integer;
begin
  Inc(selectionOperations);
  if head = nil then exit;

  current := head;
  while current <> nil do
  begin
    Inc(selectionOperations);
    minNode := current;
    temp := current^.next;
    
    { Find the minimum node in the remaining unsorted part of the list }
    while temp <> nil do
    begin
    Inc(selectionOperations);
      if temp^.data < minNode^.data then
      begin
      Inc(selectionOperations);
        minNode := temp;
        end;
      temp := temp^.next;
    end;

    { If the minimum node is not the current node, swap their data }
    if minNode <> current then
    begin
    Inc(selectionOperations);
      tempData := current^.data;
      current^.data := minNode^.data;
      minNode^.data := tempData;
    end;

    { Move to the next node }
    current := current^.next;
  end;
end;



{ ======================== Main Program ============================ }

var
  unsortedList, bubbleList, insertionList, selectionList: PNode;
  mergeList, quickHoareIterList, quickHoareRecList, quickMedianList: PNode;
  f: Text;
  i, numNodes: integer;
  numbers: array of integer;
begin
  unsortedList := nil;
  bubbleList := nil;
  insertionList := nil;
  selectionList := nil;
  mergeList := nil;
  quickHoareIterList := nil;
  quickHoareRecList := nil;
  quickMedianList := nil;
  
  // Initialize random number generator and determine number of nodes
  Randomize;
  numNodes := Random(21) + 10;  // Random number between 10 and 30
  SetLength(numbers, numNodes);
  
  { Generate random integer values (0..99) and store them in an array }
  for i := 0 to numNodes - 1 do
    numbers[i] := Random(100) + 1;
  
  { Build the unsorted linked list and duplicate lists from the array }
  for i := 0 to numNodes - 1 do
  begin
    AppendNode(unsortedList, numbers[i]);
    AppendNode(bubbleList, numbers[i]);
    AppendNode(insertionList, numbers[i]);
    AppendNode(selectionList, numbers[i]);
    AppendNode(mergeList, numbers[i]);
    AppendNode(quickHoareIterList, numbers[i]);
    AppendNode(quickHoareRecList, numbers[i]);
    AppendNode(quickMedianList, numbers[i]);
  end;
  
  { Open output file and print the unsorted and sorted lists }
  assign(f, 'arrsort.txt');
  Rewrite(f);
  
  WriteLn(f, 'Unsorted List:');
  PrintList(f, unsortedList);
  WriteLn(f);  // Skips a line
  

  
  BubbleSort(bubbleList);
  WriteLn(f, 'List Sorted with Bubble Sort:');
  PrintList(f, bubbleList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(bubbleOperations));
  WriteLn(f);  // Skips a line

  
  InsertionSort(insertionList);
  WriteLn(f, 'List Sorted with Insertion Sort:');
  PrintList(f, insertionList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(insertionOperations));
  WriteLn(f);  // Skips a line


  SelectionSort(selectionList);
  WriteLn(f, 'List Sorted with Selection Sort:');
  PrintList(f, selectionList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(selectionOperations));
  WriteLn(f);  // Skips a line

  
  mergeList := MergeSortList(mergeList);
  WriteLn(f, 'List Sorted with Merge Sort:');
  PrintList(f, mergeList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(mergeOperations));
  WriteLn(f);  // Skips a line

  
  { QuickSort using Hoare partition (iterative) }
  QuickSortHoareIter(quickHoareIterList, GetTail(quickHoareIterList));
  WriteLn(f, 'List Sorted with QuickSort Hoare (Iterative):');
  PrintList(f, quickHoareIterList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(quickIterOperations));
  WriteLn(f);  // Skips a line


  { QuickSort using Hoare partition (recursive) }
  QuickSortHoareRec(quickHoareRecList, GetTail(quickHoareRecList));
  WriteLn(f, 'List Sorted with QuickSort Hoare (Recursive):');
  PrintList(f, quickHoareRecList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(quickRecOperations));
  WriteLn(f);  // Skips a line

  
  { QuickSort using median-of-three pivot (recursive) }
  QuickSortMedianRec(quickMedianList, GetTail(quickMedianList));
  WriteLn(f, 'List Sorted with QuickSort (Median-of-Three):');
  PrintList(f, quickMedianList);
  WriteLn(f);  // Skips a line
  WriteLn(f, 'Number of Operations: ' + IntToStr(quickMedOperations));  
  WriteLn(f);  // Skips a line

  
  close(f);
end.