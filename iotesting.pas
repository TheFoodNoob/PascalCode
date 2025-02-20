program FileWriteExample;
uses
  SysUtils, Classes;

var
  outputFile: TFileStream;
  textToWrite: string;
  byteArray: TBytes;
begin
  textToWrite := 'Hello, world!';
  
  // Convert string to TBytes correctly
  byteArray := TBytes(AnsiString(textToWrite));

  // Initialize the outputFile variable
  outputFile := nil;

  // Check if we can create the file stream
  outputFile := TFileStream.Create('test.txt', fmCreate);
  
  if Assigned(outputFile) then
  begin
    // Write the byte array to the file
    outputFile.WriteBuffer(byteArray[0], Length(byteArray));
    WriteLn('File written successfully.');
    
    // Free the file stream manually
    outputFile.Free;
  end
  else
    WriteLn('Failed to create or open the file.');
end.
