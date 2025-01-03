       IDENTIFICATION DIVISION.

       PROGRAM-ID. InventoryManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InventoryFile ASSIGN TO 'inventory.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT TempFile ASSIGN TO 'temp.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InventoryFile.
       01  InventoryRecord.
           05  ItemID          PIC 9(5).
           05  ItemName        PIC X(30).
           05  ItemQuantity    PIC 9(5).
           05  ItemPrice       PIC 9(6).

       FD  TempFile.
       01  TempRecord.
           05  TempItemID      PIC 9(5).
           05  TempItemName    PIC X(30).
           05  TempItemQuantity PIC 9(5).
           05  TempItemPrice PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-ItemID          PIC 9(5).
       01  WS-ItemName        PIC X(30).
       01  WS-ItemQuantity    PIC 9(5).
       01  WS-ItemPrice    PIC 9(5).
       01  WS-Continue        PIC X(1) VALUE 'Y'.
       01  WS-MenuChoice      PIC 9.
       01  EOF                PIC X VALUE 'N'.
       01  Found              PIC X VALUE 'N'.
       01  FILE-STATUS        PIC X(2) VALUE "00".
       
       PROCEDURE DIVISION.
       Main-Logic.
           PERFORM WITH TEST AFTER UNTIL WS-MenuChoice = 6
               PERFORM Display-Menu
               ACCEPT WS-MenuChoice
               EVALUATE WS-MenuChoice
                   WHEN 1
                       PERFORM Add-Item
                   WHEN 2
                       PERFORM Display-Records
                   WHEN 3
                       PERFORM Search-Item
                   WHEN 4
                       PERFORM Update-Item
                   WHEN 5
                       PERFORM Delete-Item
                   WHEN 6
                       DISPLAY "Exiting program..."
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       Display-Menu.
           DISPLAY "------------------------------"
           DISPLAY "   INVENTORY MANAGEMENT SYSTEM"
           DISPLAY "------------------------------"
           DISPLAY "Main Menu:"
           DISPLAY "1. Add an Item"
           DISPLAY "2. Display All Items"
           DISPLAY "3. Search for an Item"
           DISPLAY "4. Update an Item Quantity"
           DISPLAY "5. Delete an Item"
           DISPLAY "6. Exit"
           DISPLAY "Enter your choice: ".

       Add-Item.
           DISPLAY "Enter Item ID to add: "
           ACCEPT WS-ItemID
           MOVE 'N' TO Found
           PERFORM Search-ItemByID
           IF Found = 'Y'
               DISPLAY "Item already exists. "
               DISPLAY "Please update quantity instead."
           ELSE
               DISPLAY "Enter Item Name: "
               ACCEPT WS-ItemName
               DISPLAY "Enter Item Quantity: "
               ACCEPT WS-ItemQuantity
               DISPLAY "Enter Item Price: "
               ACCEPT WS-ItemPrice

               OPEN INPUT InventoryFile
               IF FILE-STATUS = "35"
                   OPEN OUTPUT InventoryFile
               ELSE
                   CLOSE InventoryFile
                   OPEN EXTEND InventoryFile
               END-IF

               MOVE WS-ItemID TO ItemID
               MOVE WS-ItemName TO ItemName
               MOVE WS-ItemQuantity TO ItemQuantity
               MOVE WS-ItemPrice TO ItemPrice

               WRITE InventoryRecord
               DISPLAY "Item added successfully!"
               CLOSE InventoryFile
           END-IF.

       Display-Records.
           OPEN INPUT InventoryFile
           MOVE 'N' TO EOF
           DISPLAY "------------------------------"
           DISPLAY "       CURRENT INVENTORY      "
           DISPLAY "------------------------------"
           PERFORM UNTIL EOF = 'Y'
               READ InventoryFile INTO InventoryRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       DISPLAY "Item ID: " ItemID
                       DISPLAY "Item Name: " ItemName
                       DISPLAY "Item Quantity: " ItemQuantity
                       DISPLAY "Item Price: " ItemPrice
                       DISPLAY "---------------------------"
               END-READ
           END-PERFORM
           CLOSE InventoryFile.

       Search-Item.
           DISPLAY "Enter Item ID to search: "
           ACCEPT WS-ItemID
           MOVE 'N' TO Found
           PERFORM Search-ItemByID
           IF Found = 'Y'
               DISPLAY "Item found!"
               DISPLAY "Item ID: " ItemID
               DISPLAY "Item Name: " ItemName
               DISPLAY "Item Quantity: " ItemQuantity
               DISPLAY "Item Price: " ItemPrice
           ELSE
               DISPLAY "Item not found."
           END-IF.

       Update-Item.
           DISPLAY "Enter Item ID to update: "
           ACCEPT WS-ItemID
           OPEN INPUT InventoryFile
           OPEN OUTPUT TempFile
           MOVE 'N' TO Found
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ InventoryFile INTO InventoryRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF ItemID = WS-ItemID
                           DISPLAY "Enter new quantity: "
                           ACCEPT WS-ItemQuantity
                           MOVE WS-ItemQuantity TO TempItemQuantity
                           MOVE ItemID TO TempItemID
                           MOVE ItemName TO TempItemName
                           MOVE ItemPrice TO TempItemPrice
                           MOVE 'Y' TO Found
                       ELSE
                           MOVE ItemID TO TempItemID
                           MOVE ItemName TO TempItemName
                           MOVE ItemQuantity TO TempItemQuantity
                           MOVE ItemPrice TO TempItemPrice
                       END-IF
                       WRITE TempRecord
               END-READ
           END-PERFORM
           CLOSE InventoryFile
           CLOSE TempFile
                      IF Found = 'N'
               DISPLAY "Item not found."
           ELSE
               DISPLAY "Item quantity updated successfully!"
               OPEN INPUT TempFile
               OPEN OUTPUT InventoryFile
               MOVE 'N' TO EOF
               PERFORM UNTIL EOF = 'Y'
                   READ TempFile INTO TempRecord
                       AT END
                           MOVE 'Y' TO EOF
                       NOT AT END
                           MOVE TempItemID TO ItemID
                           MOVE TempItemName TO ItemName
                           MOVE TempItemQuantity TO ItemQuantity
                           MOVE TempItemPrice TO ItemPrice
                           WRITE InventoryRecord
                   END-READ
               END-PERFORM
               CLOSE TempFile
               CLOSE InventoryFile
           END-IF.

       Delete-Item.
           DISPLAY "Enter Item ID to delete: "
           ACCEPT WS-ItemID
           OPEN INPUT InventoryFile
           OPEN OUTPUT TempFile
           MOVE 'N' TO Found
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ InventoryFile INTO InventoryRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF ItemID = WS-ItemID
                           MOVE 'Y' TO Found
                       ELSE
                           MOVE ItemID TO TempItemID
                           MOVE ItemName TO TempItemName
                           MOVE ItemQuantity TO TempItemQuantity
                           MOVE ItemPrice TO TempItemPrice
                           WRITE TempRecord
                       END-IF
               END-READ
           END-PERFORM
           CLOSE InventoryFile
           CLOSE TempFile
                      IF Found = 'N'
               DISPLAY "Item not found."
           ELSE
               DISPLAY "Item deleted successfully!"
               OPEN INPUT TempFile
               OPEN OUTPUT InventoryFile
               MOVE 'N' TO EOF
               PERFORM UNTIL EOF = 'Y'
                   READ TempFile INTO TempRecord
                       AT END
                           MOVE 'Y' TO EOF
                       NOT AT END
                           MOVE TempItemID TO ItemID
                           MOVE TempItemName TO ItemName
                           MOVE TempItemQuantity TO ItemQuantity
                           MOVE TempItemPrice TO ItemPrice
                           WRITE InventoryRecord
                   END-READ
               END-PERFORM
               CLOSE TempFile
               CLOSE InventoryFile
           END-IF.

       Search-ItemByID.
           MOVE 'N' TO EOF
           OPEN INPUT InventoryFile
           PERFORM UNTIL EOF = 'Y' OR Found = 'Y'
               READ InventoryFile INTO InventoryRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF ItemID = WS-ItemID
                           MOVE 'Y' TO Found
                       END-IF
               END-READ
           END-PERFORM
           CLOSE InventoryFile.
