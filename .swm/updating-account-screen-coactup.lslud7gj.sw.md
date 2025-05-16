---
title: Updating Account Screen (COACTUP)
---
The Account Update Screen (COACTUP) allows users to update various details of an account, including account status, credit limits, balance, and customer information such as name, address, and contact details. This screen is essential for maintaining accurate and up-to-date account information.

## Screen Preview

```
Tran:     Date:
Prog:     Time:

                                Update Account

     Account Number : _____________     Active Y/N: _

     Opened : ____-__-__     Credit Limit : _______________

     Expiry : ____-__-__     Cash credit Limit : _______________

     Reissue: ____-__-__     Current Balance : _______________

     Current Cycle Credit: _______________

     Account Group: ____________     Current Cycle Debit : _______________

                                Customer Details

     Customer id : _________     SSN: 999-99-9999

     Date of birth: ____-__-__     FICO Score: ___

     First Name: ____________________________     Middle Name: ____________________________     Last Name: ____________________________

     Address: ____________________________________________     State __     Zip _____

     City: ____________________________________________     Country ___

     Phone 1: ___ ___ ____     Government Issued Id Ref : ______________________

     Phone 2: ___ ___ ____     EFT Account Id: ____________     Primary Card Holder Y/N: _

[Error/Status Message Area]

ENTER=Process F3=Exit F5=Save F12=Cancel
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Display-only field
- Color: Blue

### Current Date (CURDATE)

- Format: mm/dd/yy
- Display-only field
- Color: Blue

### Program Name (PGMNAME)

- Length: 8 characters
- Display-only field
- Color: Blue

### Current Time (CURTIME)

- Format: hh:mm:ss
- Display-only field
- Color: Blue

### Account Number (ACCTSID)

- Length: 11 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### Active Status (ACSTTUS)

- Length: 1 character
- Input field
- Underlined
- Highlighted
- Color: Default
- Valid values: 'Y' or 'N'

### Open Date (OPNYEAR, OPNMON, OPNDAY)

- Format: yyyy-mm-dd
- Input fields
- Underlined
- Highlighted
- Color: Default
- Year: 4 characters
- Month: 2 characters
- Day: 2 characters

### Credit Limit (ACRDLIM)

- Length: 15 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Expiry Date (EXPYEAR, EXPMON, EXPDAY)

- Format: yyyy-mm-dd
- Input fields
- Underlined
- Highlighted
- Color: Default
- Year: 4 characters
- Month: 2 characters
- Day: 2 characters

### Cash Credit Limit (ACSHLIM)

- Length: 15 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Reissue Date (RISYEAR, RISMON, RISDAY)

- Format: yyyy-mm-dd
- Input fields
- Underlined
- Highlighted
- Color: Default
- Year: 4 characters
- Month: 2 characters
- Day: 2 characters

### Current Balance (ACURBAL)

- Length: 15 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Current Cycle Credit (ACRCYCR)

- Length: 15 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Current Cycle Debit (ACRCYDB)

- Length: 15 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Account Group (AADDGRP)

- Length: 10 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### Customer ID (ACSTNUM)

- Length: 9 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### Social Security Number (ACTSSN1, ACTSSN2, ACTSSN3)

- Format: 999-99-9999
- Input fields
- Underlined
- Highlighted
- Color: Default
- Part 1: 3 characters
- Part 2: 2 characters
- Part 3: 4 characters
- Numeric validation

### Date of Birth (DOBYEAR, DOBMON, DOBDAY)

- Format: yyyy-mm-dd
- Input fields
- Underlined
- Highlighted
- Color: Default
- Year: 4 characters
- Month: 2 characters
- Day: 2 characters

### FICO Score (ACSTFCO)

- Length: 3 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation
- Valid range: 300 to 850

### First Name (ACSFNAM)

- Length: 25 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation

### Middle Name (ACSMNAM)

- Length: 25 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation

### Last Name (ACSLNAM)

- Length: 25 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation

### Address Line 1 (ACSADL1)

- Length: 50 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### State (ACSSTTE)

- Length: 2 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation
- Valid state codes

### Address Line 2 (ACSADL2)

- Length: 50 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### Zip Code (ACSZIPC)

- Length: 5 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### City (ACSCITY)

- Length: 50 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation

### Country (ACSCTRY)

- Length: 3 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Alphabetic validation

### Phone Number 1 (ACSPH1A, ACSPH1B, ACSPH1C)

- Format: (999)999-9999
- Input fields
- Underlined
- Highlighted
- Color: Default
- Area Code: 3 characters
- Prefix: 3 characters
- Line Number: 4 characters
- Numeric validation

### Government Issued ID (ACSGOVT)

- Length: 20 characters
- Input field
- Underlined
- Highlighted
- Color: Default

### Phone Number 2 (ACSPH2A, ACSPH2B, ACSPH2C)

- Format: (999)999-9999
- Input fields
- Underlined
- Highlighted
- Color: Default
- Area Code: 3 characters
- Prefix: 3 characters
- Line Number: 4 characters
- Numeric validation

### EFT Account ID (ACSEFTC)

- Length: 10 characters
- Input field
- Underlined
- Highlighted
- Color: Default
- Numeric validation

### Primary Card Holder (ACSPFLG)

- Length: 1 character
- Input field
- Underlined
- Highlighted
- Color: Default
- Valid values: 'Y' or 'N'

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBb3BlbmxlZ2FjeWRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="openlegacydemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
