---
title: Listing Users (COUSR00)
---
The COUSR00 screen is designed to list all users from the USRSEC file in the CardDemo application. It provides a user-friendly interface for searching and selecting user records, allowing actions such as updating or deleting user information.

## Screen Preview

```
Tran: CU00                      AWS Mainframe Modernization          Date: mm/dd/yy
Prog: COUSR00C                            CardDemo                   Time: hh:mm:ss

                                   List Users

     Search User ID: ________

     Sel   User ID    First Name            Last Name             Type
     ---   --------   --------------------  --------------------  ----
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _
     _     ________   ____________________  ____________________  _

Type 'U' to Update or 'D' to Delete a User from the list

ENTER=Continue F3=Back F7=Backward F8=Forward
```

## Fields

### Transaction Name (TRNNAME)

- Length: 4 characters
- Fixed value: 'CU00'
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Title (TITLE01)

- Length: 40 characters
- Fixed value: 'AWS Mainframe Modernization'
- Yellow color, normal intensity
- Field is skipped and set to be sent with the form

### Current Date (CURDATE)

- Length: 8 characters
- Fixed value: 'mm/dd/yy'
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Program Name (PGMNAME)

- Length: 8 characters
- Fixed value: 'COUSR00C'
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Title (TITLE02)

- Length: 40 characters
- Fixed value: 'CardDemo'
- Yellow color, normal intensity
- Field is skipped and set to be sent with the form

### Current Time (CURTIME)

- Length: 8 characters
- Fixed value: 'hh:mm:ss'
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Page Number (PAGENUM)

- Length: 8 characters
- Initial value: blank
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Search User ID (USRIDIN)

- Length: 8 characters
- Input field
- Green color, underlined
- Field is not protected and set to be sent with the form

### Selection Field (SEL0001 - SEL0010)

- Length: 1 character
- Input field
- Green color, underlined
- Field is not protected and set to be sent with the form

### User ID (USRID01 - USRID10)

- Length: 8 characters
- Initial value: blank
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### First Name (FNAME01 - FNAME10)

- Length: 20 characters
- Initial value: blank
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Last Name (LNAME01 - LNAME10)

- Length: 20 characters
- Initial value: blank
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### User Type (UTYPE01 - UTYPE10)

- Length: 1 character
- Initial value: blank
- Blue color, normal intensity
- Field is skipped and set to be sent with the form

### Error Message (ERRMSG)

- Length: 78 characters
- Initial value: blank
- Red color, bright intensity
- Field is skipped and set to be sent with the form

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBb3BlbmxlZ2FjeWRlbW8lM0ElM0FTd2ltbS1EZW1v" repo-name="openlegacydemo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
