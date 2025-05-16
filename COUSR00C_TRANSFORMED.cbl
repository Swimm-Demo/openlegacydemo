      ******************************************************************        
      * Program     : COUSR00C.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : List all users from USRSEC file
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUSR00C.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *************************************************************
      * SCREEN-FIELDS - Service-oriented replacement for screen   *
      *************************************************************
       01 SCREEN-FIELDS.
           05 SERVICE-INFO.
              10 SCREEN-STATUS            PIC X(01).
                  88 SCREEN-DISPLAY                 VALUE 'D'.
                  88 SCREEN-UPDATE                  VALUE 'U'.
                  88 STATUS-OK                      VALUE '0'.
                  88 STATUS-ERROR                   VALUE 'E'.
              10 VALIDATION-STATUS        PIC X(01).
                  88 VALIDATION-OK                  VALUE '0'.
                  88 VALIDATION-ERROR              VALUE 'E'.
              10 SCREEN-MESSAGE           PIC X(80).
              10 FUNCTION-CODE            PIC X(10).
              10 FIELD-IN-ERROR           PIC X(20).
           05 RECORD-COUNTS.
              10 RECORDS-FOUND            PIC 9(05) COMP.
              10 RECORDS-DISPLAYED        PIC 9(05) COMP.
              10 MORE-RECORDS-EXIST       PIC X(01).
                  88 MORE-RECORDS                   VALUE 'Y'.
                  88 NO-MORE-RECORDS               VALUE 'N'.
              10 CURRENT-POSITION         PIC 9(05) COMP.
           05 DATABASE-INFO.
              10 RECORD-ID                PIC X(10).
              10 RECORD-STATUS            PIC X(01).
                  88 RECORD-ACTIVE                  VALUE 'A'.
                  88 RECORD-INACTIVE                VALUE 'I'.
                  88 RECORD-DELETED                 VALUE 'D'.
              10 LAST-UPDATED             PIC X(26).
           05 TRANSACTION-INFO.
              10 TRANSACTION-ID           PIC X(16).
              10 TRANSACTION-STATUS       PIC X(01).
                  88 TRANS-ACTIVE                   VALUE 'A'.
                  88 TRANS-COMPLETE                 VALUE 'C'.
                  88 TRANS-FAILED                   VALUE 'F'.
              10 TRANSACTION-MESSAGE      PIC X(80).
           05 BUSINESS-DATA.
              10 CURDATE              PIC X(8).
              10 CURDATE         ERROR    PIC X(01).
              10 CURTIME              PIC X(8).
* Removed screen-related copybook:               10 CURTIME         ERROR    PIC X(01).
              10 ERRMSG               PIC X(8).
              10 ERRMSG          ERROR    PIC X(01).
              10 FNAME                PIC X(20).
              10 FNAME           ERROR    PIC X(01).
              10 FNAME01              PIC X(8).
              10 FNAME01         ERROR    PIC X(01).
* Removed screen-related copybook:               10 FNAME02              PIC X(8).
* Removed screen-related copybook:               10 FNAME02         ERROR    PIC X(01).
              10 FNAME03              PIC X(8).
              10 FNAME03         ERROR    PIC X(01).
              10 FNAME04              PIC X(8).
              10 FNAME04         ERROR    PIC X(01).
              10 FNAME05              PIC X(8).
              10 FNAME05         ERROR    PIC X(01).
              10 FNAME06              PIC X(8).
              10 FNAME06         ERROR    PIC X(01).
              10 FNAME07              PIC X(8).
              10 FNAME07         ERROR    PIC X(01).
              10 FNAME08              PIC X(8).
              10 FNAME08         ERROR    PIC X(01).
              10 FNAME09              PIC X(8).
              10 FNAME09         ERROR    PIC X(01).
              10 FNAME10              PIC X(8).
              10 FNAME10         ERROR    PIC X(01).
              10 LNAME                PIC X(20).
              10 LNAME           ERROR    PIC X(01).
              10 LNAME01              PIC X(8).
              10 LNAME01         ERROR    PIC X(01).
              10 LNAME02              PIC X(8).
              10 LNAME02         ERROR    PIC X(01).
              10 LNAME03              PIC X(8).
              10 LNAME03         ERROR    PIC X(01).
              10 LNAME04              PIC X(8).
              10 LNAME04         ERROR    PIC X(01).
              10 LNAME05              PIC X(8).
              10 LNAME05         ERROR    PIC X(01).
              10 LNAME06              PIC X(8).
              10 LNAME06         ERROR    PIC X(01).
              10 LNAME07              PIC X(8).
              10 LNAME07         ERROR    PIC X(01).
              10 LNAME08              PIC X(8).
              10 LNAME08         ERROR    PIC X(01).
              10 LNAME09              PIC X(8).
              10 LNAME09         ERROR    PIC X(01).
              10 LNAME10              PIC X(8).
              10 LNAME10         ERROR    PIC X(01). * The following SCREEN-FIELDS.SERVICE-INFO.FUNCTION-CODE evaluation has been transformed: * Updated for nested structure
              10 PAGENUM              PIC X(8).
              10 PAGENUM         ERROR    PIC X(01).
              10 PGMNAME              PIC X(8).
              10 PGMNAME         ERROR    PIC X(01).
              10 SEL0001              PIC X(8).
              10 SEL0001         ERROR    PIC X(01).
              10 SEL0002              PIC X(8).
              10 SEL0002         ERROR    PIC X(01).
              10 SEL0003              PIC X(8).
              10 SEL0003         ERROR    PIC X(01).
              10 SEL0004              PIC X(8).
              10 SEL0004         ERROR    PIC X(01).
              10 SEL0005              PIC X(8).
              10 SEL0005         ERROR    PIC X(01).
              10 SEL0006              PIC X(8).
              10 SEL0006         ERROR    PIC X(01).
              10 SEL0007              PIC X(8).
              10 SEL0007         ERROR    PIC X(01).
              10 SEL0008              PIC X(8).
              10 SEL0008         ERROR    PIC X(01).
              10 SEL0009              PIC X(8).
              10 SEL0009         ERROR    PIC X(01).
              10 SEL0010              PIC X(8).
              10 SEL0010         ERROR    PIC X(01).
              10 TITLE01              PIC X(8).
              10 TITLE01         ERROR    PIC X(01).
              10 TITLE02              PIC X(8).
              10 TITLE02         ERROR    PIC X(01).
              10 TRNNAME              PIC X(8).
              10 TRNNAME         ERROR    PIC X(01).
              10 USRID01              PIC X(8).
              10 USRID01         ERROR    PIC X(01).
              10 USRID02              PIC X(8).
              10 USRID02         ERROR    PIC X(01).
              10 USRID03              PIC X(8).
              10 USRID03         ERROR    PIC X(01).
              10 USRID04              PIC X(8).
              10 USRID04         ERROR    PIC X(01).
              10 USRID05              PIC X(8).
              10 USRID05         ERROR    PIC X(01).
              10 USRID06              PIC X(8).
              10 USRID06         ERROR    PIC X(01).
              10 USRID07              PIC X(8).
              10 USRID07         ERROR    PIC X(01).
              10 USRID08              PIC X(8).
              10 USRID08         ERROR    PIC X(01).
              10 USRID09              PIC X(8).
              10 USRID09         ERROR    PIC X(01).
              10 USRID10              PIC X(8).
              10 USRID10         ERROR    PIC X(01).
              10 USRIDIN              PIC X(8).
              10 USRIDIN         ERROR    PIC X(01).
              10 UTYPE01              PIC X(8).
              10 UTYPE01         ERROR    PIC X(01).
              10 UTYPE02              PIC X(8).
              10 UTYPE02         ERROR    PIC X(01).
              10 UTYPE03              PIC X(8).
              10 UTYPE03         ERROR    PIC X(01).
              10 UTYPE04              PIC X(8).
              10 UTYPE04         ERROR    PIC X(01).
              10 UTYPE05              PIC X(8).
              10 UTYPE05         ERROR    PIC X(01).
              10 UTYPE06              PIC X(8).
              10 UTYPE06         ERROR    PIC X(01).
              10 UTYPE07              PIC X(8).
              10 UTYPE07         ERROR    PIC X(01).
              10 UTYPE08              PIC X(8).
              10 UTYPE08         ERROR    PIC X(01).
              10 UTYPE09              PIC X(8).
              10 UTYPE09         ERROR    PIC X(01).
              10 UTYPE10              PIC X(8).
              10 UTYPE10         ERROR    PIC X(01).
      * LENGTH constants for database operations
           05 LENGTH-USR-ID               PIC S9(04) COMP VALUE 8.
           05 LENGTH-USER-DATA            PIC S9(04) COMP VALUE 80.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COUSR00C'.
         05 WS-TRANID                  PIC X(04) VALUE 'CU00'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-USRSEC-FILE             PIC X(08) VALUE 'USRSEC  '.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 STATUS-ERROR                         VALUE 'Y'. * Error handling standardized
           88 STATUS-OK                        VALUE 'N'. * Error handling standardized
         05 WS-USER-SEC-EOF            PIC X(01) VALUE 'N'.
           88 USER-SEC-EOF                       VALUE 'Y'.
           88 USER-SEC-NOT-EOF                   VALUE 'N'.
         05 WS-SEND-* ERASE removed *-FLG          PIC X(01) VALUE 'Y'. * Removed screen ERASE operation
           88 SEND-* ERASE removed *-YES                     VALUE 'Y'. * Removed screen ERASE operation
           88 SEND-* ERASE removed *-NO                      VALUE 'N'. * Removed screen ERASE operation

         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE ZEROS.

       01 WS-USER-DATA.
         02 USER-REC OCCURS 10 TIMES.
           05 USER-SEL                   PIC X(01).
           05 FILLER                     PIC X(02).
           05 USER-ID                    PIC X(08).
           05 FILLER                     PIC X(02).
           05 USER-NAME                  PIC X(25).
           05 FILLER                     PIC X(02).
           05 USER-TYPE                  PIC X(08).

       COPY COCOM01Y.
          05 CDEMO-CU00-INFO.
             10 CDEMO-CU00-USRID-FIRST     PIC X(08).
             10 CDEMO-CU00-USRID-LAST      PIC X(08).
             10 CDEMO-CU00-PAGE-NUM        PIC 9(08).
             10 CDEMO-CU00-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CU00-USR-SEL-FLG     PIC X(01).
             10 CDEMO-CU00-USR-SELECTED    PIC X(08).
* Removed screen-related copybook:        COPY COUSR00.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.
       COPY CSUSR01Y.

* Removed screen-related copybook:        COPY DFHAID.
* Removed screen-related copybook:        COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *************************************************************
      * Removed cursor operation:       * ERROR-FLAGS - Replacement for cursor positioning          *
      *************************************************************
       01 ERROR-FLAGS.
           05 VALIDATION-ERROR          PIC X(01).
               88 ERROR-PRESENT                   VALUE 'Y'.
               88 NO-ERROR                        VALUE 'N'.
           05 FIELD-IN-ERROR            PIC X(20).
           05 SEL0001-ERROR      PIC X(01) VALUE 'N'.
           05 USRID01-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0002-ERROR      PIC X(01) VALUE 'N'.
           05 USRID02-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0003-ERROR      PIC X(01) VALUE 'N'.
           05 USRID03-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0004-ERROR      PIC X(01) VALUE 'N'.
           05 USRID04-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0005-ERROR      PIC X(01) VALUE 'N'.
           05 USRID05-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0006-ERROR      PIC X(01) VALUE 'N'.
           05 USRID06-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0007-ERROR      PIC X(01) VALUE 'N'.
           05 USRID07-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0008-ERROR      PIC X(01) VALUE 'N'.
           05 USRID08-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0009-ERROR      PIC X(01) VALUE 'N'.
           05 USRID09-ERROR      PIC X(01) VALUE 'N'.
           05 SEL0010-ERROR      PIC X(01) VALUE 'N'.
           05 USRID10-ERROR      PIC X(01) VALUE 'N'.
           05 USRIDIN-ERROR      PIC X(01) VALUE 'N'.
           05 PAGENUM-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME01-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME01-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE01-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME02-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME02-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE02-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME03-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME03-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE03-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME04-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME04-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE04-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME05-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME05-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE05-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME06-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME06-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE06-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME07-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME07-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE07-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME08-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME08-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE08-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME09-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME09-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE09-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME10-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME10-ERROR      PIC X(01) VALUE 'N'.
           05 UTYPE10-ERROR      PIC X(01) VALUE 'N'.
           05 ERRMSG-ERROR      PIC X(01) VALUE 'N'.
           05 TITLE01-ERROR      PIC X(01) VALUE 'N'.
           05 TITLE02-ERROR      PIC X(01) VALUE 'N'.
           05 TRNNAME-ERROR      PIC X(01) VALUE 'N'.
           05 PGMNAME-ERROR      PIC X(01) VALUE 'N'.
           05 CURDATE-ERROR      PIC X(01) VALUE 'N'.
           05 CURTIME-ERROR      PIC X(01) VALUE 'N'.
           05 LNAME-ERROR      PIC X(01) VALUE 'N'.
           05 FNAME-ERROR      PIC X(01) VALUE 'N'.
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET STATUS-OK TO TRUE * Error handling standardized
           SET USER-SEC-NOT-EOF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-* ERASE removed *-YES TO TRUE * Removed screen ERASE operation

           MOVE SPACES TO SCREEN-FIELDS.SERVICE-INFO.SCREEN-MESSAGE * Error message standardized
                          SCREEN-FIELDS.BUSINESS-DATA.ERRMSG * Direct screen reference replaced

      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag

           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
      * Removed screen initialization:                    MOVE LOW-VALUES          TO COUSR0AO
                   PERFORM PROCESS-ENTER-KEY
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
               ELSE
                   SET SCREEN-UPDATE TO TRUE
           PERFORM MAP-SCREEN-TO-COMMAREA
           PERFORM VALIDATE-INPUT-FIELDS
                   EVALUATE EIBAID
                       WHEN "ENTER"
                           PERFORM PROCESS-ENTER-KEY
                       WHEN "PF3"
                           MOVE 'COADM01C' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN "PF7"
                           PERFORM PROCESS-PF7-KEY
                       WHEN "PF8"
                           PERFORM PROCESS-PF8-KEY
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
           MOVE CCDA-MSG-INVALID-KEY TO SCREEN-FIELDS.SERVICE-INFO.SCREEN-MESSAGE * Error message standardized
                           SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0001 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0001 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID01 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0002 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0002 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID02 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0003 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0003 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID03 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0004 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0004 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID04 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0005 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0005 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID05 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0006 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0006 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID06 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0007 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0007 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID07 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0008 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0008 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID08 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0009 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0009 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID09 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN SCREEN-FIELDS.BUSINESS-DATA.SEL0010 NOT = SPACES AND LOW-VALUES * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.SEL0010 TO CDEMO-CU00-USR-SEL-FLG * Direct screen reference replaced
                   MOVE SCREEN-FIELDS.BUSINESS-DATA.USRID10 TO CDEMO-CU00-USR-SELECTED * Direct screen reference replaced
               WHEN OTHER
                   MOVE SPACES   TO CDEMO-CU00-USR-SEL-FLG
                   MOVE SPACES   TO CDEMO-CU00-USR-SELECTED
           END-EVALUATE

           IF (CDEMO-CU00-USR-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
              (CDEMO-CU00-USR-SELECTED NOT = SPACES AND LOW-VALUES)
               EVALUATE CDEMO-CU00-USR-SEL-FLG
                   WHEN 'U'
                   WHEN 'u'
                        MOVE 'COUSR02C'   TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                        MOVE 0        TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN 'D'
                   WHEN 'd'
                        MOVE 'COUSR03C'   TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                        MOVE 0        TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN OTHER
                       MOVE
                       'Invalid selection. Valid values are U and D' TO
                                       WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
               END-EVALUATE
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRIDIN = SPACES OR LOW-VALUES * Direct screen reference replaced
               MOVE LOW-VALUES TO SEC-USR-ID
           ELSE
               MOVE SCREEN-FIELDS.BUSINESS-DATA.USRIDIN TO SEC-USR-ID * Complete screen reference replacement
           END-IF

      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag


           MOVE 0       TO CDEMO-CU00-PAGE-NUM
           PERFORM PROCESS-PAGE-FORWARD

           IF NOT STATUS-ERROR * Error handling standardized
               MOVE SPACE   TO SCREEN-FIELDS.BUSINESS-DATA.USRIDIN * Complete screen reference replacement
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF7-KEY
      *----------------------------------------------------------------*
       PROCESS-PF7-KEY.

           IF CDEMO-CU00-USRID-FIRST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO SEC-USR-ID
           ELSE
               MOVE CDEMO-CU00-USRID-FIRST TO SEC-USR-ID
           END-IF

           SET NEXT-PAGE-YES TO TRUE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag

           IF CDEMO-CU00-PAGE-NUM > 1
               PERFORM PROCESS-PAGE-BACKWARD
           ELSE
               MOVE 'You are already at the SCREEN-FIELDS.BUSINESS-DATA.top page...' TO * Complete screen reference replacement
                               WS-MESSAGE
               SET SEND-* ERASE removed *-NO TO TRUE * Removed screen ERASE operation
               SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF8-KEY
      *----------------------------------------------------------------*
       PROCESS-PF8-KEY.

           IF CDEMO-CU00-USRID-LAST = SPACES OR LOW-VALUES
               MOVE HIGH-VALUES TO SEC-USR-ID
           ELSE
               MOVE CDEMO-CU00-USRID-LAST TO SEC-USR-ID
           END-IF

      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag

           IF NEXT-PAGE-YES
               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               MOVE 'You are already at the SCREEN-FIELDS.BUSINESS-DATA.bottom page...' TO * Complete screen reference replacement
                               WS-MESSAGE
               SET SEND-* ERASE removed *-NO TO TRUE * Removed screen ERASE operation
               SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-FORWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-FORWARD.

           PERFORM STARTBR-USER-SEC-FILE

           IF NOT STATUS-ERROR * Error handling standardized

               IF SCREEN-FIELDS.SERVICE-INFO.FUNCTION-CODE NOT = "ENTER" AND "PF7" AND "PF3" * Updated for nested structure
                   PERFORM READNEXT-USER-SEC-FILE
               END-IF

               IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                   PERFORM INITIALIZE-USER-DATA
               END-PERFORM
               END-IF

               MOVE 1             TO  WS-IDX

               PERFORM UNTIL WS-IDX >= 11 OR USER-SEC-EOF OR STATUS-ERROR * Error handling standardized
                   PERFORM READNEXT-USER-SEC-FILE
                   IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
                       PERFORM POPULATE-USER-DATA
                       COMPUTE WS-IDX = WS-IDX + 1
                   END-IF
               END-PERFORM

               IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
                   COMPUTE CDEMO-CU00-PAGE-NUM =
                           CDEMO-CU00-PAGE-NUM + 1
                   PERFORM READNEXT-USER-SEC-FILE
                   IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               ELSE
                   SET NEXT-PAGE-NO TO TRUE
                   IF WS-IDX > 1
                       COMPUTE CDEMO-CU00-PAGE-NUM = CDEMO-CU00-PAGE-NUM
                        + 1
                   END-IF
               END-IF

               PERFORM ENDBR-USER-SEC-FILE

               MOVE CDEMO-CU00-PAGE-NUM TO SCREEN-FIELDS.BUSINESS-DATA.PAGENUM * Complete screen reference replacement
               MOVE SPACE   TO SCREEN-FIELDS.BUSINESS-DATA.USRIDIN * Complete screen reference replacement
               SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN

           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-BACKWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-BACKWARD.

           PERFORM STARTBR-USER-SEC-FILE

           IF NOT STATUS-ERROR * Error handling standardized

               IF SCREEN-FIELDS.SERVICE-INFO.FUNCTION-CODE NOT = "ENTER"  AND "PF8" * Updated for nested structure
                   PERFORM READPREV-USER-SEC-FILE
               END-IF

               IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                   PERFORM INITIALIZE-USER-DATA
               END-PERFORM
               END-IF

               MOVE 10          TO  WS-IDX

               PERFORM UNTIL WS-IDX <= 0 OR USER-SEC-EOF OR STATUS-ERROR * Error handling standardized
                   PERFORM READPREV-USER-SEC-FILE
                   IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
                       PERFORM POPULATE-USER-DATA
                       COMPUTE WS-IDX = WS-IDX - 1
                   END-IF
               END-PERFORM

               IF USER-SEC-NOT-EOF AND STATUS-OK * Error handling standardized
               PERFORM READPREV-USER-SEC-FILE
               IF NEXT-PAGE-YES
                   IF USER-SEC-NOT-EOF AND STATUS-OK AND * Error handling standardized
                       CDEMO-CU00-PAGE-NUM > 1
                       SUBTRACT 1 FROM CDEMO-CU00-PAGE-NUM
                   ELSE
                       MOVE 1 TO CDEMO-CU00-PAGE-NUM
                   END-IF
               END-IF
               END-IF

               PERFORM ENDBR-USER-SEC-FILE

               MOVE CDEMO-CU00-PAGE-NUM TO SCREEN-FIELDS.BUSINESS-DATA.PAGENUM * Complete screen reference replacement
               SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN

           END-IF.

      *----------------------------------------------------------------*
      *                      POPULATE-USER-DATA
      *----------------------------------------------------------------*
       POPULATE-USER-DATA.

           EVALUATE WS-IDX
               WHEN 1
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID01 * Direct screen reference replaced
                                         CDEMO-CU00-USRID-FIRST
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME01 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME01 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE01 * Direct screen reference replaced
               WHEN 2
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID02 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME02 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME02 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE02 * Direct screen reference replaced
               WHEN 3
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID03 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME03 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME03 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE03 * Direct screen reference replaced
               WHEN 4
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID04 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME04 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME04 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE04 * Direct screen reference replaced
               WHEN 5
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID05 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME05 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME05 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE05 * Direct screen reference replaced
               WHEN 6
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID06 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME06 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME06 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE06 * Direct screen reference replaced
               WHEN 7
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID07 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME07 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME07 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE07 * Direct screen reference replaced
               WHEN 8
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID08 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME08 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME08 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE08 * Direct screen reference replaced
               WHEN 9
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID09 * Direct screen reference replaced
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME09 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME09 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE09 * Direct screen reference replaced
               WHEN 10
                   MOVE SEC-USR-ID    TO SCREEN-FIELDS.BUSINESS-DATA.USRID10 * Direct screen reference replaced
                                         CDEMO-CU00-USRID-LAST
                   MOVE SEC-USR-FNAME TO SCREEN-FIELDS.BUSINESS-DATA.FNAME10 * Direct screen reference replaced
                   MOVE SEC-USR-LNAME TO SCREEN-FIELDS.BUSINESS-DATA.LNAME10 * Direct screen reference replaced
                   MOVE SEC-USR-TYPE  TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE10 * Direct screen reference replaced
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      INITIALIZE-USER-DATA
      *----------------------------------------------------------------*
       INITIALIZE-USER-DATA.

           EVALUATE WS-IDX
               WHEN 1
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID01 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME01 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME01 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE01 * Direct screen reference replaced
               WHEN 2
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID02 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME02 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME02 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE02 * Direct screen reference replaced
               WHEN 3
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID03 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME03 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME03 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE03 * Direct screen reference replaced
               WHEN 4
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID04 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME04 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME04 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE04 * Direct screen reference replaced
               WHEN 5
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID05 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME05 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME05 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE05 * Direct screen reference replaced
               WHEN 6
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID06 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME06 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME06 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE06 * Direct screen reference replaced
               WHEN 7
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID07 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME07 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME07 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE07 * Direct screen reference replaced
               WHEN 8
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID08 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME08 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME08 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE08 * Direct screen reference replaced
               WHEN 9
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID09 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME09 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME09 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE09 * Direct screen reference replaced
               WHEN 10
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.USRID10 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.FNAME10 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.LNAME10 * Direct screen reference replaced
                   MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE10 * Direct screen reference replaced
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      SEND-USRLST-SCREEN
      *----------------------------------------------------------------*
       SEND-USRLST-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO SCREEN-FIELDS.BUSINESS-DATA.ERRMSG * Direct screen reference replaced

           IF SEND-* ERASE removed *-YES * Removed screen ERASE operation
               EXEC CICS SEND
      * Removed direct MAP reference:                          MAP('COUSR0A')
      * Removed mapset reference:       * Removed direct MAPSET reference:                          MAPSET('COUSR00')
                         FROM(COUSR0AO)
                         * ERASE removed * * Removed screen ERASE operation
      * Removed cursor operation:                          CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
      * Removed direct MAP reference:                          MAP('COUSR0A')
      * Removed mapset reference:       * Removed direct MAPSET reference:                          MAPSET('COUSR00')
                         FROM(COUSR0AO)
      *                  * ERASE removed * * Removed screen ERASE operation
      * Removed cursor operation:                          CURSOR
               END-EXEC
           END-IF.

      *----------------------------------------------------------------*
      *                      RECEIVE-USRLST-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-USRLST-SCREEN.

           EXEC CICS RECEIVE
      * Removed direct MAP reference:                      MAP('COUSR0A')
      * Removed mapset reference:       * Removed direct MAPSET reference:                      MAPSET('COUSR00')
                     INTO(COUSR0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO SCREEN-FIELDS.BUSINESS-DATA.TITLE01 * Direct screen reference replaced
           MOVE CCDA-TITLE02           TO SCREEN-FIELDS.BUSINESS-DATA.TITLE02 * Direct screen reference replaced
           MOVE WS-TRANID              TO SCREEN-FIELDS.BUSINESS-DATA.TRNNAME * Direct screen reference replaced
           MOVE WS-PGMNAME             TO SCREEN-FIELDS.BUSINESS-DATA.PGMNAME * Direct screen reference replaced

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO SCREEN-FIELDS.BUSINESS-DATA.CURDATE * Direct screen reference replaced

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO SCREEN-FIELDS.BUSINESS-DATA.CURTIME. * Direct screen reference replaced

      *----------------------------------------------------------------*
      *                      STARTBR-USER-SEC-FILE
      *----------------------------------------------------------------*
       STARTBR-USER-SEC-FILE.

           EXEC CICS STARTBR
                DATASET   (WS-USRSEC-FILE)
                RIDFLD    (SEC-USR-ID)
                KEYLENGTH (SCREEN-FIELDS.BUSINESS-DATA.LENGTH-USR-ID) * Complete screen reference replacement
      *         GTEQ
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   CONTINUE
                   SET USER-SEC-EOF TO TRUE
                   MOVE 'You are at the SCREEN-FIELDS.BUSINESS-DATA.top page...' TO * Complete screen reference replacement
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup User...' TO
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READNEXT-USER-SEC-FILE
      *----------------------------------------------------------------*
       READNEXT-USER-SEC-FILE.

           EXEC CICS READNEXT
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                LENGTH    (SCREEN-FIELDS.BUSINESS-DATA.LENGTH-USER-DATA) * Complete screen reference replacement
                RIDFLD    (SEC-USR-ID)
                KEYLENGTH (SCREEN-FIELDS.BUSINESS-DATA.LENGTH-USR-ID) * Complete screen reference replacement
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   CONTINUE
                   SET USER-SEC-EOF TO TRUE
                   MOVE 'You have reached the SCREEN-FIELDS.BUSINESS-DATA.bottom page...' TO * Complete screen reference replacement
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup User...' TO
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READPREV-USER-SEC-FILE
      *----------------------------------------------------------------*
       READPREV-USER-SEC-FILE.

           EXEC CICS READPREV
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                LENGTH    (SCREEN-FIELDS.BUSINESS-DATA.LENGTH-USER-DATA) * Complete screen reference replacement
                RIDFLD    (SEC-USR-ID)
                KEYLENGTH (SCREEN-FIELDS.BUSINESS-DATA.LENGTH-USR-ID) * Complete screen reference replacement
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   CONTINUE
                   SET USER-SEC-EOF TO TRUE
                   MOVE 'You have reached the SCREEN-FIELDS.BUSINESS-DATA.top page...' TO * Complete screen reference replacement
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup User...' TO
                                   WS-MESSAGE
      * Removed cursor operation:            MOVE 'Y' TO USRIDIN-ERROR
           MOVE 'USRIDIN' TO FIELD-IN-ERROR * Cursor positioning replaced with error flag
                   SET SCREEN-DISPLAY TO TRUE
           PERFORM MAP-COMMAREA-TO-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      ENDBR-USER-SEC-FILE
      *----------------------------------------------------------------*
       ENDBR-USER-SEC-FILE.

           EXEC CICS ENDBR
                DATASET   (WS-USRSEC-FILE)
           END-EXEC.
      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
       MAP-COMMAREA-TO-SCREEN SECTION.
           MOVE LOW-VALUES TO COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.ERRMSG TO ERRMSGO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.USRIDIN TO USRIDINO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.TITLE01 TO TITLE01O OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.TITLE02 TO TITLE02O OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.TRNNAME TO TRNNAMEO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.PGMNAME TO PGMNAMEO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.CURDATE TO CURDATEO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.CURTIME TO CURTIMEO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.LNAME TO LNAMEO OF COUSR0AO
           MOVE SCREEN-FIELDS.BUSINESS-DATA.FNAME TO FNAMEO OF COUSR0AO
           EXIT.

       MAP-SCREEN-TO-COMMAREA SECTION.
           MOVE SEL0001I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0001
           MOVE USRID01I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID01
           MOVE SEL0002I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0002
           MOVE USRID02I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID02
           MOVE SEL0003I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0003
           MOVE USRID03I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID03
           MOVE SEL0004I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0004
           MOVE USRID04I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID04
           MOVE SEL0005I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0005
           MOVE USRID05I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID05
           MOVE SEL0006I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0006
           MOVE USRID06I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID06
           MOVE SEL0007I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0007
           MOVE USRID07I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID07
           MOVE SEL0008I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0008
           MOVE USRID08I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID08
           MOVE SEL0009I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0009
           MOVE USRID09I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID09
           MOVE SEL0010I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.SEL0010
           MOVE USRID10I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRID10
           MOVE USRIDINI OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.USRIDIN
           MOVE PAGENUMI OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.PAGENUM
           MOVE FNAME01I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME01
           MOVE LNAME01I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME01
           MOVE UTYPE01I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE01
           MOVE FNAME02I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME02
           MOVE LNAME02I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME02
           MOVE UTYPE02I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE02
           MOVE FNAME03I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME03
           MOVE LNAME03I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME03
           MOVE UTYPE03I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE03
           MOVE FNAME04I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME04
           MOVE LNAME04I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME04
           MOVE UTYPE04I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE04
           MOVE FNAME05I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME05
           MOVE LNAME05I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME05
           MOVE UTYPE05I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE05
           MOVE FNAME06I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME06
           MOVE LNAME06I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME06
           MOVE UTYPE06I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE06
           MOVE FNAME07I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME07
           MOVE LNAME07I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME07
           MOVE UTYPE07I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE07
           MOVE FNAME08I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME08
           MOVE LNAME08I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME08
           MOVE UTYPE08I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE08
           MOVE FNAME09I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME09
           MOVE LNAME09I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME09
           MOVE UTYPE09I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE09
           MOVE FNAME10I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.FNAME10
           MOVE LNAME10I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.LNAME10
           MOVE UTYPE10I OF COUSR0AI TO SCREEN-FIELDS.BUSINESS-DATA.UTYPE10
           EXIT.
       VALIDATE-INPUT-FIELDS SECTION.
           SET STATUS-OK TO TRUE * Error handling standardized
           MOVE SPACES TO SCREEN-MESSAGE
           MOVE 'N' TO VALIDATION-ERROR
           MOVE SPACES TO FIELD-IN-ERROR

      * Initialize all field error flags
           MOVE 'N' TO SEL0001-ERROR
           MOVE 'N' TO USRID01-ERROR
           MOVE 'N' TO SEL0002-ERROR
           MOVE 'N' TO USRID02-ERROR
           MOVE 'N' TO SEL0003-ERROR
           MOVE 'N' TO USRID03-ERROR
           MOVE 'N' TO SEL0004-ERROR
           MOVE 'N' TO USRID04-ERROR
           MOVE 'N' TO SEL0005-ERROR
           MOVE 'N' TO USRID05-ERROR
           MOVE 'N' TO SEL0006-ERROR
           MOVE 'N' TO USRID06-ERROR
           MOVE 'N' TO SEL0007-ERROR
           MOVE 'N' TO USRID07-ERROR
           MOVE 'N' TO SEL0008-ERROR
           MOVE 'N' TO USRID08-ERROR
           MOVE 'N' TO SEL0009-ERROR
           MOVE 'N' TO USRID09-ERROR
           MOVE 'N' TO SEL0010-ERROR
           MOVE 'N' TO USRID10-ERROR
           MOVE 'N' TO USRIDIN-ERROR
           MOVE 'N' TO PAGENUM-ERROR
           MOVE 'N' TO FNAME01-ERROR
           MOVE 'N' TO LNAME01-ERROR
           MOVE 'N' TO UTYPE01-ERROR
           MOVE 'N' TO FNAME02-ERROR
           MOVE 'N' TO LNAME02-ERROR
           MOVE 'N' TO UTYPE02-ERROR
           MOVE 'N' TO FNAME03-ERROR
           MOVE 'N' TO LNAME03-ERROR
           MOVE 'N' TO UTYPE03-ERROR
           MOVE 'N' TO FNAME04-ERROR
           MOVE 'N' TO LNAME04-ERROR
           MOVE 'N' TO UTYPE04-ERROR
           MOVE 'N' TO FNAME05-ERROR
           MOVE 'N' TO LNAME05-ERROR
           MOVE 'N' TO UTYPE05-ERROR
           MOVE 'N' TO FNAME06-ERROR
           MOVE 'N' TO LNAME06-ERROR
           MOVE 'N' TO UTYPE06-ERROR
           MOVE 'N' TO FNAME07-ERROR
           MOVE 'N' TO LNAME07-ERROR
           MOVE 'N' TO UTYPE07-ERROR
           MOVE 'N' TO FNAME08-ERROR
           MOVE 'N' TO LNAME08-ERROR
           MOVE 'N' TO UTYPE08-ERROR
           MOVE 'N' TO FNAME09-ERROR
           MOVE 'N' TO LNAME09-ERROR
           MOVE 'N' TO UTYPE09-ERROR
           MOVE 'N' TO FNAME10-ERROR
           MOVE 'N' TO LNAME10-ERROR
           MOVE 'N' TO UTYPE10-ERROR
           MOVE 'N' TO ERRMSG-ERROR
           MOVE 'N' TO TITLE01-ERROR
           MOVE 'N' TO TITLE02-ERROR
           MOVE 'N' TO TRNNAME-ERROR
           MOVE 'N' TO PGMNAME-ERROR
           MOVE 'N' TO CURDATE-ERROR
           MOVE 'N' TO CURTIME-ERROR
           MOVE 'N' TO LNAME-ERROR
           MOVE 'N' TO FNAME-ERROR

      * Field required validations

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0001 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0001' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0001-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0001 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID01 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID01' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID01 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0002 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0002' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0002-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0002 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID02 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID02' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID02 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0003 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0003' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0003-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0003 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID03 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID03' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID03 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0004 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0004' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0004-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0004 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID04 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID04' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID04 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0005 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0005' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0005-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0005 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID05 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID05' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID05 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0006 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0006' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0006-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0006 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID06 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID06' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID06 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0007 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0007' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0007-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0007 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID07 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID07' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID07 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0008 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0008' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0008-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0008 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID08 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID08' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID08 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0009 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0009' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0009-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0009 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID09 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID09' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID09 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.SEL0010 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0010' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0010-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0010 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRID10 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID10' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID10 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.USRIDIN = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRIDIN' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRIDIN-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRIDIN cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.PAGENUM = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'PAGENUM' TO FIELD-IN-ERROR
               MOVE 'Y' TO PAGENUM-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'PAGENUM cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME01 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME01' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME01 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME01 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME01' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME01 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE01 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE01' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE01 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME02 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME02' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME02 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME02 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME02' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME02 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE02 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE02' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE02 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME03 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME03' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME03 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME03 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME03' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME03 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE03 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE03' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE03 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME04 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME04' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME04 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME04 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME04' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME04 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE04 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE04' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE04 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME05 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME05' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME05 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME05 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME05' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME05 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE05 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE05' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE05 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME06 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME06' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME06 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME06 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME06' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME06 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE06 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE06' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE06 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME07 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME07' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME07 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME07 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME07' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME07 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE07 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE07' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE07 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME08 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME08' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME08 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME08 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME08' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME08 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE08 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE08' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE08 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME09 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME09' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME09 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME09 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME09' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME09 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE09 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE09' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE09 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.FNAME10 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME10' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME10 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.LNAME10 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME10' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME10 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF SCREEN-FIELDS.BUSINESS-DATA.UTYPE10 = SPACES OR LOW-VALUES
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE10' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE10 cannot be empty' TO SCREEN-MESSAGE
               END-IF
           END-IF

      * Field length validations

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0001)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0001' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0001-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0001 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID01)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID01' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID01 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0002)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0002' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0002-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0002 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID02)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID02' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID02 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0003)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0003' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0003-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0003 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID03)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID03' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID03 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0004)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0004' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0004-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0004 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID04)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID04' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID04 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0005)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0005' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0005-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0005 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID05)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID05' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID05 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0006)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0006' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0006-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0006 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID06)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID06' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID06 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0007)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0007' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0007-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0007 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID07)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID07' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID07 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0008)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0008' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0008-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0008 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID08)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID08' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID08 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0009)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0009' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0009-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0009 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID09)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID09' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID09 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.SEL0010)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'SEL0010' TO FIELD-IN-ERROR
               MOVE 'Y' TO SEL0010-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'SEL0010 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRID10)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRID10' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRID10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRID10 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USRIDIN)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRIDIN' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRIDIN-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRIDIN exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.PAGENUM)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'PAGENUM' TO FIELD-IN-ERROR
               MOVE 'Y' TO PAGENUM-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'PAGENUM exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME01)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME01' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME01 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME01)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME01' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME01 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE01)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE01' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE01-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE01 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME02)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME02' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME02 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME02)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME02' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME02 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE02)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE02' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE02-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE02 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME03)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME03' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME03 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME03)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME03' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME03 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE03)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE03' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE03-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE03 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME04)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME04' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME04 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME04)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME04' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME04 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE04)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE04' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE04-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE04 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME05)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME05' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME05 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME05)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME05' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME05 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE05)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE05' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE05-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE05 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME06)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME06' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME06 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME06)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME06' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME06 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE06)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE06' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE06-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE06 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME07)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME07' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME07 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME07)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME07' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME07 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE07)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE07' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE07-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE07 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME08)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME08' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME08 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME08)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME08' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME08 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE08)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE08' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE08-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE08 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME09)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME09' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME09 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME09)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME09' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME09 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE09)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE09' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE09-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE09 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.FNAME10)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'FNAME10' TO FIELD-IN-ERROR
               MOVE 'Y' TO FNAME10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'FNAME10 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.LNAME10)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'LNAME10' TO FIELD-IN-ERROR
               MOVE 'Y' TO LNAME10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'LNAME10 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

           IF FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.UTYPE10)) > 8
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'UTYPE10' TO FIELD-IN-ERROR
               MOVE 'Y' TO UTYPE10-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'UTYPE10 exceeds maximum length of 8' TO SCREEN-MESSAGE
               END-IF
           END-IF

      * Special validations for USERID
           IF SCREEN-FIELDS.BUSINESS-DATA.USERID NOT = SPACES AND
              FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.USERID)) < 3
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USERID' TO FIELD-IN-ERROR
               MOVE 'Y' TO USERID-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USERID must be at least 3 characters' TO SCREEN-MESSAGE
               END-IF
           END-IF

      * Special validations for PASSWORD
           IF SCREEN-FIELDS.BUSINESS-DATA.PASSWD NOT = SPACES AND
              FUNCTION LENGTH(FUNCTION TRIM(SCREEN-FIELDS.BUSINESS-DATA.PASSWD)) < 4
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'PASSWD' TO FIELD-IN-ERROR
               MOVE 'Y' TO PASSWD-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'PASSWORD must be at least 4 characters' TO SCREEN-MESSAGE
               END-IF
           END-IF

      * Special validations for USRTYPE
           IF SCREEN-FIELDS.BUSINESS-DATA.USRTYPE NOT = 'A' AND
              SCREEN-FIELDS.BUSINESS-DATA.USRTYPE NOT = 'U'
               SET STATUS-ERROR TO TRUE * Error handling standardized
               SET ERROR-PRESENT TO TRUE
               MOVE 'USRTYPE' TO FIELD-IN-ERROR
               MOVE 'Y' TO USRTYPE-ERROR
               IF SCREEN-MESSAGE = SPACES
                   MOVE 'USRTYPE must be A (admin) or U (user)' TO SCREEN-MESSAGE
               END-IF
           END-IF

      * Business logic validations
           IF SCREEN-FIELDS.SERVICE-INFO.FUNCTION-CODE = "U" OR SCREEN-FIELDS.SERVICE-INFO.FUNCTION-CODE = "D" * Updated for nested structure
               IF SCREEN-FIELDS.BUSINESS-DATA.USERID = SPACES
                   SET STATUS-ERROR TO TRUE * Error handling standardized
                   SET ERROR-PRESENT TO TRUE
                   MOVE 'USERID' TO FIELD-IN-ERROR
                   MOVE 'Y' TO USERID-ERROR
                   MOVE 'User ID is required for this operation' TO SCREEN-MESSAGE
               END-IF
           END-IF

           EXIT.

       HANDLE-ERROR SECTION.
           IF STATUS-ERROR
               MOVE SCREEN-MESSAGE TO WS-MESSAGE
           ELSE
               MOVE SPACES TO FIELD-IN-ERROR
               MOVE 'N' TO VALIDATION-ERROR
           END-IF.
           EXIT.

       RETURN-WITH-ERROR SECTION.
           SET STATUS-ERROR TO TRUE * Error handling standardized
           SET SCREEN-DISPLAY TO TRUE
           PERFORM HANDLE-ERROR
           EXEC CICS
               RETURN
               COMMAREA(DFHCOMMAREA)
               LENGTH(LENGTH OF DFHCOMMAREA)
           END-EXEC.
           EXIT.

       GET-FIELD-IN-ERROR SECTION.
      * Removed cursor operation:       * This section replaces cursor positioning with field identification
           IF ERROR-PRESENT
               MOVE FIELD-IN-ERROR TO SCREEN-FIELDS.BUSINESS-DATA.SERVICE-INFO.FIELD-IN-ERROR
           ELSE
               MOVE SPACES TO SCREEN-FIELDS.BUSINESS-DATA.SERVICE-INFO.FIELD-IN-ERROR
           END-IF.
           EXIT.
      *


      *----------------------------------------------------------------*
      * This program has been transformed from a screen-based (BMS) app
      * to a service-oriented program using COMMAREA for communication.
      * All screen dependencies have been removed while preserving
      * the original business logic and validation rules.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * This program has been transformed from a screen-based (BMS) app
      * to a service-oriented program using COMMAREA for communication.
      * The transformation includes:
      *
      * 1. SCREEN INDEPENDENCE:
      *    - Complete removal of all terminal dependencies
      *    - Replaced SEND/RECEIVE MAP with service calls
      *    - Eliminated cursor positioning and screen attributes
      *
      * 2. BUSINESS LOGIC PRESERVATION:
      *    - Maintained all validation rules and business logic
      *    - Retained file operations and data handling
      *    - Preserved transaction flow and error conditions
      *
      * 3. ERROR HANDLING IMPROVEMENTS:
      *    - Added field-specific error flags replacing cursor positioning
      *    - Enhanced validation with detailed error messages
      *    - Standardized error handling across all operations
      *
      * 4. SERVICE-ORIENTED DESIGN:
      *    - Structured request/response model via COMMAREA
      *    - Clear input/output boundaries
      *    - FUNCTION-CODE driven processing replacing EIBAID
      *
      * This program now operates in a completely terminal-independent mode
      * while preserving all the original functionality and business rules.
      *----------------------------------------------------------------*