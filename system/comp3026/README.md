# COMP3026

## Task: Web-based Banking System

**deadline: 11 November (Monday) 2019**

You are to design and implement a Web-based Banking System by using the Java Applet and Servlet technology. 

The system consists of three components: a Java applet as the client, a Java servlet and a DBMS system. The servlet receives requests from the applet, accesses the DBMS system via JDBC, and sends replies back to the applet. The DBMS is a MySql system which installed in the server you create. 

The client program is basically a graphical interface. It displays a list of buttons for users to select operations. There are four operations: 

- accnt_num = open_account(user_name); 
- delete_account(accnt_num); 
- bal = balance(accnt_num);
- transfer(source_accnt, dest_accnt, amnt).

There are two types of users of the system: customers and bank-clerks. Only bank clerks have authority to open or delete accounts (customers can only use “balance” or “transfer” operations). You need to use programming security method for authentication and access control. The security measure should have session tracking ability for logged in users. That is, a user will not be asked to login again if he/she already logged in during the same session. There is no need to use “SSL”.

For unspecified design and implementation details (particularly data structures, interface design), make your own reasonable decisions. You need to demonstrate your system on the due date at demonstration time. 
You must hand in the following documents at your demonstration time:
1.	a copy of design document (2 pages maximum), stating the system structure, design, and implementation. It should also include your names (or student IDs) and the URL of your system;
2.	A hard copy (no CD) of full program list (client and server).
Your assignment will be assessed on the following criteria:
    - documentation (20%)
    - interface design (20%)
    - implementation (source code) and programming styles (60%)

A system which is not working will have no more than 50% of the total mark; and a system which works properly (with minimum required functionality) will have at least 50% of the total mark. The system design could base on the programming language you prefer.


## Attention: 

- `wavelet v0.2.0` has severe bugs, whose syncing function always not works, please use `v0.1.1`.
- In angular, `import 'zone.js/dist/zone'`(in `polyfills.ts`) must be import after `import 'app.module'`
- There are zone.js bugs when using building production, please use build dev.