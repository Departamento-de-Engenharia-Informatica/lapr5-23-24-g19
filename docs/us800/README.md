# US800

As a system administrator, I want the Minimum Business Continuity Objective (MBCO) to be defined to propose to stakeholders.


## Introduction

The MBCO (Minimum Business Continuity Objective)

## Resolution

MBCO (Minimum Business Continuity Objective):

1. Databases must not fail because it can cause a critical loss of data to the system and users.
   1. Specify the maximum acceptable downtime for databases.
   For example, "Databases must not fail for more than X hours".

2. The authentication system (although not yet implemented in this sprint)
    must not fail because it can result in an unwanted leakage and alteration of information.
   1. Specify the maximum acceptable downtime for the authentication system.
   For example, "Authentication system should have a recovery time objective of Y minutes."

3. Include regular testing of the business continuity plan.
    This can involve simulated failures and recovery exercises to ensure the effectiveness of the proposed strategies

4. Detail the potential impact on end-users for each critical component.
   This could include aspects such as:
   1. Data access;
   2. System functionality;
   3. Service availability.

5. Define a mechanism for continuous monitoring of critical systems and establish alerts for potential issues.
    This proactive approach can help in identifying and addressing problems before they escalate.

6. Outline a communication plan to keep stakeholders informed during an incident.
   This could include regular updates on the status of recovery efforts and expected resolution times.

7. The MDR, SPA, and Planning Modules are also critical modules,
    and therefore, we have implemented a recovery strategy
    for the modules through regular backups, which we will discuss in the next user story.
