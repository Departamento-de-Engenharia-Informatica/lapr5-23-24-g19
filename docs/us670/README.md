# Evaluating Risks in the Proposed Solution

To assess the risks associated with the proposed solution, it is crucial to have a clear understanding
of the risk calculation process.

## Risk Calculation Factors

When calculating risk, two key variables come into play:

-   **Impact**
-   **Probability**

### Understanding Impact

The impact is assigned a value between 1 and 4, indicating the severity:

-   **1: Marginal**
-   **2: Moderate**
-   **3: Critical**
-   **4: Catastrophic**

### Assessing Probability

Probability is rated on a scale from 1 to 5, representing the likelihood:

-   **1: Improbable**
-   **2: Remote**
-   **3: Occasional**
-   **4: Probable**
-   **5: Frequent**

## Formula for Risk Calculation

The risk level is determined by multiplying the impact and probability:

```
Risk = Impact * Probability
```

By systematically considering both impact and probability, we can effectively gauge and address the
potential risks associated with the proposed solution.

## Identified Risks

| **Situation**                                                                        | **Impact** | **Probability** | **Risk** |
| ------------------------------------------------------------------------------------ | ---------- | --------------- | -------- |
| Changes in organizational policies affecting user access rights                      | 2          | 1               | 2        |
| DEI VM experiences unexpected downtime during the deployment process                 | 3          | 1               | 3        |
| Power outage affecting the availability of the system and data access                | 3          | 1               | 3        |
| Data breach compromising sensitive information                                       | 4          | 1               | 4        |
| Physical damage to the server room or data storage location due to natural disasters | 4          | 1               | 4        |
| Delayed server updates, exposing the system to potential security vulnerabilities    | 3          | 2               | 6        |
| Inadequate backup procedures leading to data loss in the event of a system failure   | 4          | 2               | 8        |
| Inadequate documentation on how to set up and configure VPN access                   | 3          | 2               | 6        |
| Backup strategy doesn't consider critical components, leading to data loss           | 3          | 2               | 6        |
| Insufficient documentation of the backup and recovery procedures                     | 3          | 2               | 6        |
| Security vulnerability discovered in a dependency, requiring urgent updates          | 4          | 2               | 8        |
| VPN service outage, preventing authorized access                                     | 4          | 2               | 8        |
| Failure in validating the module during scheduled tests, leading to unnoticed issues | 4          | 2               | 8        |
| Incorrect permissions on the public users folder, leading to unauthorized access     | 3          | 3               | 9        |
| Misconfiguration in network access controls, allowing external access                | 3          | 3               | 9        |
| Changes in the DEI network infrastructure affecting access controls                  | 3          | 3               | 9        |
| Automated deployment script encounters compatibility issues with the DEI VM          | 3          | 3               | 9        |
| Incorrect permissions on the public folder, leading to unauthorized access           | 3          | 3               | 9        |
| Inadequate backup frequency leading to a higher RPO (Recovery Point Objective)       | 4          | 3               | 12       |
