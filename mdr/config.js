import dotenv from 'dotenv'

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development'

const envFound = dotenv.config()
if (!envFound) {
    // This error should crash whole process

    throw new Error("⚠️  Couldn't find .env file  ⚠️")
}

export default {
    /**
     * Your favorite port : optional change to 4000 by JRT
     */
    port: parseInt(process.env.PORT, 10) || 4000,

    /**
     * That long string from mlab
     */
    databaseURL: process.env.MONGODB_URI || 'mongodb://127.0.0.1:27017/test',
    // databaseURL: process.env.MONGODB_URI || 'mongodb+srv://root:iJwu80jbwERuTa4t@maincluster.ocd2dr6.mongodb.net/?retryWrites=true&w=majority',

    /**
     */
    planningURL: process.env.PLANNING_URI || 'http://localhost:8090/api',

    /**
     */
    mdtURL: process.env.MDT_URI || 'http://localhost:5000/api',

    /**
     * Your secret sauce
     */
    jwtSecret: process.env.JWT_SECRET || 'my sakdfho2390asjod$%jl)!sdjas0i secret',

    specialAccessTok: 'BACKEND',

    /**
     * Used by winston logger
     */
    logs: {
        level: process.env.LOG_LEVEL || 'info',
    },

    validEmailDomains: ['isep.ipp.pt'],
    phoneNumberLength: 9,
    vatNumberLength: 9,

    systemRoles: ['ADMINISTRATOR', 'FLEET_MANAGER', 'CAMPUS_MANAGER', 'TASK_MANAGER'],

    /**
     * API configs
     */
    api: {
        prefix: '/api',
    },

    controllers: {
        role: {
            name: 'RoleController',
            path: '../controllers/roleController',
        },
        client: {
            name: 'ClientController',
            path: '../controllers/clientController',
        },
        backofficeUser: {
            name: 'BackofficeUserController',
            path: '../controllers/backofficeUserController',
        },
        building: {
            name: 'BuildingController',
            path: '../controllers/buildingController',
        },
        floor: {
            name: 'FloorController',
            path: '../controllers/floorController',
        },
        floorMap: {
            name: 'FloorMapController',
            path: '../controllers/floorMapController',
        },
        elevator: {
            name: 'ElevatorController',
            path: '../controllers/elevatorController',
        },
        passage: {
            name: 'PassageController',
            path: '../controllers/passageController',
        },
        room: {
            name: 'RoomController',
            path: '../controllers/roomController',
        },
        robotType: {
            name: 'RobotTypeController',
            path: '../controllers/robotTypeController',
        },
        robot: {
            name: 'RobotController',
            path: '../controllers/robotController',
        },
        task: {
            name: 'TaskController',
            path: '../controllers/taskController',
        },
        path: {
            name: 'PathController',
            path: '../controllers/pathController',
        },
    },

    repos: {
        role: {
            name: 'RoleRepo',
            path: '../repos/mongo/roleRepo',
        },
        user: {
            name: 'UserRepo',
            path: '../repos/mongo/userRepo',
        },
        client: {
            name: 'ClientRepo',
            path: '../repos/mongo/clientRepo',
        },
        backofficeUser: {
            name: 'BackofficeUserRepo',
            path: '../repos/mongo/backofficeUserRepo',
        },
        building: {
            name: 'BuildingRepo',
            path: '../repos/mongo/buildingRepo',
        },
        floor: {
            name: 'FloorRepo',
            path: '../repos/mongo/floorRepo',
        },
        elevator: {
            name: 'ElevatorRepo',
            path: '../repos/mongo/elevatorRepo',
        },
        passage: {
            name: 'PassageRepo',
            path: '../repos/mongo/passageRepo',
        },
        room: {
            name: 'RoomRepo',
            path: '../repos/mongo/roomRepo',
        },
        robotType: {
            name: 'RobotTypeRepo',
            path: '../repos/mongo/robotTypeRepo',
        },
        robot: {
            name: 'RobotRepo',
            path: '../repos/mongo/robotRepo',
        },
        path: {
            name: 'HttpPlanningAdapter',
            path: '../repos/planning/httpNodePlanningAdapter',
        },
        mdt: {
            name: 'HttpMdtAdapter',
            path: '../repos/mdt/httpNodeMdtAdapter',
        },
        auth: {
            name: 'AuthRepo',
            path: '../repos/auth0/authRepo',
        },
    },

    schemas: {
        user: {
            name: 'userSchema',
            schema: '../persistence/schemas/userSchema',
        },
        client: {
            name: 'clientSchema',
            schema: '../persistence/schemas/clientSchema',
        },
        backofficeUser: {
            name: 'backofficeUserSchema',
            schema: '../persistence/schemas/backofficeUserSchema',
        },
        role: {
            name: 'roleSchema',
            schema: '../persistence/schemas/roleSchema',
        },
        building: {
            name: 'buildingSchema',
            schema: '../persistence/schemas/buildingSchema',
        },
        floor: {
            name: 'floorSchema',
            schema: '../persistence/schemas/floorSchema',
        },
        elevator: {
            name: 'elevatorSchema',
            schema: '../persistence/schemas/elevatorSchema',
        },
        passage: {
            name: 'passageSchema',
            schema: '../persistence/schemas/passageSchema',
        },
        room: {
            name: 'roomSchema',
            schema: '../persistence/schemas/roomSchema',
        },
        robotType: {
            name: 'robotType',
            schema: '../persistence/schemas/robotTypeSchema',
        },
        robot: {
            name: 'robotSchema',
            schema: '../persistence/schemas/robotSchema',
        },
    },

    services: {
        archive: {
            name: 'ZipArchiveService',
            path: '../services/zipArchiveService',
        },
        role: {
            name: 'RoleService',
            path: '../services/roleService',
        },
        client: {
            name: 'ClientService',
            path: '../services/clientService',
        },
        backofficeUser: {
            name: 'BackofficeUserService',
            path: '../services/backofficeUserService',
        },
        building: {
            name: 'BuildingService',
            path: '../services/buildingService',
        },
        floor: {
            name: 'FloorService',
            path: '../services/floorService',
        },
        floorMap: {
            name: 'FloorMapService',
            path: '../services/floorMapService',
        },
        elevator: {
            name: 'ElevatorService',
            path: '../services/elevatorService',
        },
        passage: {
            name: 'PassageService',
            path: '../services/passageService',
        },
        room: {
            name: 'RoomService',
            path: '../services/roomService',
        },
        robotType: {
            name: 'RobotTypeService',
            path: '../services/robotTypeService',
        },
        robot: {
            name: 'RobotService',
            path: '../services/robotService',
        },
        task: {
            name: 'TaskService',
            path: '../services/taskService',
        },
        path: {
            name: 'PathService',
            path: '../services/pathService',
        },
    },

    strategies: {
        taskDistribution: {
            name: 'RoundRobinDistribution',
            path: '../core/logic/taskDistribution/roundRobinDistribution',
        }
    },

    storage: {
        name: 'NodeDiskStorage',
        path: '../fs/nodeDiskStorage',
        prefix: './filesystem',
    },

    auth0: {
        audience: `https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2`,
        issuer: `https://dev-wt48psyid1ra2e8l.us.auth0.com/`,
        jwksUri: `https://dev-wt48psyid1ra2e8l.us.auth0.com/.well-known/jwks.json`,
        clientId: 'VWCGyPRVo5EZ2vlA4T657WddIn0nLVwl',
        clientSecret: 'BKSxLlyXyUdMaBJ7x5W4Xn7N6Cd30UhHBj6xp55f9GVyMOEzyCqfbPGlSIh1rVEf',
        bearer:
            'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImhXQzAySWstalR1S3I5OVVhT1pycCJ9.eyJpc3MiOiJodHRwczovL2Rldi13dDQ4cHN5aWQxcmEyZThsLnVzLmF1dGgwLmNvbS8iLCJzdWIiOiJxcnJ5eHRrTWNlMm1LaWhMcmJ2eE1tZVA2NVpTQzBnekBjbGllbnRzIiwiYXVkIjoiaHR0cHM6Ly9kZXYtd3Q0OHBzeWlkMXJhMmU4bC51cy5hdXRoMC5jb20vYXBpL3YyLyIsImlhdCI6MTcwMzYxMzAwMiwiZXhwIjoxNzA2MjA1MDAyLCJhenAiOiJxcnJ5eHRrTWNlMm1LaWhMcmJ2eE1tZVA2NVpTQzBneiIsInNjb3BlIjoicmVhZDpjbGllbnRfZ3JhbnRzIGNyZWF0ZTpjbGllbnRfZ3JhbnRzIGRlbGV0ZTpjbGllbnRfZ3JhbnRzIHVwZGF0ZTpjbGllbnRfZ3JhbnRzIHJlYWQ6dXNlcnMgdXBkYXRlOnVzZXJzIGRlbGV0ZTp1c2VycyBjcmVhdGU6dXNlcnMgcmVhZDp1c2Vyc19hcHBfbWV0YWRhdGEgdXBkYXRlOnVzZXJzX2FwcF9tZXRhZGF0YSBkZWxldGU6dXNlcnNfYXBwX21ldGFkYXRhIGNyZWF0ZTp1c2Vyc19hcHBfbWV0YWRhdGEgcmVhZDp1c2VyX2N1c3RvbV9ibG9ja3MgY3JlYXRlOnVzZXJfY3VzdG9tX2Jsb2NrcyBkZWxldGU6dXNlcl9jdXN0b21fYmxvY2tzIGNyZWF0ZTp1c2VyX3RpY2tldHMgcmVhZDpjbGllbnRzIHVwZGF0ZTpjbGllbnRzIGRlbGV0ZTpjbGllbnRzIGNyZWF0ZTpjbGllbnRzIHJlYWQ6Y2xpZW50X2tleXMgdXBkYXRlOmNsaWVudF9rZXlzIGRlbGV0ZTpjbGllbnRfa2V5cyBjcmVhdGU6Y2xpZW50X2tleXMgcmVhZDpjb25uZWN0aW9ucyB1cGRhdGU6Y29ubmVjdGlvbnMgZGVsZXRlOmNvbm5lY3Rpb25zIGNyZWF0ZTpjb25uZWN0aW9ucyByZWFkOnJlc291cmNlX3NlcnZlcnMgdXBkYXRlOnJlc291cmNlX3NlcnZlcnMgZGVsZXRlOnJlc291cmNlX3NlcnZlcnMgY3JlYXRlOnJlc291cmNlX3NlcnZlcnMgcmVhZDpkZXZpY2VfY3JlZGVudGlhbHMgdXBkYXRlOmRldmljZV9jcmVkZW50aWFscyBkZWxldGU6ZGV2aWNlX2NyZWRlbnRpYWxzIGNyZWF0ZTpkZXZpY2VfY3JlZGVudGlhbHMgcmVhZDpydWxlcyB1cGRhdGU6cnVsZXMgZGVsZXRlOnJ1bGVzIGNyZWF0ZTpydWxlcyByZWFkOnJ1bGVzX2NvbmZpZ3MgdXBkYXRlOnJ1bGVzX2NvbmZpZ3MgZGVsZXRlOnJ1bGVzX2NvbmZpZ3MgcmVhZDpob29rcyB1cGRhdGU6aG9va3MgZGVsZXRlOmhvb2tzIGNyZWF0ZTpob29rcyByZWFkOmFjdGlvbnMgdXBkYXRlOmFjdGlvbnMgZGVsZXRlOmFjdGlvbnMgY3JlYXRlOmFjdGlvbnMgcmVhZDplbWFpbF9wcm92aWRlciB1cGRhdGU6ZW1haWxfcHJvdmlkZXIgZGVsZXRlOmVtYWlsX3Byb3ZpZGVyIGNyZWF0ZTplbWFpbF9wcm92aWRlciBibGFja2xpc3Q6dG9rZW5zIHJlYWQ6c3RhdHMgcmVhZDppbnNpZ2h0cyByZWFkOnRlbmFudF9zZXR0aW5ncyB1cGRhdGU6dGVuYW50X3NldHRpbmdzIHJlYWQ6bG9ncyByZWFkOmxvZ3NfdXNlcnMgcmVhZDpzaGllbGRzIGNyZWF0ZTpzaGllbGRzIHVwZGF0ZTpzaGllbGRzIGRlbGV0ZTpzaGllbGRzIHJlYWQ6YW5vbWFseV9ibG9ja3MgZGVsZXRlOmFub21hbHlfYmxvY2tzIHVwZGF0ZTp0cmlnZ2VycyByZWFkOnRyaWdnZXJzIHJlYWQ6Z3JhbnRzIGRlbGV0ZTpncmFudHMgcmVhZDpndWFyZGlhbl9mYWN0b3JzIHVwZGF0ZTpndWFyZGlhbl9mYWN0b3JzIHJlYWQ6Z3VhcmRpYW5fZW5yb2xsbWVudHMgZGVsZXRlOmd1YXJkaWFuX2Vucm9sbG1lbnRzIGNyZWF0ZTpndWFyZGlhbl9lbnJvbGxtZW50X3RpY2tldHMgcmVhZDp1c2VyX2lkcF90b2tlbnMgY3JlYXRlOnBhc3N3b3Jkc19jaGVja2luZ19qb2IgZGVsZXRlOnBhc3N3b3Jkc19jaGVja2luZ19qb2IgcmVhZDpjdXN0b21fZG9tYWlucyBkZWxldGU6Y3VzdG9tX2RvbWFpbnMgY3JlYXRlOmN1c3RvbV9kb21haW5zIHVwZGF0ZTpjdXN0b21fZG9tYWlucyByZWFkOmVtYWlsX3RlbXBsYXRlcyBjcmVhdGU6ZW1haWxfdGVtcGxhdGVzIHVwZGF0ZTplbWFpbF90ZW1wbGF0ZXMgcmVhZDptZmFfcG9saWNpZXMgdXBkYXRlOm1mYV9wb2xpY2llcyByZWFkOnJvbGVzIGNyZWF0ZTpyb2xlcyBkZWxldGU6cm9sZXMgdXBkYXRlOnJvbGVzIHJlYWQ6cHJvbXB0cyB1cGRhdGU6cHJvbXB0cyByZWFkOmJyYW5kaW5nIHVwZGF0ZTpicmFuZGluZyBkZWxldGU6YnJhbmRpbmcgcmVhZDpsb2dfc3RyZWFtcyBjcmVhdGU6bG9nX3N0cmVhbXMgZGVsZXRlOmxvZ19zdHJlYW1zIHVwZGF0ZTpsb2dfc3RyZWFtcyBjcmVhdGU6c2lnbmluZ19rZXlzIHJlYWQ6c2lnbmluZ19rZXlzIHVwZGF0ZTpzaWduaW5nX2tleXMgcmVhZDpsaW1pdHMgdXBkYXRlOmxpbWl0cyBjcmVhdGU6cm9sZV9tZW1iZXJzIHJlYWQ6cm9sZV9tZW1iZXJzIGRlbGV0ZTpyb2xlX21lbWJlcnMgcmVhZDplbnRpdGxlbWVudHMgcmVhZDphdHRhY2tfcHJvdGVjdGlvbiB1cGRhdGU6YXR0YWNrX3Byb3RlY3Rpb24gcmVhZDpvcmdhbml6YXRpb25zX3N1bW1hcnkgY3JlYXRlOmF1dGhlbnRpY2F0aW9uX21ldGhvZHMgcmVhZDphdXRoZW50aWNhdGlvbl9tZXRob2RzIHVwZGF0ZTphdXRoZW50aWNhdGlvbl9tZXRob2RzIGRlbGV0ZTphdXRoZW50aWNhdGlvbl9tZXRob2RzIHJlYWQ6b3JnYW5pemF0aW9ucyB1cGRhdGU6b3JnYW5pemF0aW9ucyBjcmVhdGU6b3JnYW5pemF0aW9ucyBkZWxldGU6b3JnYW5pemF0aW9ucyBjcmVhdGU6b3JnYW5pemF0aW9uX21lbWJlcnMgcmVhZDpvcmdhbml6YXRpb25fbWVtYmVycyBkZWxldGU6b3JnYW5pemF0aW9uX21lbWJlcnMgY3JlYXRlOm9yZ2FuaXphdGlvbl9jb25uZWN0aW9ucyByZWFkOm9yZ2FuaXphdGlvbl9jb25uZWN0aW9ucyB1cGRhdGU6b3JnYW5pemF0aW9uX2Nvbm5lY3Rpb25zIGRlbGV0ZTpvcmdhbml6YXRpb25fY29ubmVjdGlvbnMgY3JlYXRlOm9yZ2FuaXphdGlvbl9tZW1iZXJfcm9sZXMgcmVhZDpvcmdhbml6YXRpb25fbWVtYmVyX3JvbGVzIGRlbGV0ZTpvcmdhbml6YXRpb25fbWVtYmVyX3JvbGVzIGNyZWF0ZTpvcmdhbml6YXRpb25faW52aXRhdGlvbnMgcmVhZDpvcmdhbml6YXRpb25faW52aXRhdGlvbnMgZGVsZXRlOm9yZ2FuaXphdGlvbl9pbnZpdGF0aW9ucyBkZWxldGU6cGhvbmVfcHJvdmlkZXJzIGNyZWF0ZTpwaG9uZV9wcm92aWRlcnMgcmVhZDpwaG9uZV9wcm92aWRlcnMgdXBkYXRlOnBob25lX3Byb3ZpZGVycyBkZWxldGU6cGhvbmVfdGVtcGxhdGVzIGNyZWF0ZTpwaG9uZV90ZW1wbGF0ZXMgcmVhZDpwaG9uZV90ZW1wbGF0ZXMgdXBkYXRlOnBob25lX3RlbXBsYXRlcyBjcmVhdGU6ZW5jcnlwdGlvbl9rZXlzIHJlYWQ6ZW5jcnlwdGlvbl9rZXlzIHVwZGF0ZTplbmNyeXB0aW9uX2tleXMgZGVsZXRlOmVuY3J5cHRpb25fa2V5cyByZWFkOnNlc3Npb25zIGRlbGV0ZTpzZXNzaW9ucyByZWFkOnJlZnJlc2hfdG9rZW5zIGRlbGV0ZTpyZWZyZXNoX3Rva2VucyByZWFkOmNsaWVudF9jcmVkZW50aWFscyBjcmVhdGU6Y2xpZW50X2NyZWRlbnRpYWxzIHVwZGF0ZTpjbGllbnRfY3JlZGVudGlhbHMgZGVsZXRlOmNsaWVudF9jcmVkZW50aWFscyIsImd0eSI6ImNsaWVudC1jcmVkZW50aWFscyJ9.mUGNX3toCP4COl_Ue_xAWuKpCkrikPG2DbO9-9cZZZ8-Y-H0GbsAhNW66XAVCfG8LRYInc5AxuhdYQfN10o4pu2INoCCRQZQV4Pn9i0vwzmZRp-TvAOMbaNZ1iLJ3Wzky7aBUsC89mPGkCdci9XnMjVWnWmW-DFDbm9qZtUjcJTAH1V7ZX5uimQMpl8ADGfnsCwg_pfxNrG9e5VcSSdpWMs6lnNqX0Uf8Qj7CI4K7DDFIM1ofJKHr_4bDNmHMAOaN5oFgNHGK5gbbAyODc1OJc3EO8PZffB4ru5kOf2obfxR8hJwE0GcmzLOUsM22jPu444VWVmh0UGTrQRcl_-pUg',
    },
}
