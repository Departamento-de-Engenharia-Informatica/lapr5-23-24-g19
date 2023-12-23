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
            path: '../controllers/floorController'
        },
        floorMap: {
            name: 'FloorMapController',
            path: '../controllers/floorMapController'
        },
        elevator: {
            name: 'ElevatorController',
            path: '../controllers/elevatorController'
        },
        passage: {
            name: 'PassageController',
            path: '../controllers/passageController'
        },
        room: {
            name: 'RoomController',
            path: '../controllers/roomController'
        },
        robotType: {
            name: 'RobotTypeController',
            path: '../controllers/robotTypeController'
        },
        robot: {
            name: 'RobotController',
            path: '../controllers/robotController'
        },
        task: {
            name: 'TaskController',
            path: '../controllers/taskController'
        },
        path: {
            name: 'PathController',
            path: '../controllers/pathController'
        }
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
            path: '../repos/mongo/floorRepo'
        },
        elevator: {
            name: 'ElevatorRepo',
            path: '../repos/mongo/elevatorRepo',
        },
        passage: {
            name: 'PassageRepo',
            path: '../repos/mongo/passageRepo'
        },
        room: {
            name: 'RoomRepo',
            path: '../repos/mongo/roomRepo'
        },
        robotType: {
            name: 'RobotTypeRepo',
            path: '../repos/mongo/robotTypeRepo'
        },
        robot: {
            name: 'RobotRepo',
            path: '../repos/mongo/robotRepo'
        },
        path: {
            name: 'HttpPlanningAdapter',
            path: '../repos/planning/httpNodePlanningAdapter'
        },
        mdt: {
            name: 'HttpMdtAdapter',
            path: '../repos/mdt/httpNodeMdtAdapter'
        }

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
            schema: '../persistence/schemas/floorSchema'
        },
        elevator: {
            name: 'elevatorSchema',
            schema: '../persistence/schemas/elevatorSchema'
        },
        passage: {
            name: 'passageSchema',
            schema: '../persistence/schemas/passageSchema'
        },
        room: {
            name: 'roomSchema',
            schema: '../persistence/schemas/roomSchema'
        },
        robotType: {
            name: 'robotType',
            schema: '../persistence/schemas/robotTypeSchema'
        },
        robot: {
            name: 'robotSchema',
            schema: '../persistence/schemas/robotSchema'
        },
    },

    services: {
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
            path: '../services/floorService'
        },
        floorMap: {
            name: 'FloorMapService',
            path: '../services/floorMapService'
        },
        elevator: {
            name: 'ElevatorService',
            path: '../services/elevatorService',
        },
        passage: {
            name: 'PassageService',
            path: '../services/passageService'
        },
        room: {
            name: 'RoomService',
            path: '../services/roomService'
        },
        robotType: {
            name: 'RobotTypeService',
            path: '../services/robotTypeService'
        },
        robot: {
            name: 'RobotService',
            path: '../services/robotService'
        },
        task: {
            name: 'TaskService',
            path: '../services/taskService'
        },
        path: {
            name: 'PathService',
            path: '../services/pathService'
        }
    },

    storage: {
        name: 'NodeDiskStorage',
        path: '../fs/nodeDiskStorage',
        prefix: './filesystem'
    },

    auth0: {
        audience: `https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/`,
        issuer: `https://dev-wt48psyid1ra2e8l.us.auth0.com/`,
        jwksUri: `https://dev-wt48psyid1ra2e8l.us.auth0.com/.well-known/jwks.json`,
        clientId: 'VWCGyPRVo5EZ2vlA4T657WddIn0nLVwl',
        clientSecret: 'BKSxLlyXyUdMaBJ7x5W4Xn7N6Cd30UhHBj6xp55f9GVyMOEzyCqfbPGlSIh1rVEf'
    }
}
