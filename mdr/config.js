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
    },

    schemas: {
        user: {
            name: 'userSchema',
            schema: '../persistence/schemas/userSchema',
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
    },

    storage: {
        name: 'NodeDiskStorage',
        path: '../fs/nodeDiskStorage',
        prefix: './filesystem'
    }
}
