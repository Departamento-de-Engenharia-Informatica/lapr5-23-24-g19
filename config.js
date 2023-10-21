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
        /* FIXME: MARCOOOOOOOOOOOOOOOOOO */
        // floor: {
        //     name: 'FloorController',
        //     path: '../controllers/floorController'
        // },
    },

    repos: {
        role: {
            name: 'RoleRepo',
            path: '../repos/roleRepo',
        },
        user: {
            name: 'UserRepo',
            path: '../repos/userRepo',
        },
        building: {
            name: 'BuildingRepo',
            path: '../repos/buildingRepo',
        },
        /* FIXME: MARCOOOOOOOOOOOOOOOOOO */
        // floor: {
        //     name: 'FloorRepo',
        //     path: '../repos/floorRepo'
        // },
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
        /* FIXME: MARCOOOOOOOOOOOOOOOOOO */
        // floor: {
        //     name: 'FloorSchema',
        //     path: '../persistence/schemas/floorSchema'
        // },
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
        /* FIXME: MARCOOOOOOOOOOOOOOOOOO */
        // floor: {
        //     name: 'FloorService',
        //     path: '../services/floorService'
        // },
    },
}
