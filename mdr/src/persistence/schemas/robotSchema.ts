import { Document, model, Schema } from 'mongoose'
import { IRobotPersistence } from '../../dataschema/mongo/IRobotPersistence'

const Robot = new Schema({
    domainId: {
        type: String,
        unique: true,
        required: true,
        index: true,
    },

    code: {
        type: String,
        unique: true,
        required: true,
        index: true,
    },

    nickname: {
        type: String,
        unique: true,
        required: true,
        index: true,
    },

    type: {
        type: String,
        required: true,
        // index: true
    },

    serialNumber: {
        type: String,
        required: true,
        // index: true
    },

    state: {
        type: Number,
        required: true,
        // index: true
    },

    description: {
        type: String,
        // required: true,
        // index: true
    },
})

Robot.index({ code: 1 }, { unique: true })
Robot.index({ nickname: 1 }, { unique: true })

// serialNumber must be unique per robot type
Robot.index({ serialNumber: 1, type: 1 }, { unique: true })

export default model<IRobotPersistence & Document>('Robot', Robot)
