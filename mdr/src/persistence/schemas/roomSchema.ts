import { Schema, Document, model } from 'mongoose'
import IElevatorPersistence from '../../dataschema/mongo/IElevatorPersistence'
import { IRoomPersistence } from '../../dataschema/mongo/IRoomPersistence'

const Room = new Schema(
    {
        domainId: {
            type: String,
            required: true,
            unique: true,
        },

        name: {
            type: String,
            required: true,
            unique: true,
        },

        floorNumber: {
            type: Number,
            required: true,
            index: true,
        },

        buildingCode: {
            type: String,
            required: true,
            index: true,
        },

        category: {
            type: String,
            required: true,
        },

        description: {
            type: String,
            required: true,
        },

        dimensions: {
            type: {
                length: Number,
                width: Number,
            },
            required: true,
        },

        position: {
            type: {
                x: Number,
                y: Number,
            },
            required: true,
        },
    },
    { timestamps: true },
)

export default model<IRoomPersistence & Document>('Room', Room)
