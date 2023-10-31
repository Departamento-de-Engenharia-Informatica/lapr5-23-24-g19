import { Schema, Document, model } from 'mongoose'
import IElevatorPersistence from '../../dataschema/mongo/IElevatorPersistence'

const Elevator = new Schema(
    {
        domainId: {
            type: String,
            required: true,
            unique: true,
        },

        building: {
            type: String,
            required: true,
            index: true,
        },

        identifier: {
            type: Number,
            required: true,
            index: true,
        },

        floors: [
            {
                type: Number,
                required: true,
            },
        ],

        brand: {
            type: String,
            required: false,
        },

        model: {
            type: String,
            required: false,
        },

        serialNumber: {
            type: String,
            required: false,
        },

        description: {
            type: String,
            required: false,
        },
    },
    { timestamps: true },
)

Elevator.index({ building: 1, identifier: 1 }, { unique: true })

export default model<IElevatorPersistence & Document>('Elevator', Elevator)
