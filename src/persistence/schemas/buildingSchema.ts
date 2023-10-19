import mongoose from 'mongoose'
import { IBuildingPersistence } from '../../dataschema/IBuildingPersistence'

const Building = new mongoose.Schema(
    {
        domainId: {
            type: String,
            required: true,
            unique: true,
        },

        code: {
            type: String,
            required: [true, 'Please enter a building code'],
            unique: true,
            index: true,
        },

        name: {
            type: String,
            required: false,
            index: true,
        },

        description: {
            type: String,
            required: false,
            index: true,
        },

        maxFloorLength: {
            type: Number,
            index: true,
        },

        maxFloorWidth: {
            type: Number,
            index: true,
        },
    },
    { timestamps: true },
)

export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', Building)
