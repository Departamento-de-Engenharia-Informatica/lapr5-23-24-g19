import mongoose from 'mongoose'
import { IBuildingPersistence } from '../../dataschema/mongo/IBuildingPersistence'

const Building = new mongoose.Schema(
    {
        domainId: {
            type: String,
            required: true,
            unique: true,
        },

        code: {
            type: String,
            required: true,
            unique: true,
            index: true,
        },

        name: {
            type: String,
            required: false,
        },

        description: {
            type: String,
            required: false,
        },

        maxFloorLength: Number,
        maxFloorWidth: Number,
    },
    { timestamps: true },
)

export default mongoose.model<IBuildingPersistence & mongoose.Document>(
    'Building',
    Building,
)
