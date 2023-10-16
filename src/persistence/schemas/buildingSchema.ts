import mongoose from 'mongoose'
import { IBuildingPersistence } from '../../dataschema/IBuildingPersistence'

const Building = new mongoose.Schema(
    {
        domainId: {
            type: String,
            unique: true,
        },

        code: {
            type: String,
            required: [true, 'Please enter a building code'],
            index: true,
        },

        name: {
            type: String,
            required: [true, 'Please enter a building name'],
            index: true,
        },

        description: {
            type: String,
            required: [true, 'Please enter a building description'],
            index: true,
        },
    },
    { timestamps: true },
)

export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', Building)
