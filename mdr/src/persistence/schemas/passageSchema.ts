import mongoose from 'mongoose'
import { IPassagePersistence } from '../../dataschema/mongo/IPassagePersistence'

const Passage = new mongoose.Schema(
    {
        domainID: {
            type: String,
            unique: true,
        },

        floor1ID: {
            type: String,
            required: true,
            index: true,
        },

        floor2ID: {
            type: String,
            required: true,
            index: true,
        },
    },
    { timestamps: true },
)

Passage.index({ floor1ID: 1, floor2ID: 1 }, { unique: true })

export default mongoose.model<IPassagePersistence & mongoose.Document>('Passage', Passage)
