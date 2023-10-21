import mongoose from 'mongoose'
import { IFloorPersistence } from '../../dataschema/IFloorPersistence'

const Floor = new mongoose.Schema(
    {
        domainId: {
            type: String,
            unique: true,
            required: true,
        },

        floorNumber: {
            type: String,
            required:true,
            index: true,
        },

        buildingCode: {
            type: String,
            required:true,
            index: true,
        },

        description: {
            type: String,
            required: false,
        },
    },
    { timestamps: true },
)

Floor.index({ floorNumber: 1, buildingCode: 1 }, { unique: true })

export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', Floor)
