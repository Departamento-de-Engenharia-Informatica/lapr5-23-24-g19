import mongoose from 'mongoose'
import { IFloorPersistence } from '../../dataschema/mongo/IFloorPersistence'

const floorPropsSchema = new mongoose.Schema({
    dimensions: {
        type: {
            length: Number,
            width: Number,
        },
        required: true,
    },

    mapContent: {
        type: [[Number]],
        required: true,
    },

    passages: [
        {
            x: Number,
            y: Number,
        },
    ],

    rooms: [
        {
            x: Number,
            y: Number,
        },
    ],

    elevators: [
        {
            x: Number,
            y: Number,
        },
    ],
})

const Floor = new mongoose.Schema(
    {
        domainId: {
            type: String,
            unique: true,
            required: true,
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

        description: {
            type: String,
            required: false,
        },

        path: {
            type: String,
            required: false,
        },
        // map: {
        //     type: floorPropsSchema,
        //     required:false,
        // },
    },

    { timestamps: true },
)

Floor.index({ floorNumber: 1, buildingCode: 1 }, { unique: true })

export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', Floor)
