import { Schema, Document, model } from 'mongoose'
import { IRobotTypePersistence } from '../../dataschema/mongo/IRobotTypePersistence'

const RobotType = new Schema(
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
        },

        brand: {
            type: String,
            required: true,
        },

        model: {
            type: String,
            required: true,
        },

        taskType: {
            type: [String],
            required: true,
        },
    },
    { timestamps: true },
)

export default model<IRobotTypePersistence & Document>('RobotType', RobotType)
