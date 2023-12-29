import { IRolePersistence } from '../../dataschema/mongo/IRolePersistence'
import mongoose from 'mongoose'

const RoleSchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        name: { type: String, unique: true, required: true, index: true },
        active: { type: Boolean, required: true, index: true },
    },
    {
        timestamps: true,
    },
)

export default mongoose.model<IRolePersistence & mongoose.Document>('Role', RoleSchema)
