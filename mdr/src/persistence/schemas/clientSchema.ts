import { Document, model, Schema } from 'mongoose'
import { IClientPersistence } from '../../dataschema/mongo/IClientPersistence'

const Client = new Schema({
    domainId: {
        type: String,
        unique: true,
        required: true,
        index: true,
    },

    email: {
        type: String,
        index: true,
        unique: true,
        required: true,
    },

    name: {
        type: String,
        required: true,
    },

    phoneNumber: {
        type: String,
        unique: true,
        required: true,
    },

    vatNumber: {
        type: Number,
        unique: true,
        required: true,
    },

    status: {
        type: String,
        required: true,
    },

    password: String,
    salt: String,
})

export default model<IClientPersistence & Document>('Client', Client)
