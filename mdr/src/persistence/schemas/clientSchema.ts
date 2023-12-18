import { Document, model, Schema } from 'mongoose'
import { IRobotPersistence } from '../../dataschema/mongo/IRobotPersistence'

const Robot = new Schema({
    domainId: {
        type: String,
        unique: true,
        required: true,
        index: true,
    },

    name: {
        type: String,
        required: true,
    },

    email: {
        type: String,
        unique: true,
        required: true,
    },

    phoneNumber: {
        type: Number,
        unique: true,
        required: true,
    },

    vatNumber: {
        type: Number,
        unique: true,
        required: true,
    },

    password: {
        type: String,
        required: true,
    },


})



export default model<IClientPersistence & Document>('Client', Client)
