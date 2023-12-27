import { Document, model, Schema } from 'mongoose'
import { IBackofficeUserPersistence } from '../../dataschema/mongo/IBackofficeUserPersistence'

const BackofficeUser = new Schema({
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

    role: {
        type: String,
        required: true,
        index: true,
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

    password: String,
    //salt: String,
})

export default model<IBackofficeUserPersistence & Document>(
    'BackofficeUser',
    BackofficeUser,
)
