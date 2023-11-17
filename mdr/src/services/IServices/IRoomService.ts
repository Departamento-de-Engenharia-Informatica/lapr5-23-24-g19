import { Either, Result } from '../../core/logic/Result'
import { IRoomDTO } from '../../dto/IRoomDTO'

export enum ErrorCode {
    NotFound,
    BussinessRuleViolation,

    AlreadyExists,
}

export type ErrorResult = {
    errorCode: ErrorCode
    message: string
}
export default interface IRoomService {
    createRoom(roomDTO: IRoomDTO): Promise<Either<ErrorResult, IRoomDTO>>
    getRooms(roomDTO: IRoomDTO): Promise<Either<ErrorResult, IRoomDTO[]>>
}
