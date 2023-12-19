import { Either } from '../../core/logic/Result'
import { IRobotTypeDTO } from '../../dto/IRobotTypeDTO'

export enum RobotTypeErrorCode {
    NotFound,
    BussinessRuleViolation,
}

export type RobotTypeErrorResult = {
    errorCode: RobotTypeErrorCode
    message: string
}

export default interface IRobotTypeService {
    createRobotType(
        robotTypeDTO: IRobotTypeDTO,
    ): Promise<Either<RobotTypeErrorResult, IRobotTypeDTO>>
    getRobotTypes(): Promise<Either<RobotTypeErrorResult, IRobotTypeDTO[]>>
}
