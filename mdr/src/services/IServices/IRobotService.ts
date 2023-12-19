import { Either } from '../../core/logic/Result'
import { ICreatedRobotDTO } from '../../dto/ICreatedRobotDTO'
import { IRobotDTO } from '../../dto/IRobotDTO'
import { IRobotInhibitDTO } from '../../dto/IRobotInhibitDTO'

export enum RobotErrorCode {
    NotFound,
    BussinessRuleViolation,
}

export type RobotErrorResult = {
    errorCode: RobotErrorCode
    message: string
}

export default interface IRobotService {
    createRobot(robotDTO: IRobotDTO): Promise<Either<RobotErrorResult, ICreatedRobotDTO>>
    inhibitRobot(
        robotDTO: IRobotInhibitDTO,
    ): Promise<Either<RobotErrorResult, ICreatedRobotDTO>>
    getRobots(): Promise<Either<RobotErrorResult, ICreatedRobotDTO[]>>
}
