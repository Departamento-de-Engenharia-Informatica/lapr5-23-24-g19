import { Result } from '../../core/logic/Result'
import { ICreatedRobotDTO } from '../../dto/ICreatedRobotDTO'
import { IRobotDTO } from '../../dto/IRobotDTO'
import { IRobotInhibitDTO } from '../../dto/IRobotInhibitDTO'

export default interface IRobotService {
    createRobot(robotDTO: IRobotDTO): Promise<Result<ICreatedRobotDTO>>
    inhibitRobot(robotDTO: IRobotInhibitDTO): Promise<Result<ICreatedRobotDTO>>
    getRobots(): Promise<Result<ICreatedRobotDTO[]>>
}
