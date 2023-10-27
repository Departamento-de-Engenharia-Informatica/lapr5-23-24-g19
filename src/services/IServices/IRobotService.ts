import { Result } from '../../core/logic/Result'
import { ICreatedRobotDTO } from '../../dto/ICreatedRobotDTO'
import { IRobotDTO } from '../../dto/IRobotDTO'

export default interface IRobotService {
    createRobot(robotTypeDTO: IRobotDTO): Promise<Result<ICreatedRobotDTO>>
    getRobots(): Promise<Result<ICreatedRobotDTO[]>>
}
