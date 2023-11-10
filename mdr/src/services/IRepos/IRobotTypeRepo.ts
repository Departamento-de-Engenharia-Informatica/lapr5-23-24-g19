import { Repo } from '../../core/infra/Repo'
import RobotType from '../../domain/robotType/robotType'
import { RobotTypeCode } from '../../domain/robotType/robotTypeCode'

export default interface IRobotTypeRepo extends Repo<RobotType> {
    save(robotType: RobotType): Promise<RobotType>
    find(code: RobotTypeCode): Promise<RobotType>
    findAll(): Promise<RobotType[]>
}
