import DataMapper from '../../core/infra/DataMapper'
import { Repo } from '../../core/infra/Repo'
import { Robot } from '../../domain/robot/Robot'

export interface RobotDataMap<Persistence>
    extends DataMapper<Robot, Persistence> {}

export default interface IRobotRepo extends Repo<Robot> {
    save(robot: Robot): Promise<Robot>

}

