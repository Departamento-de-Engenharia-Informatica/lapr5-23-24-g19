import { Mapper } from '../core/infra/Mapper'
import { ICreatedRobotDTO } from '../dto/ICreatedRobotDTO'

import Robot from '../domain/robot/Robot'
import { RobotState } from '../domain/robot/state'

export class RobotMap extends Mapper<Robot> {
    static toDTO(robot: Robot): ICreatedRobotDTO {
        return {
            code: robot.code.value,
            nickname: robot.nickname.value,
            state: robot.state,
            serialNumber: robot.serialNumber.value,
            description: robot.description?.value,

            // change to robotType.toString() if implemented
            typeCode: robot.type.code.value,
        }
    }
}
