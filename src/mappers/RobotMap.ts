import { Mapper } from "../core/infra/Mapper";
import Robot from "../domain/robot/Robot";
import { ICreatedRobotDTO } from "../dto/ICreatedRobotDTO";

export class RobotMap extends Mapper<Robot> {

    static toDTO(robot: Robot): ICreatedRobotDTO {
        return {
            code: robot.code.value,
            nickname: robot.nickname.value,
            state: robot.state.toString(),
            serialNumber: robot.serialNumber.value,
            description: robot.description?.value,

            // change to robotType.toString() if implemented
            typeCode: robot.type.code.value,
        }
    }
}
