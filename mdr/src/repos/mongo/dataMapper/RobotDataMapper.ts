import config from '../../../../config'
import Container from 'typedi'

import { RobotDataMap } from '../../../services/IRepos/IRobotRepo'
import { IRobotPersistence } from '../../../dataschema/mongo/IRobotPersistence'
import IRobotTypeRepo from '../../../services/IRepos/IRobotTypeRepo'

import Robot from '../../../domain/robot/Robot'
import { RobotCode as Code } from '../../../domain/robot/code'
import { RobotDescription as Description } from '../../../domain/robot/description'
import { RobotNickname as Nickname } from '../../../domain/robot/nickname'
import { RobotSerialNumber as SerialNumber } from '../../../domain/robot/serialNumber'
import { RobotTypeCode } from '../../../domain/robotType/robotTypeCode'
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID'

// scuffed impl because of schema not using ObjectId's
export default class MongoRobotDataMap implements RobotDataMap<IRobotPersistence> {
    private robotTypeRepo: IRobotTypeRepo
    constructor() {
        this.robotTypeRepo = Container.get(config.repos.robotType.name)
    }

    async toPersistence(robot: Robot): Promise<IRobotPersistence> {
        return {
            domainId: robot.id.toString(),
            code: robot.code.value,
            nickname: robot.nickname.value,
            type: robot.type.code.value,
            serialNumber: robot.serialNumber.value,
            state: robot.state,
            description: robot.description?.value,
        }
    }

    async toDomain(p: IRobotPersistence): Promise<Robot> {
        const code = Code.create(p.code).getValue()
        const nickname = Nickname.create(p.nickname).getValue()
        const type = await this.robotTypeRepo.find(
            RobotTypeCode.create(p.type).getValue(),
        )
        const serialNumber = SerialNumber.create(p.serialNumber).getValue()
        const description = p.description && Description.create(p.description).getValue()

        const result = Robot.create(
            {
                code,
                nickname,
                type,
                serialNumber,
                description,
            },
            new UniqueEntityID(p.domainId),
        )

        if (result.isFailure) {
            return null
        }

        const robot = result.getValue()
        robot.props.state = p.state

        return robot
    }
}
