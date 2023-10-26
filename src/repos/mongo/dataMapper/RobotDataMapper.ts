import Container from 'typedi'
import config from '../../../../config'
import { IRobotPersistence } from '../../../dataschema/mongo/IRobotPersistence'
import { RobotCode } from '../../../domain/robot/code'
import { RobotDescription } from '../../../domain/robot/description'
import { RobotNickname } from '../../../domain/robot/nickname'
import { Robot } from '../../../domain/robot/Robot'
import { RobotSerialNumber } from '../../../domain/robot/serialNumber'
import { RobotState } from '../../../domain/robot/state'
import { RobotTypeCode } from '../../../domain/robotType/robotTypeCode'
import { RobotDataMap } from '../../../services/IRepos/IRobotRepo'
import IRobotTypeRepo from '../../../services/IRepos/IRobotTypeRepo'

// scuffed impl because of schema not using ObjectId's
export default class MongoRobotDataMap
    implements RobotDataMap<IRobotPersistence> {
        private readonly robotTypeRepo: IRobotTypeRepo
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
            state: robot.state.value,
            description: robot.description?.value
        }
    }

    async toDomain(p: IRobotPersistence): Promise<Robot> {
        const code = RobotCode.create(p.code).getValue()
        const nickname = RobotNickname.create(p.nickname).getValue()
        const type = await this.robotTypeRepo.find(RobotTypeCode.create(p.type).getValue())
        const serialNumber = RobotSerialNumber.create(p.serialNumber).getValue()
        const state = RobotState.create().getValue()
        state.value = p.state
        const description = p.description && RobotDescription.create(p.description).getValue()

        const robot = Robot.create({
            code,
            nickname,
            type,
            serialNumber,
            state,
            description
        })

        if (robot.isFailure) {
            return null
        }

        return robot.getValue()
    }
}
