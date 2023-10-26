import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result } from '../core/logic/Result'

import IRobotService from './IServices/IRobotService'
import { RobotMap } from '../mappers/RobotMap'
import { IRobotDTO } from '../dto/IRobotDTO'

import IRobotTypeRepo from './IRepos/IRobotTypeRepo'
import IRobotRepo from './IRepos/IRobotRepo'

import { RobotTypeCode } from '../domain/robotType/robotTypeCode'

import Robot from '../domain/robot/Robot'
import { RobotCode } from '../domain/robot/code'
import { RobotNickname as Nickname } from '../domain/robot/nickname'
import { RobotSerialNumber as SerialNumber } from '../domain/robot/serialNumber'
import { RobotDescription as Description } from '../domain/robot/description'
import { RobotState } from '../domain/robot/state'
import { ICreatedRobotDTO } from '../dto/ICreatedRobotDTO'

@Service()
export default class RobotService implements IRobotService {
    constructor(
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo
    ) {}

    public async createRobot(dto: IRobotDTO): Promise<Result<ICreatedRobotDTO>> {
        try {
            const type = await this.robotTypeRepo.find(RobotTypeCode.create(dto.typeCode).getValue())

            if (!type) {
                return Result.fail(404)
            }

            const code = RobotCode.create(dto.code).getValue()
            const nickname = Nickname.create(dto.nickname).getValue()
            const serialNumber = SerialNumber.create(dto.serialNumber).getValue()
            const description = dto.description && Description.create(dto.description).getValue()

            const state = RobotState.create()

            const robot = Robot.create({
                code,
                nickname,
                type,
                state,
                serialNumber,
                description,
            })

            if (robot.isFailure) {
                return Result.fail(422)
            }

            const saved = await this.robotRepo.save(robot.getValue())
            return Result.ok(RobotMap.toDTO(saved))
        } catch (e) {
            return Result.fail(422)
        }
    }
}
