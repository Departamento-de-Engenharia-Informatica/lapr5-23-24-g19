import config from '../../config'
import { Service, Inject } from 'typedi'
import { Result } from '../core/logic/Result'

import IRobotService from './IServices/IRobotService'
import { RobotMap } from '../mappers/RobotMap'
import { IRobotDTO } from '../dto/IRobotDTO'

import IRobotRepo from './IRepos/IRobotRepo'
import IRobotTypeRepo from './IRepos/IRobotTypeRepo'

import { RobotTypeCode } from '../domain/robotType/robotTypeCode'

import Robot from '../domain/robot/Robot'
import { RobotCode } from '../domain/robot/code'
import { RobotNickname as Nickname } from '../domain/robot/nickname'
import { RobotSerialNumber as SerialNumber } from '../domain/robot/serialNumber'
import { RobotDescription as Description } from '../domain/robot/description'
import { ICreatedRobotDTO } from '../dto/ICreatedRobotDTO'
import { IRobotInhibitDTO } from '../dto/IRobotInhibitDTO'

@Service()
export default class RobotService implements IRobotService {
    constructor(
        @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
    ) {}

    async createRobot(dto: IRobotDTO): Promise<Result<ICreatedRobotDTO>> {
        try {
            const type = await this.robotTypeRepo.find(RobotTypeCode.create(dto.typeCode).getValue())

            if (!type) {
                return Result.fail(404)
            }

            const code = RobotCode.create(dto.code).getValue()
            const nickname = Nickname.create(dto.nickname).getValue()
            const serialNumber = SerialNumber.create(dto.serialNumber).getValue()
            const description = dto.description && Description.create(dto.description).getValue()

            const robot = Robot.create({
                code,
                nickname,
                type,
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

    async inhibitRobot(dto: IRobotInhibitDTO): Promise<Result<ICreatedRobotDTO>> {
        const robot = await this.robotRepo.find(RobotCode.create(dto.code).getValue())

        if (!robot) {
            return Result.fail('No such robot found')
        }

        robot.inhibit()
        const saved = await this.robotRepo.save(robot)
        return Result.ok(RobotMap.toDTO(saved))
    }

    async getRobots(): Promise<Result<ICreatedRobotDTO[]>> {
        try {
            const robots = await this.robotRepo.findAll()

            if (robots.length === 0) {
                return Result.fail('robots not found')
            } else {
                const dtoList = await Promise.all(robots.map(robot => RobotMap.toDTO(robot)))
                return Result.ok(dtoList)
            }
        } catch (e) {
            throw e
        }
    }
}
