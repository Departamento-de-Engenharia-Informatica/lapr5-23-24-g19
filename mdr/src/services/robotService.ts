import config from '../../config'
import { Service, Inject } from 'typedi'
import { Either, left, right } from '../core/logic/Result'

import IRobotService, { RobotErrorCode, RobotErrorResult } from './IServices/IRobotService'
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

    async createRobot(dto: IRobotDTO): Promise<Either<RobotErrorResult, ICreatedRobotDTO>> {
        try {
            const type = await this.robotTypeRepo.find(RobotTypeCode.create(dto.typeCode).getValue())

            if (!type) {
                return left({
                    errorCode: RobotErrorCode.NotFound,
                    message: `Could not find robot type: ${type}`,
                })
            }

            const code = RobotCode.create(dto.code).getOrThrow()
            const nickname = Nickname.create(dto.nickname).getOrThrow()
            const serialNumber = SerialNumber.create(dto.serialNumber).getOrThrow()
            const description = dto.description && Description.create(dto.description).getOrThrow()

            const robot = Robot.create({
                code,
                nickname,
                type,
                serialNumber,
                description,
            }).getOrThrow()

            const saved = await this.robotRepo.save(robot)
            return right(RobotMap.toDTO(saved))
        } catch (e) {
            return left({
                errorCode: RobotErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }
    }

    async inhibitRobot(dto: IRobotInhibitDTO): Promise<Either<RobotErrorResult, ICreatedRobotDTO>> {
        const code = RobotCode.create(dto.code)
        if (code.isFailure) {
            return left({
                errorCode: RobotErrorCode.BussinessRuleViolation,
                message: code.errorValue().toString(),
            })
        }

        const robot = await this.robotRepo.find(code.getValue())

        if (!robot) {
            return left({
                errorCode: RobotErrorCode.NotFound,
                message: `Could not find robot type: ${dto.code}`,
            })
        }

        robot.inhibit()
        const saved = await this.robotRepo.save(robot)
        return right(RobotMap.toDTO(saved))
    }

    async getRobots(): Promise<Either<RobotErrorResult, ICreatedRobotDTO[]>> {
        try {
            const robots = await this.robotRepo.findAll()

            if (robots.length === 0) {
                return left({
                    errorCode: RobotErrorCode.NotFound,
                    message: 'Robots not found',
                })
            } else {
                return right(robots.map((robot) => RobotMap.toDTO(robot)))
            }
        } catch (e) {
            return left({
                errorCode: RobotErrorCode.BussinessRuleViolation,
                message: e.message ?? e.toString(),
            })
        }

        // TODO: change to this?
        // try {
        //     const robots = await this.robotRepo.findAll()
        //     return right(robots.map(robot => RobotMap.toDTO(robot)))
        // } catch (e) {
        //     throw e
        // }
    }
}
